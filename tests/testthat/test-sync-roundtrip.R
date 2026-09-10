# Surviving the CSV.
#
# The tests in test-sync.R hand `sync_results()` tibbles built in memory, which
# is not what the weekly job does: it reads two CSVs, syncs, writes them, and a
# week later reads back what it wrote. A CSV does not remember what its columns
# were, so that round trip is its own failure surface — an empty file whose
# every column reads as character, an all-digit fixture id that comes back a
# number — and none of it shows up until the week a result first appears.

# Everything here goes through the real files in a temporary directory, with the
# real read and write functions the runner uses.
in_a_temp_project <- function(matches_csv, events_csv, work) {
  dir <- withr::local_tempdir()
  match_path <- file.path(dir, "matches.csv")
  event_path <- file.path(dir, "match_events.csv")
  writeLines(matches_csv, match_path)
  writeLines(events_csv, event_path)
  work(match_path, event_path)
}

BLANK_MATCHES <- "date,opponent,goals_for,goals_against,dl_match_id"
BLANK_EVENTS <- "date,dl_match_id,team,minute,event_type,player"

one_open_season <- function() {
  tibble(
    season_id = "s1", label = "Season 1", start_date = as.Date("2026-07-01"),
    fee_rules = "core_backstop", status = "open", end_date = as.Date("9999-12-31")
  )
}

a_played_fixture <- function(date = "2026-07-22", opponent = "Ball FC") {
  tibble(
    date = as.Date(date), dl_match_id = "96094", opponent = opponent,
    goals_for = 2L, goals_against = 1L,
    events = list(tibble(
      team = c("us", "them", "us"), minute = c(12L, 20L, 31L),
      event_type = "goal", player = c("Felix", "Andy", "Ferg")
    ))
  )
}

test_that("a first result can actually be written to the empty files", {
  # The empty match_events.csv reads back with a character `minute`, which the
  # scrape supplies as an integer. Combining them is the first thing the sync
  # does with a real result, and the first week it ever gets to try.
  in_a_temp_project(BLANK_MATCHES, BLANK_EVENTS, function(match_path, event_path) {
    result <- sync_results(
      a_played_fixture(), read_match_file(match_path), read_event_file(event_path),
      one_open_season(), tibble(site_name = character(), player = character()),
      today = as.Date("2026-08-01")
    )

    expect_null(result$abort)
    expect_true(result$changed)
    expect_no_error(write_sync_files(result, match_path, event_path))

    expect_equal(nrow(read_match_file(match_path)), 1)
    # Two of the three goals were ours; the opposition's is not recorded.
    expect_equal(nrow(read_event_file(event_path)), 2)
  })
})

test_that("the week after, the same result is read back and left alone", {
  # `96094` written out and read back in is a number unless somebody insists
  # otherwise, and a number will not combine with the scrape's string.
  in_a_temp_project(BLANK_MATCHES, BLANK_EVENTS, function(match_path, event_path) {
    sync <- function(fixtures, today) {
      sync_results(
        fixtures, read_match_file(match_path), read_event_file(event_path),
        one_open_season(), tibble(site_name = character(), player = character()),
        today = today
      )
    }

    write_sync_files(sync(a_played_fixture(), as.Date("2026-08-01")), match_path, event_path)

    # Week two: the same fixture is still on the page, plus a new one.
    week_two <- bind_rows(a_played_fixture(), a_played_fixture("2026-07-29", "Finessin FC"))
    second <- sync(week_two, as.Date("2026-08-08"))

    expect_null(second$abort)
    expect_equal(nrow(second$report$new), 1)
    expect_equal(nrow(second$report$unchanged), 1)
    expect_equal(nrow(second$report$enriched), 0)
    expect_no_error(write_sync_files(second, match_path, event_path))

    on_file <- read_match_file(match_path)
    expect_equal(nrow(on_file), 2)
    expect_equal(on_file$dl_match_id, c("96094", "96094"))
  })
})

test_that("a third run with nothing new reports no change at all", {
  # If this drifts the scheduled job commits an identical file every Friday.
  in_a_temp_project(BLANK_MATCHES, BLANK_EVENTS, function(match_path, event_path) {
    sync <- function() {
      sync_results(
        a_played_fixture(), read_match_file(match_path), read_event_file(event_path),
        one_open_season(), tibble(site_name = character(), player = character()),
        today = as.Date("2026-08-01")
      )
    }

    write_sync_files(sync(), match_path, event_path)
    before <- readLines(match_path)
    events_before <- readLines(event_path)

    again <- sync()
    expect_false(again$changed)

    write_sync_files(again, match_path, event_path)
    expect_equal(readLines(match_path), before)
    expect_equal(readLines(event_path), events_before)
  })
})

test_that("the real data files survive a sync of the real fixture list", {
  # The season rollover in data/seasons.csv is the one about to happen, and the
  # live page is a full list of unplayed fixtures — so the correct outcome is
  # that nothing changes at all.
  fixtures <- our_fixtures(parse_fixtures(fixture_html("shoreditch-unplayed.html")))
  matches <- read_match_file(here::here("data", "matches.csv"))
  events <- read_event_file(here::here("data", "match_events.csv"))

  result <- sync_results(
    fixtures, matches, events,
    load_seasons(here::here("data", "seasons.csv")),
    read_csv(here::here("data", "name_map.csv"), show_col_types = FALSE),
    today = as.Date("2026-09-08")
  )

  expect_null(result$abort)
  expect_false(result$changed)
  expect_equal(nrow(result$matches), nrow(matches))
  expect_equal(nrow(result$report$new), 0)
})
