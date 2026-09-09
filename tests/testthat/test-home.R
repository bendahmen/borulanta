# The home page: what just happened, what is next, and the two snapshot files
# those answers are read out of.

matches_for <- function(dates, goals_for, goals_against, opponent = NA_character_) {
  tibble(
    date = as.Date(dates),
    opponent = opponent,
    goals_for = as.integer(goals_for),
    goals_against = as.integer(goals_against),
    dl_match_id = NA_character_
  ) %>%
    with_result()
}

# Last match ----

test_that("the last match is the latest one, whatever order the file is in", {
  matches <- matches_for(
    c("2026-08-05", "2026-08-19", "2026-08-12"), c(6, 5, 10), c(2, 3, 6)
  ) %>%
    mutate(season_id = "s2")
  attendance <- tibble(
    date = as.Date(c("2026-08-19", "2026-08-19", "2026-08-12")),
    player = c("Ben", "Vitto", "Ben")
  )

  summary <- last_match_summary(matches, attendance, empty_events())

  expect_equal(summary$match$date, as.Date("2026-08-19"))
  expect_equal(summary$match$result, "5-3")
  expect_equal(summary$match$points, 3)
  expect_equal(summary$squad_size, 2L)
  expect_setequal(summary$lineup$player, c("Ben", "Vitto"))
})

test_that("scorers are tallied per player and the opposition's are left out", {
  matches <- matches_for("2026-08-19", 3, 1) %>% mutate(season_id = "s2")
  events <- tibble(
    date = as.Date("2026-08-19"),
    dl_match_id = NA_character_,
    team = c("us", "us", "us", "them", "us"),
    minute = c(10L, 20L, 30L, 40L, NA),
    event_type = c("goal", "goal", "goal", "goal", "mom"),
    player = c("Ben", "Ben", "Vitto", "Someone Else", "Ben")
  )

  summary <- last_match_summary(matches, tibble(date = as.Date("2026-08-19"), player = "Ben"), events)

  # A hat-trick reads as one line with a tally, not three identical lines.
  expect_equal(summary$scorers$player, c("Ben", "Vitto"))
  expect_equal(summary$scorers$goals, c(2L, 1L))
  expect_false("Someone Else" %in% summary$scorers$player)
  expect_equal(summary$mom, "Ben")
})

test_that("a match with no events recorded still summarises", {
  # Everything played before the sync existed is in exactly this state, so it
  # is the common case rather than an edge one.
  matches <- matches_for("2026-08-19", 5, 3) %>% mutate(season_id = "s2")

  summary <- last_match_summary(
    matches, tibble(date = as.Date("2026-08-19"), player = "Ben"), empty_events()
  )

  expect_equal(nrow(summary$scorers), 0)
  expect_true(is.na(summary$mom))
  expect_equal(summary$squad_size, 1L)
})

test_that("no matches at all gives NULL rather than an error", {
  expect_null(last_match_summary(matches_for(character(), integer(), integer()),
                                 tibble(date = as.Date(character()), player = character()),
                                 empty_events()))
})

# Next fixture ----

fixtures_for <- function(dates, opponents) {
  tibble(date = as.Date(dates), opponent = opponents, dl_match_id = NA_character_)
}

test_that("the next fixture is the soonest one still to come", {
  fixtures <- fixtures_for(
    c("2026-09-09", "2026-09-16", "2026-09-30"),
    c("Shamrock Posers", "Brother Man FC", "Ball FC")
  )
  played <- matches_for("2026-09-09", 4, 2)

  # 09 Sep has been played, so the next one is the 16th even though the
  # fixture list still carries the 9th until the next sync replaces it.
  fixture <- next_fixture(fixtures, played, today = as.Date("2026-09-10"))
  expect_equal(fixture$date, as.Date("2026-09-16"))
  expect_equal(fixture$opponent, "Brother Man FC")
})

test_that("a fixture today is still to come", {
  # The sync draws the line the same way: the page is read during the day and
  # the game is in the evening.
  fixtures <- fixtures_for("2026-09-09", "Shamrock Posers")

  fixture <- next_fixture(fixtures, matches_for(character(), integer(), integer()),
                          today = as.Date("2026-09-09"))
  expect_equal(fixture$date, as.Date("2026-09-09"))
})

test_that("a stale fixture list does not offer a match already played", {
  # Between a match finishing and the next sync, the fixture file still lists
  # it. Without the result check the home page would call it the next match.
  fixtures <- fixtures_for("2026-09-09", "Shamrock Posers")
  played <- matches_for("2026-09-09", 4, 2)

  expect_null(next_fixture(fixtures, played, today = as.Date("2026-09-09")))
})

test_that("an empty or exhausted fixture list gives NULL", {
  expect_null(next_fixture(fixtures_for(character(), character()),
                           matches_for(character(), integer(), integer())))
  expect_null(next_fixture(fixtures_for("2026-09-09", "Shamrock Posers"),
                           matches_for(character(), integer(), integer()),
                           today = as.Date("2026-12-01")))
})

# Season record ----

test_that("the record counts wins, draws, losses and goals", {
  matches <- matches_for(
    c("2026-08-05", "2026-08-12", "2026-08-19"),
    c(6, 2, 1), c(2, 2, 3)
  )

  record <- season_record(matches)
  expect_equal(record$played, 3L)
  expect_equal(record$won, 1L)
  expect_equal(record$drawn, 1L)
  expect_equal(record$lost, 1L)
  expect_equal(record$goals_for, 9L)
  expect_equal(record$goals_against, 7L)
  expect_equal(record$goal_difference, 2L)
  expect_equal(record$points, 4)
})

test_that("a season with no matches is all zeroes, not no rows", {
  record <- season_record(matches_for(character(), integer(), integer()))

  expect_equal(nrow(record), 1L)
  expect_equal(record$played, 0L)
  expect_equal(record$points, 0)
})

# Snapshot files ----

test_that("the fixture snapshot keeps every fixture and drops the scores", {
  # Unplayed fixtures included: the app decides which is next, using the date
  # it is actually being read on rather than the date the sync ran.
  fixtures <- tibble(
    date = as.Date(c("2026-09-16", "2026-09-09")),
    dl_match_id = c("96099", "96094"),
    opponent = c("Brother Man FC", "Shamrock Posers"),
    goals_for = c(0L, 4L),
    goals_against = c(0L, 2L)
  )

  snapshot <- snapshot_tables(fixtures, empty_standings(), as.Date("2026-09-10"))

  expect_equal(names(snapshot$fixtures), c("date", "opponent", "dl_match_id"))
  expect_equal(snapshot$fixtures$date, as.Date(c("2026-09-09", "2026-09-16")))
  # matches.csv is the only source of truth for a result.
  expect_false(any(c("goals_for", "goals_against") %in% names(snapshot$fixtures)))
})

test_that("the standings snapshot is dated and ordered by position", {
  standings <- tibble(
    position = c(2L, 1L), team = c("Borulanta", "Ball FC"),
    played = 1L, won = c(0L, 1L), drawn = 0L, lost = c(1L, 0L),
    goals_for = c(2L, 5L), goals_against = c(5L, 2L),
    goal_difference = c(-3L, 3L), points = c(0L, 3L)
  )

  snapshot <- snapshot_tables(
    tibble(date = as.Date(character()), dl_match_id = character(),
           opponent = character(), goals_for = integer(), goals_against = integer()),
    standings, as.Date("2026-09-10")
  )

  expect_equal(snapshot$league_table$position, c(1L, 2L))
  expect_true(all(snapshot$league_table$scraped_on == as.Date("2026-09-10")))
})

test_that("both snapshot files round-trip through disk unchanged", {
  # The writers format dates as dd/mm/yyyy and the readers parse them back;
  # a mismatch there would be invisible until the home page showed a fixture
  # in the year 10.
  fixture_path <- withr::local_tempfile(fileext = ".csv")
  table_path <- withr::local_tempfile(fileext = ".csv")

  fixtures <- tibble(
    date = as.Date(c("2026-09-09", "2026-09-16")),
    dl_match_id = c("96094", "96099"),
    opponent = c("Shamrock Posers", "Brother Man FC"),
    goals_for = 0L, goals_against = 0L
  )
  standings <- tibble(
    position = 1L, team = "Borulanta", played = 1L, won = 1L, drawn = 0L,
    lost = 0L, goals_for = 4L, goals_against = 2L, goal_difference = 2L, points = 3L
  )

  snapshot <- snapshot_tables(fixtures, standings, as.Date("2026-09-10"))
  write_snapshot_files(snapshot, fixture_path, table_path)

  expect_equal(read_fixture_file(fixture_path), snapshot$fixtures)
  expect_equal(read_league_table_file(table_path), snapshot$league_table)
})

test_that("a missing snapshot file reads as empty rather than failing", {
  # True of a fresh clone, and of any checkout from before these files existed.
  # The app has to start either way.
  missing <- file.path(tempdir(), "definitely-not-here.csv")

  expect_equal(nrow(read_fixture_file(missing)), 0L)
  expect_equal(nrow(read_league_table_file(missing)), 0L)
  expect_s3_class(read_fixture_file(missing)$date, "Date")
  expect_null(next_fixture(read_fixture_file(missing),
                           matches_for(character(), integer(), integer())))
})
