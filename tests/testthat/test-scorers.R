# Goals and man of the match only exist for matches the sync wrote, and
# attendance goes back further than that. These check the rate is computed over
# the covered matches alone, rather than over an archive the events cannot
# possibly cover.

covered_match <- function(date, id = "1") {
  tibble(
    date = as.Date(date), opponent = "Shamrock Posers",
    goals_for = 2L, goals_against = 1L, dl_match_id = id, season_id = "s1"
  )
}

archive_match <- function(date) {
  tibble(
    date = as.Date(date), opponent = NA_character_,
    goals_for = 3L, goals_against = 0L, dl_match_id = NA_character_,
    season_id = "s1"
  )
}

goal <- function(date, player, team = "us") {
  tibble(
    date = as.Date(date), dl_match_id = "1", team = team,
    minute = 10L, event_type = "goal", player = player, season_id = "s1"
  )
}

mom <- function(date, player, team = "us") {
  tibble(
    date = as.Date(date), dl_match_id = "1", team = team,
    minute = NA_integer_, event_type = "mom", player = player, season_id = "s1"
  )
}

played <- function(date, players) {
  tibble(date = as.Date(date), player = players, season_id = "s1")
}

test_that("a match is covered when the sync stamped its fixture id on it", {
  matches <- bind_rows(archive_match("2026-01-07"), covered_match("2026-01-14"))

  coverage <- event_coverage(matches)

  expect_equal(coverage$observed, 1)
  expect_equal(coverage$total, 2)
  expect_equal(coverage$dates, as.Date("2026-01-14"))
})

test_that("a synced goalless draw is covered even with nothing logged in it", {
  # The reason coverage keys on the fixture id rather than on having events:
  # this match is completely recorded and has no events at all.
  goalless <- covered_match("2026-01-14") %>%
    mutate(goals_for = 0L, goals_against = 0L)

  expect_equal(event_coverage(goalless)$observed, 1)
})

test_that("the rate divides by appearances in covered matches, not every one", {
  matches <- bind_rows(archive_match("2026-01-07"), covered_match("2026-01-14"))
  attendance <- bind_rows(
    played("2026-01-07", c("Ben", "Vitto")),
    played("2026-01-14", c("Ben", "Vitto"))
  )
  events <- goal("2026-01-14", "Ben")

  table <- scorer_table(events, attendance, matches)

  ben <- table %>% filter(player == "Ben")
  # Two appearances in total, one of them in a match we have events for.
  expect_equal(ben$appearances, 1L)
  expect_equal(ben$goals_per_appearance, 1)
})

test_that("everyone who played is listed, including the players on nought", {
  matches <- covered_match("2026-01-14")
  attendance <- played("2026-01-14", c("Ben", "Vitto"))

  table <- scorer_table(goal("2026-01-14", "Ben"), attendance, matches)

  expect_setequal(table$player, c("Ben", "Vitto"))
  expect_equal(table$goals[table$player == "Vitto"], 0L)
})

test_that("the opposition's goals are not ours", {
  matches <- covered_match("2026-01-14")
  events <- bind_rows(
    goal("2026-01-14", "Ben"),
    goal("2026-01-14", "Some Winger", team = "them")
  )

  table <- scorer_table(events, played("2026-01-14", "Ben"), matches)

  expect_setequal(table$player, "Ben")
})

test_that("goals and man of the match are counted separately", {
  matches <- covered_match("2026-01-14")
  events <- bind_rows(
    goal("2026-01-14", "Ben"), goal("2026-01-14", "Ben"),
    mom("2026-01-14", "Vitto")
  )

  table <- scorer_table(events, played("2026-01-14", c("Ben", "Vitto")), matches)

  expect_equal(table$goals[table$player == "Ben"], 2L)
  expect_equal(table$mom[table$player == "Ben"], 0L)
  expect_equal(table$mom[table$player == "Vitto"], 1L)
})

test_that("a scorer with no attendance row is shown rather than dropped", {
  # Otherwise their goals leave the tally silently and the only sign is a total
  # that no longer adds up to the scorelines.
  matches <- covered_match("2026-01-14")
  events <- goal("2026-01-14", "Ghost")

  table <- scorer_table(events, played("2026-01-14", "Ben"), matches)

  ghost <- table %>% filter(player == "Ghost")
  expect_equal(ghost$goals, 1L)
  expect_equal(ghost$appearances, 0L)
  expect_true(is.na(ghost$goals_per_appearance))
})

test_that("a window with no covered match yields no rows rather than zeroes", {
  matches <- archive_match("2026-01-07")

  table <- scorer_table(empty_events(), played("2026-01-07", "Ben"), matches)

  expect_equal(nrow(table), 0)
  expect_named(
    table,
    c("player", "goals", "mom", "appearances", "goals_per_appearance")
  )
})
