# The player page is mostly a reshuffle of statistics that already exist. The
# part worth testing is the with/without comparison, which has to split the same
# matches two ways without losing or double-counting any of them.

pmatch <- function(date, gf, ga, id = NA_character_) {
  tibble(
    date = as.Date(date), opponent = "Ball FC", goals_for = gf,
    goals_against = ga, dl_match_id = id, season_id = "s1"
  )
}

turnout <- function(date, players) {
  tibble(date = as.Date(date), player = players, season_id = "s1")
}

test_that("played and missed partition the window exactly", {
  matches <- bind_rows(
    pmatch("2026-01-07", 3L, 0L), pmatch("2026-01-14", 0L, 1L),
    pmatch("2026-01-21", 1L, 1L)
  )
  attendance <- bind_rows(
    turnout("2026-01-07", "Ben"), turnout("2026-01-21", "Ben")
  )

  p <- player_profile("Ben", attendance, matches, empty_events())

  expect_equal(p$present$played + p$absent$played, nrow(matches))
  expect_equal(p$appearances, 2L)
  expect_equal(p$present$won, 1L)
  expect_equal(p$absent$lost, 1L)
})

test_that("the differential is the gap between the two records", {
  matches <- bind_rows(pmatch("2026-01-07", 3L, 0L), pmatch("2026-01-14", 0L, 1L))
  attendance <- turnout("2026-01-07", "Ben")

  p <- player_profile("Ben", attendance, matches, empty_events())

  # 3 points when playing, 0 when not.
  expect_equal(p$present$points_per_match, 3)
  expect_equal(p$absent$points_per_match, 0)
  expect_equal(p$differential, 3)
})

test_that("an ever-present has no comparison rather than a zero one", {
  # NaN would print as a number and read as "no difference", which is a claim.
  matches <- pmatch("2026-01-07", 3L, 0L)

  p <- player_profile("Ben", turnout("2026-01-07", "Ben"), matches, empty_events())

  expect_equal(p$absent$played, 0L)
  expect_true(is.na(p$absent$points_per_match))
  expect_true(is.na(p$differential))
})

test_that("somebody else's goals are not counted as theirs", {
  matches <- pmatch("2026-01-07", 2L, 0L, id = "1")
  events <- tibble(
    date = as.Date("2026-01-07"), dl_match_id = "1", team = "us",
    minute = c(10L, 20L), event_type = "goal", player = c("Ben", "Vitto"),
    season_id = "s1"
  )

  p <- player_profile("Ben", turnout("2026-01-07", c("Ben", "Vitto")), matches, events)

  expect_equal(p$goals, 1L)
  expect_equal(p$covered, 1L)
})

test_that("goals are counted over covered matches only", {
  # An event on an uncovered match should not be reachable, but if one exists
  # it must not be counted against a denominator that excludes its match.
  matches <- pmatch("2026-01-07", 2L, 0L)
  events <- tibble(
    date = as.Date("2026-01-07"), dl_match_id = NA_character_, team = "us",
    minute = 10L, event_type = "goal", player = "Ben", season_id = "s1"
  )

  p <- player_profile("Ben", turnout("2026-01-07", "Ben"), matches, events)

  expect_equal(p$covered, 0L)
  expect_equal(p$goals, 0L)
})

test_that("the timeline marks every match played or missed", {
  matches <- bind_rows(pmatch("2026-01-07", 3L, 0L), pmatch("2026-01-14", 0L, 1L))

  p <- player_profile("Ben", turnout("2026-01-07", "Ben"), matches, empty_events())

  expect_equal(nrow(p$timeline), 2)
  expect_equal(p$timeline$played, c(TRUE, FALSE))
})

test_that("the picker offers whoever turned out, not the roster", {
  # The roster table is the picker now, so this is the rule it has to keep:
  # anyone who played in the window is on it, whether or not they are still on
  # the fee roster, and a signing who has not played yet is not.
  matches <- with_result(tibble(
    date = as.Date(c("2026-01-07", "2026-01-14")),
    opponent = NA_character_, goals_for = 2L, goals_against = 1L,
    dl_match_id = NA_character_
  ))
  attendance <- bind_rows(
    turnout("2026-01-07", c("Vitto", "Ben")), turnout("2026-01-14", "Ben")
  )

  expect_setequal(create_attendance_list(attendance, matches)$player, c("Ben", "Vitto"))
})
