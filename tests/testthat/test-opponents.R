# The opponent arrived with the sync, so most of the archive has none. These
# check that a record is only claimed where there is one, and that the run-in
# survives a standings table that does not carry everybody.

result <- function(date, opponent, gf, ga) {
  tibble(
    date = as.Date(date), opponent = opponent, goals_for = gf,
    goals_against = ga, dl_match_id = "1", season_id = "s1"
  )
}

test_that("the record is per opponent, and points are per match", {
  matches <- bind_rows(
    result("2026-01-07", "Ball FC", 3L, 1L),
    result("2026-01-14", "Ball FC", 0L, 2L),
    result("2026-01-21", "Finessin FC", 2L, 2L)
  )

  record <- opponent_record(matches)

  ball <- record %>% filter(opponent == "Ball FC")
  expect_equal(ball$played, 2L)
  expect_equal(ball$won, 1L)
  expect_equal(ball$lost, 1L)
  expect_equal(ball$goal_difference, 0L)
  expect_equal(ball$points_per_match, 1.5)
})

test_that("meeting a side more often does not move it up the table", {
  # Two wins out of four beats one win out of one on totals, and should not.
  matches <- bind_rows(
    result("2026-01-07", "Ball FC", 1L, 0L),
    result("2026-01-14", "Ball FC", 1L, 0L),
    result("2026-01-21", "Ball FC", 0L, 1L),
    result("2026-01-28", "Ball FC", 0L, 1L),
    result("2026-02-04", "Finessin FC", 5L, 0L)
  )

  record <- opponent_record(matches)

  expect_equal(record$opponent[[1]], "Finessin FC")
})

test_that("matches from before the sync are dropped, not pooled", {
  # Each has a different unrecorded opponent, so one row averaging them would
  # describe nobody.
  matches <- bind_rows(
    result("2026-01-07", NA_character_, 4L, 5L),
    result("2026-01-14", NA_character_, 6L, 2L),
    result("2026-01-21", "Ball FC", 1L, 0L)
  )

  record <- opponent_record(matches)

  expect_equal(nrow(record), 1)
  expect_equal(record$opponent, "Ball FC")
})

test_that("a window with no opponent on file yields no rows, not an error", {
  matches <- result("2026-01-07", NA_character_, 4L, 5L)

  record <- opponent_record(matches)

  expect_equal(nrow(record), 0)
  expect_true("points_per_match" %in% names(record))
})

test_that("the run-in is what is left to play, with the standings attached", {
  fixtures <- tibble(
    date = as.Date(c("2026-01-07", "2026-01-14")),
    opponent = c("Ball FC", "Finessin FC"),
    dl_match_id = c("1", "2"), season_id = "s1"
  )
  standings <- tibble(
    position = 1:2, team = c("Ball FC", "Finessin FC"),
    played = c(4L, 4L), goal_difference = c(6L, -3L), points = c(10L, 4L)
  )

  left <- run_in(fixtures, result("2026-01-07", "Ball FC", 1L, 0L), standings,
                 today = as.Date("2026-01-08"))

  # The 7th is both played and in the past, so only the 14th is left.
  expect_equal(nrow(left), 1)
  expect_equal(left$opponent, "Finessin FC")
  expect_equal(left$points, 4L)
})

test_that("an opponent missing from the standings keeps its fixture", {
  # Dropping it would silently shorten the run-in.
  fixtures <- tibble(
    date = as.Date("2026-02-04"), opponent = "New Team",
    dl_match_id = "9", season_id = "s1"
  )
  standings <- tibble(
    position = 1L, team = "Ball FC", played = 4L,
    goal_difference = 6L, points = 10L
  )

  left <- run_in(fixtures, result("2026-01-07", "Ball FC", 1L, 0L), standings,
                 today = as.Date("2026-01-08"))

  expect_equal(left$opponent, "New Team")
  expect_true(is.na(left$points))
})

test_that("no standings at all still lists the fixtures", {
  fixtures <- tibble(
    date = as.Date("2026-02-04"), opponent = "Ball FC",
    dl_match_id = "9", season_id = "s1"
  )

  left <- run_in(fixtures, result("2026-01-07", NA_character_, 1L, 0L),
                 empty_standings(), today = as.Date("2026-01-08"))

  expect_equal(nrow(left), 1)
  expect_true(is.na(left$position))
})
