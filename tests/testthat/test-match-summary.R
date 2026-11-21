# What a match page says about its own night: the score, who was there and what
# they did.

matches_for <- function(dates, goals_for, goals_against, opponent = NA_character_) {
  tibble(
    date = as.Date(dates),
    opponent = opponent,
    goals_for = as.integer(goals_for),
    goals_against = as.integer(goals_against),
    dl_match_id = NA_character_
  ) |>
    with_result()
}

test_that("the summary is of the latest match, whatever order the file is in", {
  matches <- matches_for(
    c("2026-08-05", "2026-08-19", "2026-08-12"), c(6, 5, 10), c(2, 3, 6)
  ) |>
    mutate(season_id = "s2")
  attendance <- tibble(
    date = as.Date(c("2026-08-19", "2026-08-19", "2026-08-12")),
    player = c("Ben", "Vitto", "Ben")
  )

  summary <- match_summary(matches, attendance, empty_events())

  expect_equal(summary$match$date, as.Date("2026-08-19"))
  expect_equal(summary$match$result, "5-3")
  expect_equal(summary$match$points, 3)
  expect_equal(summary$squad_size, 2L)
  expect_setequal(summary$lineup$player, c("Ben", "Vitto"))
})

test_that("scorers are tallied per player and the opposition's are left out", {
  matches <- matches_for("2026-08-19", 3, 1) |> mutate(season_id = "s2")
  events <- tibble(
    date = as.Date("2026-08-19"),
    dl_match_id = NA_character_,
    team = c("us", "us", "us", "them", "us"),
    minute = c(10L, 20L, 30L, 40L, NA),
    event_type = c("goal", "goal", "goal", "goal", "mom"),
    player = c("Ben", "Ben", "Vitto", "Someone Else", "Ben")
  )

  summary <- match_summary(matches, tibble(date = as.Date("2026-08-19"), player = "Ben"), events)

  # A hat-trick reads as one line with a tally, not three identical lines.
  expect_equal(summary$scorers$player, c("Ben", "Vitto"))
  expect_equal(summary$scorers$goals, c(2L, 1L))
  expect_false("Someone Else" %in% summary$scorers$player)
  expect_equal(summary$mom, "Ben")
})

test_that("a match with no events recorded still summarises", {
  # Everything played before the sync existed is in exactly this state, so it
  # is the common case rather than an edge one.
  matches <- matches_for("2026-08-19", 5, 3) |> mutate(season_id = "s2")

  summary <- match_summary(
    matches, tibble(date = as.Date("2026-08-19"), player = "Ben"), empty_events()
  )

  expect_equal(nrow(summary$scorers), 0)
  expect_true(is.na(summary$mom))
  expect_equal(summary$squad_size, 1L)
})

test_that("no matches at all gives NULL rather than an error", {
  expect_null(match_summary(matches_for(character(), integer(), integer()),
                            tibble(date = as.Date(character()), player = character()),
                            empty_events()))
})
