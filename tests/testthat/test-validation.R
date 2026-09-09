# The warnings validate_app_data() raises go to the console or the deployment
# log, which is where they are not read. It returns them as well so the app can
# put them on the page, and these check that the returned vector is complete
# and that warning as before is unaffected.

clean_data <- function() {
  list(
    seasons = tibble(
      season_id = "s1", label = "Season 1",
      start_date = as.Date("2026-01-07"), fee_rules = "even_split", status = "open"
    ),
    players = tibble(player = c("Ben", "Vitto")),
    rosters = tibble(season_id = "s1", player = c("Ben", "Vitto"), core = TRUE, active = TRUE),
    matches = tibble(
      date = as.Date("2026-01-14"), opponent = "Shamrock Posers",
      goals_for = 2L, goals_against = 1L, season_id = "s1"
    ),
    attendance = tibble(date = as.Date("2026-01-14"), player = "Ben", season_id = "s1"),
    payments = tibble(
      date = as.Date("2026-01-21"), player = "Ben", amount = 10, season_id = "s1"
    ),
    events = empty_events(),
    fixtures = tibble(
      date = as.Date("2026-01-21"), opponent = "Brother Man FC",
      dl_match_id = "1", season_id = "s1"
    ),
    league_table = empty_standings(),
    league_results = tibble(
      date = as.Date(character()), home_team = character(), away_team = character(),
      home_goals = integer(), away_goals = integer(), dl_match_id = character()
    )
  )
}

test_that("data with nothing wrong with it returns no problems and warns none", {
  expect_warning(problems <- validate_app_data(clean_data()), NA)
  expect_identical(problems, character())
})

test_that("every problem found comes back, not just the first", {
  data <- clean_data()
  # One unknown name trips two separate checks: not a player, and not on the
  # roster. Both should be reported rather than the first one found.
  data$attendance <- bind_rows(
    data$attendance,
    tibble(date = as.Date("2026-01-14"), player = "Nobody", season_id = "s1")
  )

  problems <- suppressWarnings(validate_app_data(data))

  expect_length(problems, 2)
  expect_true(all(grepl("Nobody", problems)))
})

test_that("the offenders are named in the problem, not just the check", {
  data <- clean_data()
  data$attendance$date <- as.Date("2026-02-11")

  problems <- suppressWarnings(validate_app_data(data))

  expect_match(problems, "attendance for a date with no match", all = FALSE)
  expect_match(problems, "11/02/2026", all = FALSE)
})

test_that("dropping out of the league table is reported like any other problem", {
  data <- clean_data()
  data$league_table <- tibble(
    position = 1L, team = "Shamrock Posers", played = 1L, won = 1L, drawn = 0L,
    lost = 0L, goals_for = 2L, goals_against = 1L, goal_difference = 1L, points = 3L
  )

  problems <- suppressWarnings(validate_app_data(data))

  expect_match(problems, "missing from data/league_table.csv", all = FALSE)
})

test_that("warning is still how it announces itself at startup", {
  data <- clean_data()
  data$payments$player <- "Ghost"

  expect_warning(validate_app_data(data), "payments from an unknown player")
})
