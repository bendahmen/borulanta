# The league results file and the opponent strength built on it.
#
# The file is the whole league's results accumulated across seasons; the index
# is each opponent's goal difference per game against everybody but us. Most of
# what matters is the same as for matches.csv — what a sync keeps — plus the
# one place the rules differ: the page wins on a fixture it still lists.

no_league_results <- function() {
  tibble(
    date = as.Date(character()), home_team = character(), away_team = character(),
    home_goals = integer(), away_goals = integer(), dl_match_id = character()
  )
}

#' One parsed fixture, shaped the way `parse_fixtures()` returns them.
league_fixture <- function(date, home, away, home_goals = 0L, away_goals = 0L,
                           id = "1", goals_logged = 0L) {
  tibble(
    date = as.Date(date), dl_match_id = id, home_team = home, away_team = away,
    home_goals = as.integer(home_goals), away_goals = as.integer(away_goals),
    events = list(tibble(
      side = rep("home", goals_logged), minute = rep(1L, goals_logged),
      event_type = rep("goal", goals_logged), player = rep("X", goals_logged)
    ))
  )
}

recorded <- function(date, home, away, home_goals, away_goals, id = "1") {
  tibble(
    date = as.Date(date), home_team = home, away_team = away,
    home_goals = as.integer(home_goals), away_goals = as.integer(away_goals),
    dl_match_id = id
  )
}

sync_league <- function(fixtures, results = no_league_results(),
                        today = as.Date("2026-07-01")) {
  sync_league_results(fixtures, results, today = today)
}

# Syncing the file ----

test_that("every played fixture on the page is recorded, ours included", {
  page <- bind_rows(
    league_fixture("2026-06-03", "Ball FC", "Borulanta", 2L, 1L, id = "1"),
    league_fixture("2026-06-03", "Finessin FC", "The Dirty Boys", 0L, 3L, id = "2")
  )

  result <- sync_league(page)

  expect_true(result$changed)
  expect_equal(nrow(result$added), 2)
  expect_equal(result$league_results$home_team, c("Ball FC", "Finessin FC"))
  expect_equal(result$league_results$dl_match_id, c("1", "2"))
})

test_that("an unplayed, future or ambiguous goalless fixture is not recorded", {
  page <- bind_rows(
    league_fixture("2026-06-03", "Ball FC", "Finessin FC"), # 0-0, nothing logged
    league_fixture("2026-07-01", "Ball FC", "Finessin FC", 3L, 1L), # today
    league_fixture("2026-08-01", "Ball FC", "Finessin FC", 3L, 1L) # future
  )

  result <- sync_league(page)

  expect_false(result$changed)
  expect_equal(nrow(result$league_results), 0)
})

test_that("a goalless draw with goals logged against it counts as played", {
  # The site's default is 0 : 0 with nothing logged; a 0 : 0 that has goal rows
  # is a partially entered result, and a result either way.
  page <- league_fixture("2026-06-03", "Ball FC", "Finessin FC", goals_logged = 1L)

  expect_equal(nrow(sync_league(page)$league_results), 1)
})

test_that("results that have dropped off the page are kept", {
  # After the league season rolls over the page is wiped; a new season's page
  # is full of unplayed fixtures and knows nothing of last season's results.
  on_file <- recorded("2026-02-04", "Ball FC", "Finessin FC", 4L, 2L)
  wiped_page <- league_fixture("2026-09-09", "Ball FC", "Finessin FC")

  result <- sync_league(wiped_page, on_file, today = as.Date("2026-09-10"))

  expect_false(result$changed)
  expect_equal(result$league_results, as_league_result_table(on_file))
})

test_that("a score changed on the site is applied as a correction", {
  on_file <- recorded("2026-06-03", "Ball FC", "Finessin FC", 4L, 2L)
  page <- league_fixture("2026-06-03", "Ball FC", "Finessin FC", 4L, 3L)

  result <- sync_league(page, on_file)

  expect_true(result$changed)
  expect_equal(nrow(result$corrected), 1)
  expect_equal(result$corrected$old_away, 2L)
  expect_equal(result$league_results$away_goals, 3L)
})

test_that("a result that has reverted to the site's default is left alone", {
  on_file <- recorded("2026-06-03", "Ball FC", "Finessin FC", 4L, 2L)
  page <- league_fixture("2026-06-03", "Ball FC", "Finessin FC")

  result <- sync_league(page, on_file)

  expect_false(result$changed)
  expect_equal(result$league_results$home_goals, 4L)
})

test_that("a run with nothing new returns the file untouched, in its order", {
  on_file <- bind_rows(
    recorded("2026-06-03", "Ball FC", "Finessin FC", 4L, 2L),
    recorded("2026-06-10", "The Dirty Boys", "Ball FC", 1L, 1L)
  )
  page <- bind_rows(
    league_fixture("2026-06-03", "Ball FC", "Finessin FC", 4L, 2L),
    league_fixture("2026-06-10", "The Dirty Boys", "Ball FC", 1L, 1L)
  )

  result <- sync_league(page, on_file)

  expect_false(result$changed)
  expect_identical(result$league_results, as_league_result_table(on_file))
})

test_that("a real page of played matches records the whole league", {
  page <- parse_fixtures(fixture_html("hoxton-played.html"))

  result <- sync_league(page, today = as.Date("2030-01-01"))

  # Every fixture on that page has a score, and the file has one row each.
  expect_equal(nrow(result$league_results), nrow(page))
  expect_false(anyNA(result$league_results$home_goals))
})

test_that("the file round-trips through disk and reads as empty when missing", {
  path <- withr::local_tempfile(fileext = ".csv")
  page <- bind_rows(
    league_fixture("2026-06-03", "Ball FC", "Finessin FC", 4L, 2L, id = "96094"),
    league_fixture("2026-06-10", "The Dirty Boys", "Ball FC", 1L, 1L, id = "96095")
  )
  result <- sync_league(page)

  write_league_result_file(result$league_results, path)
  read_back <- read_league_result_file(path)

  expect_equal(read_back, result$league_results)
  # A second sync against what was read back finds nothing to do.
  expect_false(sync_league(page, read_back)$changed)

  missing <- file.path(tempdir(), "definitely-not-here.csv")
  expect_equal(nrow(read_league_result_file(missing)), 0L)
  expect_s3_class(read_league_result_file(missing)$date, "Date")
})

# Opponent strength ----

one_season <- function() {
  tibble(
    season_id = "s1", label = "Season 1", start_date = as.Date("2026-01-01"),
    fee_rules = "core_backstop", status = "open", end_date = as.Date("9999-12-31")
  )
}

# Three other teams and us. Ball FC beat everyone, The Dirty Boys lost to
# everyone, and our results against all of them are deliberately lopsided so a
# leak of our own matches into the index would show.
a_league <- function() {
  bind_rows(
    recorded("2026-01-07", "Ball FC", "Finessin FC", 3L, 1L),
    recorded("2026-01-14", "Ball FC", "The Dirty Boys", 4L, 0L),
    recorded("2026-01-21", "Finessin FC", "The Dirty Boys", 2L, 1L),
    recorded("2026-01-28", "Borulanta", "Ball FC", 9L, 0L),
    recorded("2026-02-04", "The Dirty Boys", "Borulanta", 9L, 0L)
  )
}

test_that("strength is goal difference per game against everyone but us, centred", {
  strength <- opponent_strength(a_league(), one_season(), team = "Borulanta")

  expect_equal(strength$season_id, rep("s1", 3))
  expect_equal(strength$games, rep(2L, 3))
  # Per game: Ball FC +3, Finessin FC -0.5, The Dirty Boys -2.5; mean 0.
  expect_equal(
    strength$strength[match(c("Ball FC", "Finessin FC", "The Dirty Boys"), strength$opponent)],
    c(3, -0.5, -2.5)
  )
  expect_equal(mean(strength$strength), 0)
  expect_false("Borulanta" %in% strength$opponent)
})

test_that("a team that only ever played us has no strength, and no results give none", {
  strength <- opponent_strength(
    recorded("2026-01-07", "Borulanta", "Ball FC", 1L, 0L), one_season()
  )
  expect_equal(nrow(strength), 0)
  expect_equal(nrow(opponent_strength(no_league_results(), one_season())), 0)
})

test_that("a match whose opponent is blank or unmeasured takes the neutral value", {
  strength <- opponent_strength(a_league(), one_season())
  matches <- tibble(
    date = as.Date(c("2026-01-28", "2026-02-04", "2026-02-11")),
    opponent = c("Ball FC", NA, "Nobody FC"),
    season_id = "s1"
  )

  controls <- with_opponent_strength(matches, strength)

  expect_equal(controls$opponent_strength, c(3, 0, 0))
  expect_equal(controls$opponent_unobserved, c(0L, 1L, 1L))
  expect_equal(opponent_coverage(matches, strength), list(observed = 1L, total = 3L))
  expect_equal(with_opponent_strength(matches)$opponent_unobserved, rep(1L, 3))
})

# In the regression ----

# Eight matches, four players who each miss some, so the design has room for a
# control or two.
a_window <- function(strengths) {
  dates <- as.Date("2026-01-07") + 7 * (0:7)
  matches <- tibble(
    date = dates, season_id = "s1",
    opponent = paste("Team", seq_along(dates)),
    goals_for = c(3L, 1L, 4L, 0L, 2L, 5L, 1L, 2L),
    goals_against = c(1L, 2L, 1L, 3L, 2L, 0L, 4L, 2L)
  )
  attendance <- bind_rows(
    tibble(date = dates[c(1, 2, 3, 4, 5, 6, 7)], player = "A"),
    tibble(date = dates[c(1, 3, 5, 7, 8)], player = "B"),
    tibble(date = dates[c(2, 4, 6, 8, 1)], player = "C"),
    tibble(date = dates[c(3, 4, 7, 8, 2, 6)], player = "D")
  )
  strength <- tibble(
    season_id = rep("s1", length(strengths)),
    opponent = names(strengths) %||% character(),
    games = rep(2L, length(strengths)),
    strength = as.numeric(unname(strengths))
  )
  list(matches = matches, attendance = attendance, strength = strength)
}

test_that("with no opponent on file the regression is exactly the one it always was", {
  window <- a_window(c())

  with_control <- player_regression_results(window$attendance, window$matches, window$strength)
  without <- player_regression_results(window$attendance, window$matches)

  expect_equal(with_control, without)
  expect_setequal(with_control$player, c("A", "B", "C", "D"))
})

test_that("a measured opponent shifts the player effects and stays out of the table", {
  window <- a_window(c("Team 1" = 2, "Team 3" = 1.5, "Team 6" = -1, "Team 8" = 0.5))

  with_control <- player_regression_results(window$attendance, window$matches, window$strength)
  without <- player_regression_results(window$attendance, window$matches)

  expect_setequal(with_control$player, c("A", "B", "C", "D"))
  expect_false(any(grepl("opponent", with_control$player)))
  goal_difference <- function(results) {
    results %>% filter(outcome == "goal_difference") %>% arrange(player) %>% pull(estimate) %>% unname()
  }
  expect_false(isTRUE(all.equal(goal_difference(with_control), goal_difference(without))))

  # The same fit by hand: player indicators, the centred index, and the
  # missing indicator, no intercept.
  data <- create_player_contribution_table(
    window$attendance, with_opponent_strength(window$matches, window$strength)
  )
  by_hand <- lm(goal_difference ~ 0 + A + B + C + D + opponent_strength + opponent_unobserved, data)
  expect_equal(goal_difference(with_control), unname(coef(by_hand)[c("A", "B", "C", "D")]))
})

test_that("a control that cannot vary in the window is left out rather than aliased", {
  # One measured opponent: the index has a single value, so only the missing
  # indicator goes in.
  one <- a_window(c("Team 1" = 2))
  data <- create_player_contribution_table(
    one$attendance, with_opponent_strength(one$matches, one$strength)
  )
  expect_equal(opponent_controls(data), "opponent_unobserved")

  # Every opponent measured: the indicator is constant, so only the index.
  all <- a_window(setNames(c(2, 1, 0, -1, -2, 1, 0, -1), paste("Team", 1:8)))
  data <- create_player_contribution_table(
    all$attendance, with_opponent_strength(all$matches, all$strength)
  )
  expect_equal(opponent_controls(data), "opponent_strength")
  expect_equal(nrow(player_regression_results(all$attendance, all$matches, all$strength)), 16)
})

test_that("the controls count against the degrees of freedom", {
  # Eight matches, four players, two controls: six regressors fit. With eight
  # players there would be nothing left, so nothing is estimated.
  window <- a_window(c("Team 1" = 2, "Team 3" = 1.5))
  crowded <- window$attendance %>%
    bind_rows(tibble(date = rep(window$matches$date, 4),
                     player = rep(c("E", "F", "G", "H"), each = 8)))

  expect_gt(nrow(player_regression_results(window$attendance, window$matches, window$strength)), 0)
  expect_equal(nrow(player_regression_results(crowded, window$matches, window$strength)), 0)
})

# Shrinkage ----
#
# Ridge is offered beside OLS on the same tab, so it has to return the same
# shape and shrink in the direction it claims to.

test_that("ridge returns the same shape as OLS, with no interval", {
  dates <- as.Date("2026-01-07") + seq(0, 11) * 7
  attendance <- bind_rows(
    tibble(date = dates[1:10], player = "Ben"),
    tibble(date = dates[c(1:6, 9:12)], player = "Vitto"),
    tibble(date = dates[2:11], player = "Max"),
    tibble(date = dates[c(1, 3, 5, 7, 8, 10, 12)], player = "Boris")
  ) %>% mutate(season_id = "s1")
  matches <- tibble(
    date = dates,
    opponent = "Ball FC",
    goals_for = c(3L, 1L, 2L, 0L, 4L, 1L, 2L, 3L, 1L, 0L, 2L, 1L),
    goals_against = c(1L, 2L, 2L, 3L, 0L, 1L, 4L, 1L, 0L, 2L, 1L, 3L),
    dl_match_id = "1", season_id = "s1"
  )

  ridge <- player_regression_results(attendance, matches, estimator = "ridge")

  expect_setequal(names(ridge), names(player_regression_results(attendance, matches)))
  expect_true(all(is.na(ridge$conf_low)))
  expect_true(all(is.na(ridge$p_value)))
  expect_false(any(is.na(ridge$estimate)))
})

test_that("ridge pulls the estimates in toward each other", {
  # The defining property, and the only one that survives the two fits
  # reporting different quantities: OLS without an intercept gives a share of
  # the scoreline, ridge with one gives a deviation from the average player, so
  # a per-player ratio between them is not a shrinkage factor. Dispersion is.
  dates <- as.Date("2026-01-07") + seq(0, 11) * 7
  attendance <- bind_rows(
    tibble(date = dates[-6], player = "Regular"),
    tibble(date = dates[1:3], player = "Occasional"),
    tibble(date = dates[1:9], player = "A"),
    tibble(date = dates[4:12], player = "B"),
    tibble(date = dates[c(1:4, 8:12)], player = "C")
  ) %>% mutate(season_id = "s1")
  matches <- tibble(
    date = dates, opponent = "Ball FC",
    goals_for = c(5L, 6L, 7L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L),
    goals_against = c(0L, 0L, 0L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
    dl_match_id = "1", season_id = "s1"
  )

  spread <- function(estimator) {
    results <- player_regression_results(attendance, matches, estimator = estimator)
    sd(results$estimate[results$outcome == "goal_difference"])
  }

  expect_lt(spread("ridge"), spread("ols"))
})

test_that("an ever-present player does not take the whole fit down with them", {
  # Their indicator never varies, so nothing separates them from the intercept.
  # OLS aliases the column; glmnet refuses the entire fit unless it is dropped.
  dates <- as.Date("2026-01-07") + seq(0, 9) * 7
  attendance <- bind_rows(
    tibble(date = dates, player = "EverPresent"),
    tibble(date = dates[1:6], player = "Ben"),
    tibble(date = dates[4:10], player = "Vitto"),
    tibble(date = dates[c(1, 3, 5, 7, 9)], player = "Max")
  ) %>% mutate(season_id = "s1")
  matches <- tibble(
    date = dates, opponent = "Ball FC",
    goals_for = c(3L, 1L, 2L, 0L, 4L, 1L, 2L, 3L, 1L, 2L),
    goals_against = c(1L, 2L, 2L, 3L, 0L, 1L, 4L, 1L, 1L, 0L),
    dl_match_id = "1", season_id = "s1"
  )

  ridge <- player_regression_results(attendance, matches, estimator = "ridge")

  expect_false("EverPresent" %in% ridge$player)
  expect_setequal(unique(ridge$player), c("Ben", "Vitto", "Max"))
})

test_that("ridge is reproducible across calls", {
  # cv.glmnet picks lambda by random folds; without a fixed seed the chart would
  # move between page loads on identical data.
  dates <- as.Date("2026-01-07") + seq(0, 9) * 7
  attendance <- bind_rows(
    tibble(date = dates[1:8], player = "Ben"),
    tibble(date = dates[3:10], player = "Vitto"),
    tibble(date = dates[c(1, 2, 5, 6, 9, 10)], player = "Max")
  ) %>% mutate(season_id = "s1")
  matches <- tibble(
    date = dates, opponent = "Ball FC",
    goals_for = c(3L, 1L, 2L, 0L, 4L, 1L, 2L, 3L, 1L, 2L),
    goals_against = c(1L, 2L, 2L, 3L, 0L, 1L, 4L, 1L, 1L, 0L),
    dl_match_id = "1", season_id = "s1"
  )

  first <- player_regression_results(attendance, matches, estimator = "ridge")
  second <- player_regression_results(attendance, matches, estimator = "ridge")

  expect_equal(first$estimate, second$estimate)
})
