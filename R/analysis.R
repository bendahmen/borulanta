# Match and player analytics ----
#
# Every function here takes matches/attendance that have already been filtered
# to the window of interest (one season, several, or all time), so nothing in
# this file needs to know what a season is. Rates and averages are computed
# from whatever is passed in.

#' Results with points and goal difference derived from the score.
match_outcomes <- function(matches) {
  matches %>%
    mutate(
      goals_scored = as.integer(goals_for),
      goals_conceded = as.integer(goals_against),
      points = case_when(
        goals_scored > goals_conceded ~ 3,
        goals_scored == goals_conceded ~ 1,
        TRUE ~ 0
      ),
      goal_difference = goals_scored - goals_conceded
    )
}

#' One row per match, one 0/1 column per player, plus that match's outcome.
create_player_contribution_table <- function(attendance, matches) {
  if (nrow(attendance) == 0 || nrow(matches) == 0) {
    return(match_outcomes(matches[0, , drop = FALSE]))
  }

  attendance %>%
    select(date, player) %>%
    mutate(present = 1L) %>%
    pivot_wider(names_from = player, values_from = present, values_fill = 0L) %>%
    inner_join(match_outcomes(matches), by = "date")
}

rolling_match_average <- function(values, window = 5L) {
  vapply(seq_along(values), function(index) {
    first_index <- max(1L, index - window + 1L)
    mean(values[first_index:index])
  }, numeric(1))
}

season_form_data <- function(attendance, matches) {
  attendance_by_match <- attendance %>% count(date, name = "squad_size")

  match_outcomes(matches) %>%
    left_join(attendance_by_match, by = "date") %>%
    mutate(squad_size = coalesce(squad_size, 0L)) %>%
    arrange(date) %>%
    mutate(
      rolling_points = rolling_match_average(points),
      rolling_goals_scored = rolling_match_average(goals_scored),
      rolling_goals_conceded = rolling_match_average(goals_conceded),
      rolling_goal_difference = rolling_match_average(goal_difference),
      rolling_squad_size = rolling_match_average(squad_size)
    )
}

match_detail_data <- function(season_form, attendance, selected_date) {
  selected_date <- as.Date(selected_date)
  selected_match <- season_form %>% filter(date == selected_date)

  if (nrow(selected_match) != 1) {
    stop("selected_date must identify exactly one match")
  }

  n_matches <- nrow(season_form)
  player_attendance <- attendance %>%
    count(player, name = "season_appearances") %>%
    mutate(attendance_rate = season_appearances / n_matches)

  lineup <- attendance %>%
    filter(date == selected_date) %>%
    distinct(player) %>%
    left_join(player_attendance, by = "player") %>%
    arrange(desc(season_appearances), player)

  list(match = selected_match, lineup = lineup)
}

#' Appearance rate and on-pitch averages, one row per player.
player_summary <- function(attendance, matches) {
  n_matches <- n_distinct(matches$date)
  outcomes <- match_outcomes(matches) %>%
    select(date, points, goals_scored, goals_conceded)

  attendance %>%
    inner_join(outcomes, by = "date") %>%
    group_by(player) %>%
    summarise(
      appearances = n(),
      avg_points = mean(points),
      avg_goals_scored = mean(goals_scored),
      avg_goals_conceded = mean(goals_conceded),
      .groups = "drop"
    ) %>%
    mutate(attendance_rate = appearances / n_matches)
}

create_attendance_list <- function(attendance, matches) {
  player_summary(attendance, matches) %>%
    arrange(desc(attendance_rate)) %>%
    transmute(
      Player = player,
      `% Games Played` = attendance_rate * 100,
      `Avg points` = avg_points,
      `Avg goals scored` = avg_goals_scored,
      `Avg goals conceded` = avg_goals_conceded
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
}

# Goals and man of the match ----
#
# Both arrived with the sync, so they exist only for the matches the sync
# wrote. Attendance goes back further, and dividing a player's goals by every
# appearance they have ever made would understate each of them by a different
# amount depending on how much of the archive they played in. So the whole
# table is computed over the covered matches alone, and the card says how many
# those are.
#
# A match is covered when it carries the site's fixture id, which the sync
# stamps on everything it writes. Keying on "has an event on file" instead
# would be wrong in the direction that matters: a synced goalless draw with no
# man of the match has no events and is nonetheless completely recorded.

#' Which matches in the window have their events on file.
event_coverage <- function(matches) {
  covered <- !is.na(matches$dl_match_id)
  list(
    dates = matches$date[covered],
    observed = sum(covered),
    total = nrow(matches)
  )
}

#' Goals and man of the match per player, over the matches we have events for.
#'
#' Everyone who appeared in a covered match is listed, scorer or not: a
#' leaderboard that drops the players on nought is not the squad. A player with
#' events but no attendance row is listed too, on no appearances, rather than
#' having their goals quietly left out of the tally — that is a data problem
#' validate_app_data() reports, and a total that no longer reconciles with the
#' scorelines would otherwise be the only sign of it.
scorer_table <- function(events, attendance, matches) {
  coverage <- event_coverage(matches)

  if (coverage$observed == 0 || nrow(events) == 0) {
    return(tibble(
      player = character(), goals = integer(), mom = integer(),
      appearances = integer(), goals_per_appearance = numeric()
    ))
  }

  ours <- events %>%
    filter(team == "us", date %in% coverage$dates)

  tally <- function(type, name) {
    ours %>%
      filter(event_type == type) %>%
      count(player, name = name)
  }

  attendance %>%
    filter(date %in% coverage$dates) %>%
    count(player, name = "appearances") %>%
    full_join(tally("goal", "goals"), by = "player") %>%
    full_join(tally("mom", "mom"), by = "player") %>%
    mutate(
      across(c(appearances, goals, mom), ~ coalesce(.x, 0L)),
      # Nought from nothing is not a rate. A player with events but no
      # appearance on file would otherwise divide by zero and sort to the top.
      goals_per_appearance = if_else(appearances > 0, goals / appearances, NA_real_)
    ) %>%
    arrange(desc(goals), desc(mom), player)
}

# Opponent strength ----
#
# The player regressions below compare matches with one another, and a match
# against the league's best side is not the same test as one against its worst.
# Opponent fixed effects are out of reach — eight other teams, each met about
# twice a season, on a design that already spends a column per player — so the
# opponent enters as one number: its goal difference per game over the season,
# measured on its matches against everybody except us.
#
# Leaving our own matches out matters. Our result is a seventh of an opponent's
# record, so a heavy defeat to them would raise their measured strength and put
# part of our own residual into the control. The whole season's record is used,
# games played after ours included: the aim is to measure how good they were,
# not to forecast, and a table two weeks into a season says nearly nothing.
#
# The index is centred within season so that 0 is an average opponent. That is
# also the value a match takes when its opponent is not on file — every match
# from before the sync existed — alongside an indicator saying so, which lets
# those matches keep their own level instead of forcing it through the player
# indicators. The strength coefficient is identified only off matches whose
# opponent is known, so the old matches cost it nothing either way.

#' Goal difference per game against third parties, one row per season and team.
#'
#' Seasons are the fee seasons from seasons.csv rather than the league's own:
#' they are the windows the app scopes a regression to, and the league's season
#' is not recorded anywhere. A team that met nobody but us in a season has no
#' row, and a result outside every season is ignored.
opponent_strength <- function(league_results, seasons, team = OUR_TEAM) {
  if (nrow(league_results) == 0) {
    return(tibble(
      season_id = character(), opponent = character(),
      games = integer(), strength = numeric()
    ))
  }

  third_party <- league_results %>%
    filter(home_team != team, away_team != team) %>%
    with_season(seasons) %>%
    filter(!is.na(season_id))

  # Each result is one game for each of its two sides.
  bind_rows(
    third_party %>%
      transmute(season_id, opponent = home_team, goal_difference = home_goals - away_goals),
    third_party %>%
      transmute(season_id, opponent = away_team, goal_difference = away_goals - home_goals)
  ) %>%
    group_by(season_id, opponent) %>%
    summarise(games = n(), per_game = mean(goal_difference), .groups = "drop") %>%
    group_by(season_id) %>%
    mutate(strength = per_game - mean(per_game)) %>%
    ungroup() %>%
    select(season_id, opponent, games, strength)
}

#' Attach the control columns the regression uses to a season-tagged match table.
#'
#' `opponent_strength` is 0 and `opponent_unobserved` is 1 wherever the opponent
#' is blank or has no row in `strength` for that season. With no strength table
#' at all every match is unobserved, which is what the regression ran on before
#' the league results existed.
with_opponent_strength <- function(matches, strength = NULL) {
  if (is.null(strength) || nrow(strength) == 0) {
    return(matches %>% mutate(opponent_strength = 0, opponent_unobserved = 1L))
  }

  matches %>%
    left_join(
      strength %>% select(season_id, opponent, strength),
      by = c("season_id", "opponent")
    ) %>%
    mutate(
      opponent_unobserved = as.integer(is.na(strength)),
      opponent_strength = coalesce(strength, 0)
    ) %>%
    select(-strength)
}

#' How many of the matches in a window have a measured opponent.
opponent_coverage <- function(matches, strength = NULL) {
  controls <- with_opponent_strength(matches, strength)
  list(
    observed = sum(controls$opponent_unobserved == 0L),
    total = nrow(controls)
  )
}

# Player-effect regressions ----
#
# Players with only a handful of appearances cannot be separated from the
# matches they happened to play in, so they are dropped from the design matrix
# (they stay in attendance and every other statistic). At the default threshold
# of 3 this excludes exactly the players the previous hard-coded list did.
MIN_REGRESSION_APPEARANCES <- 3L

REGRESSION_OUTCOMES <- c(
  "points",
  "goals_scored",
  "goals_conceded",
  "goal_difference"
)

# Shrinkage ----
#
# The OLS fit spends one column per player on 44 matches, so a three-appearance
# player's coefficient is estimated off three nights and the extremes of the
# chart are populated by exactly the people we know least about. Ridge pulls
# every estimate toward the middle by an amount inversely proportional to how
# much is known about that player: three appearances move a long way, thirty
# barely move. It is the same fix, for the same reason, as regularised adjusted
# plus-minus in basketball — collinear lineups, thin data per player.
#
# Three settings matter and are easy to get wrong.
#
# The intercept is fitted and left unpenalised. Without one, shrinking toward
# zero means shrinking toward "contributed nothing to the scoreline", and ten
# players share a four-goal total, so the whole table would be biased downward.
# With one, the shrinkage target is the average player, which is the comparison
# anybody reading the chart already has in mind.
#
# standardize = FALSE, because glmnet's default penalises on the standardised
# scale: a rare player has a small standard deviation, so his original-scale
# coefficient would be penalised *less* than a regular's. That is backwards.
# On the original scale an equal penalty shrinks the thin players more, which
# is the entire point.
#
# The opponent controls are unpenalised. They are there to be conditioned on,
# not estimated, and shrinking them would leak opponent quality back into the
# player coefficients.
#
# What it costs is the confidence interval: ridge has no usable analytic
# standard error, so those columns come back NA and the display drops the parts
# that depend on them.
RIDGE_SEED <- 20260910L

#' Ridge coefficients for one outcome, on the same design OLS is given.
ridge_coefficients <- function(formula, data, penalised) {
  design <- model.matrix(formula, data = data)
  design <- design[, colnames(design) != "(Intercept)", drop = FALSE]
  response <- data[[all.vars(formula)[[1]]]]

  # cv.glmnet picks lambda by k-fold CV, which is random; at n in the dozens it
  # would otherwise move between page loads on identical data.
  # Ten folds over a single short season leaves one or two matches in each,
  # which cv.glmnet complains about and which makes the chosen penalty mostly
  # noise. Keep at least three matches per fold, and at least three folds.
  folds <- max(3L, min(10L, nrow(design) %/% 3L))

  withr::with_seed(RIDGE_SEED, {
    fit <- glmnet::cv.glmnet(
      x = design, y = response, alpha = 0, nfolds = folds,
      standardize = FALSE, intercept = TRUE,
      penalty.factor = as.integer(colnames(design) %in% penalised)
    )
    # lambda.min rather than lambda.1se: the 1-SE rule flattens nearly
    # everything to the intercept at this sample size, which is defensible
    # inference and a useless chart.
    coefficients <- as.matrix(stats::coef(fit, s = "lambda.min"))
  })

  stats::setNames(coefficients[, 1], rownames(coefficients))
}

#' Match-level regressions of each outcome on player indicators.
#'
#' `strength` is the table from `opponent_strength()`; when it is given,
#' `matches` must carry a `season_id`. Only the player coefficients are
#' returned — the opponent controls are there to be conditioned on, not read.
#'
#' `estimator` picks OLS or ridge. The two report different quantities and the
#' display says which: without an intercept an OLS coefficient is a player's
#' additive share of the scoreline, while a ridge coefficient is their deviation
#' from the average player. The return shape is the same either way, with the
#' uncertainty columns NA under ridge.
player_regression_results <- function(attendance, matches, strength = NULL,
                                      min_appearances = MIN_REGRESSION_APPEARANCES,
                                      estimator = c("ols", "ridge")) {
  estimator <- match.arg(estimator)
  empty_results <- function() {
    tibble(
      player = character(), outcome = character(), estimate = numeric(),
      std_error = numeric(), conf_low = numeric(), conf_high = numeric(),
      p_value = numeric(), appearances = integer()
    )
  }

  if (nrow(attendance) == 0 || nrow(matches) == 0) {
    return(empty_results())
  }

  contribution_data <- create_player_contribution_table(
    attendance, with_opponent_strength(matches, strength)
  )
  player_appearances <- attendance %>% count(player, name = "appearances")
  player_names <- player_appearances %>%
    filter(appearances >= min_appearances) %>%
    pull(player) %>%
    sort()
  regressors <- c(player_names, opponent_controls(contribution_data))

  # With no more matches than regressors the design matrix has no residual
  # degrees of freedom, so OLS has nothing to estimate. Ridge is defined either
  # way — the penalty is what buys the identification back — so the guard only
  # applies to the fit that needs it.
  if (length(player_names) == 0) {
    return(empty_results())
  }
  if (estimator == "ols" && nrow(contribution_data) <= length(regressors)) {
    return(empty_results())
  }

  if (estimator == "ridge") {
    # A player who turned out for every match in the window has an indicator
    # that never varies, so nothing separates them from the intercept. OLS
    # aliases such a column and reports NA; glmnet refuses the whole fit. Drop
    # them from the design rather than lose the other players with them — being
    # ever-present is a plausible thing to be in a single short season.
    varies <- vapply(
      player_names,
      function(name) n_distinct(contribution_data[[name]]) > 1,
      logical(1)
    )
    player_names <- player_names[varies]
    if (length(player_names) == 0) {
      return(empty_results())
    }
    regressors <- c(player_names, opponent_controls(contribution_data))

    return(purrr::map_dfr(REGRESSION_OUTCOMES, function(outcome) {
      estimates <- ridge_coefficients(
        reformulate(regressors, response = outcome, intercept = TRUE),
        contribution_data,
        penalised = player_names
      )
      tibble(
        player = names(estimates),
        outcome = outcome,
        estimate = unname(estimates),
        std_error = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
        p_value = NA_real_
      ) %>%
        filter(player %in% player_names)
    }) %>%
      left_join(player_appearances, by = "player"))
  }

  purrr::map_dfr(REGRESSION_OUTCOMES, function(outcome) {
    fit <- lm(
      reformulate(regressors, response = outcome, intercept = FALSE),
      data = contribution_data
    )
    coefficients <- summary(fit)$coefficients
    confidence_critical_value <- qt(0.975, df = df.residual(fit))

    tibble(
      player = rownames(coefficients),
      outcome = outcome,
      estimate = coefficients[, "Estimate"],
      std_error = coefficients[, "Std. Error"],
      conf_low = estimate - confidence_critical_value * std_error,
      conf_high = estimate + confidence_critical_value * std_error,
      p_value = coefficients[, "Pr(>|t|)"]
    ) %>%
      filter(player %in% player_names)
  }) %>%
    left_join(player_appearances, by = "player")
}

#' Which opponent controls the window can actually identify.
#'
#' A control with no variation in the window is left out rather than handed to
#' lm() to alias: the strength index needs at least two different measured
#' values, and the unobserved indicator needs both observed and unobserved
#' matches. A window with no opponent on file — every one before the sync
#' existed — gets neither, and the regression is exactly the one it always was.
opponent_controls <- function(contribution_data) {
  observed <- contribution_data$opponent_unobserved == 0L
  controls <- character()
  if (n_distinct(contribution_data$opponent_strength[observed]) > 1) {
    controls <- c(controls, "opponent_strength")
  }
  if (any(observed) && !all(observed)) {
    controls <- c(controls, "opponent_unobserved")
  }
  controls
}

#' The coefficient with its p-value in brackets, as one string.
#'
#' One column per outcome rather than two: the p-value is a footnote to the
#' coefficient, not a number anyone reads on its own.
format_estimate <- function(estimate, p_value) {
  # Ridge has no usable analytic standard error, so there is no bracket to
  # print, and its estimates are small enough that two decimals would render
  # the whole column as 0.00. Significant figures instead, so the ranking the
  # penalty produced is still legible.
  if (all(is.na(p_value))) {
    return(formatC(estimate, format = "g", digits = 3))
  }

  paste0(
    formatC(estimate, format = "f", digits = 2),
    " [",
    if_else(
      p_value < 0.001,
      "<0.001",
      formatC(p_value, format = "f", digits = 3)
    ),
    "]"
  )
}

player_regression_table <- function(regression_results) {
  regression_results %>%
    select(player, appearances, outcome, estimate, p_value) %>%
    pivot_wider(
      names_from = outcome,
      values_from = c(estimate, p_value),
      names_sep = "_"
    ) %>%
    transmute(
      Player = player,
      Appearances = appearances,
      Points = format_estimate(estimate_points, p_value_points),
      `Goals scored` = format_estimate(estimate_goals_scored, p_value_goals_scored),
      `Goals conceded` = format_estimate(estimate_goals_conceded, p_value_goals_conceded),
      `Goal difference` = format_estimate(
        estimate_goal_difference, p_value_goal_difference
      ),
      # Hidden in the table, and there only so that clicking a column sorts on
      # the coefficient rather than on the string that displays it.
      sort_points = estimate_points,
      sort_goals_scored = estimate_goals_scored,
      sort_goals_conceded = estimate_goals_conceded,
      sort_goal_difference = estimate_goal_difference
    ) %>%
    arrange(desc(Appearances), Player)
}

# The columns above, paired: the one you see and the one it sorts on.
REGRESSION_TABLE_COLUMNS <- c(
  Points = "sort_points",
  `Goals scored` = "sort_goals_scored",
  `Goals conceded` = "sort_goals_conceded",
  `Goal difference` = "sort_goal_difference"
)

# The home page ----
#
# Three summaries of where things stand right now. Unlike everything above,
# these are deliberately not scoped by the season picker: the home page answers
# "what just happened and what is next", and a question phrased in the present
# tense does not take a season argument. They are still pure functions of what
# they are handed, so a caller that wants a season can filter first.

#' The most recent match played, with who was there and what they did.
#'
#' Returns NULL when there is no match at all, which is a real state on a fresh
#' season rather than an error. `scorers` and `mom` come from the event log,
#' which is thin for anything predating the sync and empty for everything
#' before it existed — so both come back empty rather than absent, and the
#' caller renders what it has.
last_match_summary <- function(matches, attendance, events) {
  if (nrow(matches) == 0) {
    return(NULL)
  }

  match <- matches %>%
    filter(date == max(date)) %>%
    slice(1) %>%
    match_outcomes()

  lineup <- attendance %>%
    filter(date == match$date) %>%
    distinct(player) %>%
    arrange(player)

  match_events <- events %>% filter(date == match$date)

  # One row per scorer with a tally, since a hat-trick should read as a
  # hat-trick rather than three identical lines.
  scorers <- match_events %>%
    filter(event_type == "goal", team == "us", !is.na(player)) %>%
    count(player, name = "goals") %>%
    arrange(desc(goals), player)

  mom <- match_events %>%
    filter(event_type == "mom", team == "us", !is.na(player)) %>%
    pull(player)

  list(
    match = match,
    lineup = lineup,
    squad_size = nrow(lineup),
    scorers = scorers,
    mom = if (length(mom) == 0) NA_character_ else mom[[1]]
  )
}

#' The next fixture we are down to play, or NULL if the list has run out.
#'
#' Two things are filtered out, not one. A fixture in the past is obviously
#' gone; so is a fixture whose date already carries a result, which is what a
#' fixture list looks like between a match being played and the next sync
#' replacing the file. Without that second filter the home page would spend
#' every Wednesday evening offering that afternoon's game as the one to come.
#'
#' A fixture dated today counts as still to come, matching the sync: the game is
#' in the evening and the page is read during the day.
next_fixture <- function(fixtures, matches, today = Sys.Date()) {
  upcoming <- fixtures %>%
    filter(date >= today, !date %in% matches$date) %>%
    arrange(date)

  if (nrow(upcoming) == 0) {
    return(NULL)
  }
  upcoming %>% slice(1)
}

# One player at a time ----

#' Everything the app knows about one player, over the matches in scope.
#'
#' The with/without comparison is the reason this exists rather than a filtered
#' row of the attendance table. It is also the number most likely to be
#' over-read, so it is reported as two records side by side rather than as a
#' single effect: the difference between them is not an estimate of anything a
#' player did, only of how the nights they turned up happened to go.
#'
#' Goals and man of the match are counted over the covered matches only, the
#' same rule the scorer table uses, and the coverage comes back with them so
#' the card can say what it is out of.
player_profile <- function(player, attendance, matches, events) {
  outcomes <- match_outcomes(matches)
  appearances <- attendance %>% filter(.data$player == .env$player)
  played_dates <- appearances$date

  record_over <- function(dates) {
    subset <- outcomes %>% filter(date %in% dates)
    tibble(
      played = nrow(subset),
      won = sum(subset$points == 3),
      drawn = sum(subset$points == 1),
      lost = sum(subset$points == 0),
      goals_for = sum(subset$goals_scored),
      goals_against = sum(subset$goals_conceded),
      # NA rather than NaN on an empty set: there is no rate, and NaN prints
      # as a number that looks like one.
      points_per_match = if (nrow(subset) > 0) mean(subset$points) else NA_real_
    )
  }

  present <- record_over(played_dates)
  absent <- record_over(setdiff(outcomes$date, played_dates))

  coverage <- event_coverage(matches)
  ours <- events %>%
    filter(team == "us", .data$player == .env$player, date %in% coverage$dates)

  list(
    player = player,
    matches = nrow(outcomes),
    appearances = present$played,
    attendance_rate = if (nrow(outcomes) > 0) present$played / nrow(outcomes) else NA_real_,
    last_appearance = if (length(played_dates) > 0) max(played_dates) else NA,
    present = present,
    absent = absent,
    differential = present$points_per_match - absent$points_per_match,
    goals = sum(ours$event_type == "goal"),
    mom = sum(ours$event_type == "mom"),
    covered = coverage$observed,
    timeline = outcomes %>%
      transmute(date, points, played = date %in% played_dates) %>%
      arrange(date)
  )
}

#' Who the player picker can offer: anyone who turned out in the window.
#'
#' Deliberately not the active roster, which is what the fee picker uses. This
#' is a page about matches that happened, so a player who has since left still
#' has one and a new signing who has not played yet does not.
players_in_scope <- function(attendance) {
  sort(unique(attendance$player))
}

# Opponents ----

#' Our record against each side we have played, one row per opponent.
#'
#' Matches from before the sync have no opponent recorded and are dropped
#' rather than pooled into an "unknown" row: they are a different opponent each
#' time, and a row averaging all of them says nothing about anybody.
#'
#' Points per match rather than total points, because we have met some sides
#' twice as often as others and the totals would rank on that instead of on how
#' the games went.
opponent_record <- function(matches) {
  played <- matches %>% filter(!is.na(opponent))

  if (nrow(played) == 0) {
    return(tibble(
      opponent = character(), played = integer(), won = integer(),
      drawn = integer(), lost = integer(), goals_for = integer(),
      goals_against = integer(), goal_difference = integer(),
      points_per_match = numeric()
    ))
  }

  match_outcomes(played) %>%
    group_by(opponent) %>%
    summarise(
      played = n(),
      won = sum(points == 3),
      drawn = sum(points == 1),
      lost = sum(points == 0),
      goals_for = sum(goals_scored),
      goals_against = sum(goals_conceded),
      .groups = "drop"
    ) %>%
    mutate(
      goal_difference = goals_for - goals_against,
      points_per_match = (won * 3 + drawn) / played
    ) %>%
    arrange(desc(points_per_match), desc(goal_difference), opponent)
}

#' The fixtures still to come, with what the table says about who we are facing.
#'
#' Deliberately not season-scoped. Fixtures and the standings both describe the
#' current league season only — they are wiped and rebuilt on the site when it
#' rolls over — so scoping them by a fee season would be filtering one thing by
#' the boundaries of another.
#'
#' An opponent the standings do not carry keeps its row with the league columns
#' empty. The fixture is still a fixture, and dropping it would silently
#' shorten the run-in.
run_in <- function(fixtures, matches, league_table, today = Sys.Date()) {
  upcoming <- fixtures %>%
    filter(date >= today, !date %in% matches$date) %>%
    arrange(date)

  if (nrow(upcoming) == 0) {
    return(tibble(
      date = as.Date(character()), opponent = character(),
      position = integer(), played = integer(), goal_difference = integer(),
      points = integer()
    ))
  }

  standings <- if (nrow(league_table) == 0) {
    tibble(
      team = character(), position = integer(), played = integer(),
      goal_difference = integer(), points = integer()
    )
  } else {
    league_table %>%
      select(team, position, played, goal_difference, points)
  }

  upcoming %>%
    select(date, opponent) %>%
    left_join(standings, by = c("opponent" = "team"))
}

#' Played, won, drawn, lost and goals over whatever matches are handed in.
#'
#' Always one row, zeroes included, so a caller can print it without checking
#' whether a season has started.
season_record <- function(matches) {
  outcomes <- match_outcomes(matches)

  tibble(
    played = nrow(outcomes),
    won = sum(outcomes$points == 3),
    drawn = sum(outcomes$points == 1),
    lost = sum(outcomes$points == 0),
    goals_for = sum(outcomes$goals_scored),
    goals_against = sum(outcomes$goals_conceded),
    goal_difference = goals_for - goals_against,
    points = sum(outcomes$points)
  )
}
