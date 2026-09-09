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

#' Match-level regressions of each outcome on player indicators.
#'
#' `strength` is the table from `opponent_strength()`; when it is given,
#' `matches` must carry a `season_id`. Only the player coefficients are
#' returned — the opponent controls are there to be conditioned on, not read.
player_regression_results <- function(attendance, matches, strength = NULL,
                                      min_appearances = MIN_REGRESSION_APPEARANCES) {
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
  # degrees of freedom, so there is nothing to estimate.
  if (length(player_names) == 0 || nrow(contribution_data) <= length(regressors)) {
    return(empty_results())
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
