# Match and player analytics ----
#
# Every function here takes matches/attendance that have already been filtered
# to the window of interest (one season, several, or all time), so nothing in
# this file needs to know what a season is. Rates and averages are computed
# from whatever is passed in.

#' Results with points, goals and goal difference parsed out of the score.
match_outcomes <- function(matches) {
  matches %>%
    mutate(
      goals_scored = as.integer(str_extract(result, "^\\d+")),
      goals_conceded = as.integer(str_extract(result, "\\d+$")),
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

player_regression_results <- function(attendance, matches,
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

  contribution_data <- create_player_contribution_table(attendance, matches)
  player_appearances <- attendance %>% count(player, name = "appearances")
  player_names <- player_appearances %>%
    filter(appearances >= min_appearances) %>%
    pull(player) %>%
    sort()

  # With no more matches than players the design matrix has no residual degrees
  # of freedom, so there is nothing to estimate.
  if (length(player_names) == 0 || nrow(contribution_data) <= length(player_names)) {
    return(empty_results())
  }

  purrr::map_dfr(REGRESSION_OUTCOMES, function(outcome) {
    fit <- lm(
      reformulate(player_names, response = outcome, intercept = FALSE),
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
    )
  }) %>%
    left_join(player_appearances, by = "player")
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
      `Points (beta)` = estimate_points,
      `Points (p)` = p_value_points,
      `Goals scored (beta)` = estimate_goals_scored,
      `Goals scored (p)` = p_value_goals_scored,
      `Goals conceded (beta)` = estimate_goals_conceded,
      `Goals conceded (p)` = p_value_goals_conceded,
      `Goal difference (beta)` = estimate_goal_difference,
      `Goal difference (p)` = p_value_goal_difference
    ) %>%
    arrange(desc(Appearances), Player)
}
