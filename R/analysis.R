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
