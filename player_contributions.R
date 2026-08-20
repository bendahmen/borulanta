create_player_contribution_table <- function(attendance, matches, players) {
  core_player_names <- players %>%
    filter(core) %>%
    pull(player)

  contribution_data <- attendance %>%
    mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
    mutate(present = 1) %>%
    pivot_wider(names_from = player, values_from = present, values_fill = 0) %>%
    inner_join(
      matches %>% mutate(date = as.Date(date, format = "%d/%m/%Y")),
      by = "date"
    ) %>%
    mutate(
      goals_scored = str_extract(result, "^\\d+") %>% as.integer(),
      goals_conceded = str_extract(result, "\\d+$") %>% as.integer(),
      points = case_when(
        goals_scored > goals_conceded ~ 3,
        goals_scored == goals_conceded ~ 1,
        TRUE ~ 0
      ),
      core_players = rowSums(across(any_of(core_player_names)))
    )
  return(contribution_data)
}

# Player-effect regressions ----
player_regression_results <- function(attendance, matches, players) {
  contribution_data <- create_player_contribution_table(
    attendance,
    matches,
    players
  ) %>%
    mutate(goal_difference = goals_scored - goals_conceded)

  player_names <- attendance %>%
    distinct(player) %>%
    pull(player) %>%
    sort()

  player_appearances <- attendance %>%
    count(player, name = "appearances")

  outcomes <- c(
    "points",
    "goals_scored",
    "goals_conceded",
    "goal_difference"
  )

  purrr::map_dfr(outcomes, function(outcome) {
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

avg_by_player <- function(attendance, matches, players) {
  attendance <- attendance %>%
    mutate(date = as.Date(date, format = "%d/%m/%Y"))

  contribution_data <- create_player_contribution_table(
    attendance,
    matches,
    players
  )
  avg_points_by_player <- data.frame()
  for (p in players$player) {
    player_matches <- attendance %>%
      filter(player == p) %>%
      pull(date)
    if (length(player_matches) == 0) {
      next
    }
    points <- contribution_data %>%
      filter(date %in% player_matches) %>%
      summarise(
        avg_points = mean(points),
        avg_goals_scored = mean(goals_scored),
        avg_goals_conceded = mean(goals_conceded)
      )
    avg_points_by_player <- rbind(
      avg_points_by_player,
      data.frame(
        player = p,
        avg_points = points$avg_points,
        avg_goals_scored = points$avg_goals_scored,
        avg_goals_conceded = points$avg_goals_conceded
      )
    )
  }
  return(avg_points_by_player)
}
