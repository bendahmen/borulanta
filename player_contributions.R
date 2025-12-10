create_player_contribution_table <- function() {
  contribution_data <- attendance %>% 
    mutate(present = 1) %>%
    pivot_wider(names_from = player, values_from = present, values_fill = 0) %>% 
    inner_join(matches, by = "date") %>% 
    mutate(
      goals_scored = str_extract(result, "^\\d+") %>% as.integer(),
      goals_conceded = str_extract(result, "\\d+$") %>% as.integer(),
      points = case_when(
        goals_scored > goals_conceded ~ 3,
        goals_scored == goals_conceded ~ 1,
        TRUE ~ 0
      )
    )
  return(contribution_data)
}
  
#   # Regressions
#   reg_players <- names(contribution_data %>% select(-date, -result, -goals_scored, -goals_conceded, -points, -Langkun, -Yelong, -Pietro, -Jasper, -Oleg))
#   reg_player_fml <- paste(reg_players, collapse = " + ")
#   points_regression <- lm(
#     formula = as.formula(paste("goals_scored ~", reg_player_fml, ' + 0')),
#     data = contribution_data
#   )
# }

avg_by_player <- function() {
  contribution_data <- create_player_contribution_table()
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
