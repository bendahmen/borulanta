guest_fee <- 9.75
min_players <- 7
match_fee <- 75

calculate_fees <- function(player, matches, attendance, payments, players) {
  total_fees <- 0
  # How much does the player owe?
  for (match in unique(matches$date)) {
    match_players <- attendance[attendance$date == match,]$player
    attended <- player %in% match_players
    # Is the player a guest?
    if (!players[players$player == player,]$regular) {
      # Did the player attend?
      if (attended) {
        total_fees <- total_fees + guest_fee
      } else {
        total_fees <- total_fees + 0
      }
    } else {
      n_guests <- sum(match_players %in% players[players$regular == FALSE,]$player)
      fee_remainder <- match_fee - (n_guests * guest_fee)
      # Were there enough players?
      if (length(match_players) >= min_players) {
        # Did the player attend?
        if (attended) {
          regular_fee <- fee_remainder /
            (length(match_players) - n_guests)
          total_fees <- total_fees + regular_fee
        } else {
          total_fees <- total_fees + 0
        }
      } else {
        regular_fee <- fee_remainder / length(players[players$regular == TRUE,]$player)
        total_fees <- total_fees + regular_fee
      }
    }
  }
  # Subtract any payments made
  payments_made <- sum(as.numeric(payments[payments$player == player,]$amount))
  total_fees <- round(total_fees - payments_made, 2)
  return(total_fees)
}

create_attendance_list <- function(matches, attendance, avg_points_by_player) {
  n_matches <- length(unique(matches$date))
  attendance_ranking <- attendance %>% 
    group_by(player) %>% 
    summarise(rate = n() / n_matches * 100) %>% 
    arrange(desc(rate)) %>% 
    rename('% Games Played' = rate) %>% 
    left_join(avg_points_by_player, by = c("player")) %>% 
    mutate(across(where(is.numeric), ~ round(.x, 2)))
  return(attendance_ranking)
}
