guest_fee <- 9.75
min_players <- 7
match_fee <- 75
fee_cutoff_date <- as.Date("2026-02-01")

parse_match_date <- function(x) {
  as.Date(x, format = "%d/%m/%Y")
}

legacy_match_fee <- function(player, match_players, players) {
  attended <- player %in% match_players
  is_core_old <- players[players$player == player, ]$core_old

  if (!is_core_old) {
    if (attended) {
      return(guest_fee)
    }
    return(0)
  }

  guest_players <- players[players$core_old == FALSE, ]$player
  n_guests <- sum(match_players %in% guest_players)
  fee_remainder <- match_fee - (n_guests * guest_fee)

  if (length(match_players) >= min_players) {
    if (!attended) {
      return(0)
    }

    return(fee_remainder / (length(match_players) - n_guests))
  }

  core_old_players <- players[players$core_old == TRUE, ]$player
  fee_remainder / length(core_old_players)
}

current_match_fee <- function(player, match_players, players) {
  n_players <- length(match_players)

  if (n_players >= min_players) {
    if (player %in% match_players) {
      return(match_fee / n_players)
    }
    return(0)
  }

  core_players <- players[players$core == TRUE, ]$player
  if (player %in% core_players) {
    return(match_fee / length(core_players))
  }

  0
}

calculate_fees <- function(player, matches, attendance, payments, players) {
  matches <- matches %>%
    mutate(match_date = parse_match_date(date))

  attendance <- attendance %>%
    mutate(match_date = parse_match_date(date))

  total_fees <- 0

  for (match_date in unique(matches$match_date)) {
    match_players <- attendance[attendance$match_date == match_date, ]$player

    if (match_date < fee_cutoff_date) {
      total_fees <- total_fees + legacy_match_fee(player, match_players, players)
    } else {
      total_fees <- total_fees + current_match_fee(player, match_players, players)
    }
  }

  payments_made <- sum(as.numeric(payments[payments$player == player, ]$amount))
  total_fees <- round(total_fees - payments_made, 2)
  total_fees
}

create_attendance_list <- function(matches, attendance, avg_points_by_player) {
  n_matches <- length(unique(matches$date))
  attendance_ranking <- attendance %>%
    group_by(player) %>%
    summarise(rate = n() / n_matches * 100) %>%
    arrange(desc(rate)) %>%
    rename("% Games Played" = rate) %>%
    left_join(avg_points_by_player, by = c("player")) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
  return(attendance_ranking)
}
