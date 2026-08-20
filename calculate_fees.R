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

match_charge_detail <- function(player, match_date, match_players, players) {
  player_record <- players[players$player == player, , drop = FALSE]
  if (nrow(player_record) != 1) {
    stop("player must identify exactly one player")
  }

  played <- player %in% match_players
  squad_size <- length(match_players)

  if (match_date < fee_cutoff_date) {
    is_core_old <- player_record$core_old
    guest_players <- players[players$core_old == FALSE, ]$player
    n_guests <- sum(match_players %in% guest_players)
    fee_remainder <- match_fee - n_guests * guest_fee

    if (!is_core_old) {
      charge <- if (played) guest_fee else 0
      explanation <- if (played) {
        "Played as a guest: fixed guest fee."
      } else {
        "Did not play: guests were not charged."
      }
    } else if (squad_size >= min_players) {
      core_players_present <- squad_size - n_guests
      charge <- if (played) fee_remainder / core_players_present else 0
      explanation <- if (played) {
        paste0(
          "Played: the remaining £", formatC(fee_remainder, format = "f", digits = 2),
          " was split across ", core_players_present, " legacy core players."
        )
      } else {
        "Did not play: legacy core players were charged only when present."
      }
    } else {
      n_core_old <- sum(players$core_old)
      charge <- fee_remainder / n_core_old
      explanation <- paste0(
        "Small squad: the remaining £", formatC(fee_remainder, format = "f", digits = 2),
        " was split across all ", n_core_old, " legacy core players."
      )
    }
  } else if (squad_size >= min_players) {
    charge <- if (played) match_fee / squad_size else 0
    explanation <- if (played) {
      paste0("Played: £75.00 was split across ", squad_size, " recorded players.")
    } else {
      "Did not play: players were charged only when present."
    }
  } else if (player_record$core) {
    n_core <- sum(players$core)
    charge <- match_fee / n_core
    explanation <- paste0(
      "Small squad: £75.00 was split across all ", n_core,
      " core players, including those absent."
    )
  } else {
    charge <- 0
    explanation <- "Small squad: non-core players were not charged."
  }

  tibble(
    played = played,
    squad_size = squad_size,
    charge = charge,
    explanation = explanation
  )
}

player_match_charges <- function(player, matches, attendance, players) {
  attendance <- attendance %>%
    mutate(match_date = parse_match_date(date))

  matches %>%
    mutate(match_date = parse_match_date(date)) %>%
    arrange(desc(match_date)) %>%
    mutate(
      match_players = purrr::map(
        match_date,
        ~ attendance[attendance$match_date == .x, ]$player
      ),
      charge_detail = purrr::map2(
        match_date,
        match_players,
        ~ match_charge_detail(player, .x, .y, players)
      )
    ) %>%
    tidyr::unnest(charge_detail) %>%
    transmute(
      date = match_date,
      result = result,
      played = played,
      `Squad size` = squad_size,
      charge = charge,
      explanation = explanation
    )
}

player_payment_history <- function(player, payments) {
  payments %>%
    mutate(date = parse_match_date(date)) %>%
    filter(.data$player == .env$player) %>%
    arrange(desc(date)) %>%
    transmute(date, amount)
}

player_fee_overview <- function(player, matches, attendance, payments, players) {
  match_charges <- player_match_charges(player, matches, attendance, players)
  payment_history <- player_payment_history(player, payments)
  total_charges <- sum(match_charges$charge)
  total_payments <- sum(payment_history$amount)

  list(
    match_charges = match_charges,
    payment_history = payment_history,
    total_charges = round(total_charges, 2),
    total_payments = round(total_payments, 2),
    balance = round(total_charges - total_payments, 2)
  )
}

calculate_fees <- function(player, matches, attendance, payments, players) {
  player_fee_overview(player, matches, attendance, payments, players)$balance
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
