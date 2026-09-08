# Fee rules ----
#
# Each season names one rule set (data/seasons.csv `fee_rules`). A rule set is
# a list of:
#   params : named list of the money/threshold constants the rule uses
#   charge : function(squad, roster, params) -> tibble(player, charge, explanation)
#
# `squad`  is the character vector of players recorded present at one match.
# `roster` is the season roster: tibble(player, core, active).
# The function must return one row per rostered player, charge 0 included, so
# the engine never has to know anything about a rule's internals.
#
# To add next season's rules: add an entry here and point the season's
# `fee_rules` column at it. Nothing else needs to change.

parse_match_date <- function(x) {
  as.Date(x, format = "%d/%m/%Y")
}

money <- function(x) formatC(x, format = "f", digits = 2)

FEE_RULE_SETS <- list(

  # Match fee split evenly across everyone who turned up, whatever the numbers.
  even_split = list(
    label = "Even split across the squad",
    params = list(match_fee = 75),
    charge = function(squad, roster, params) {
      squad_size <- length(squad)
      share <- if (squad_size > 0) params$match_fee / squad_size else 0

      tibble(
        player = roster$player,
        charge = if_else(roster$player %in% squad, share, 0),
        explanation = if_else(
          roster$player %in% squad,
          paste0(
            "Played: £", money(params$match_fee), " split across ",
            squad_size, " recorded players."
          ),
          "Did not play: players were charged only when present."
        )
      )
    }
  ),

  # Even split once the squad is big enough; below that, core players cover the
  # shortfall whether or not they played and guests pay a fixed reduced fee.
  # This is the rule that was in force at the end of 2025/26.
  core_backstop = list(
    label = "Even split, with core players covering small squads",
    params = list(match_fee = 75, guest_fee = 6.50, min_players = 7),
    charge = function(squad, roster, params) {
      squad_size <- length(squad)
      played <- roster$player %in% squad

      if (squad_size >= params$min_players) {
        share <- params$match_fee / squad_size
        return(tibble(
          player = roster$player,
          charge = if_else(played, share, 0),
          explanation = if_else(
            played,
            paste0(
              "Played: £", money(params$match_fee), " split across ",
              squad_size, " recorded players."
            ),
            "Did not play: players were charged only when present."
          )
        ))
      }

      guests <- roster$player[!roster$core]
      n_guests <- sum(squad %in% guests)
      guest_total <- n_guests * params$guest_fee
      remainder <- params$match_fee - guest_total
      n_core <- sum(roster$core)
      core_share <- if (n_core > 0) remainder / n_core else 0

      tibble(
        player = roster$player,
        charge = case_when(
          roster$core ~ core_share,
          played ~ params$guest_fee,
          TRUE ~ 0
        ),
        explanation = case_when(
          roster$core ~ paste0(
            "Small squad: after £", money(guest_total),
            " in guest fees, the remaining £", money(remainder),
            " was split across all ", n_core, " core players, including those absent."
          ),
          played ~ paste0(
            "Small squad: played as a guest and paid the fixed £",
            money(params$guest_fee), " fee."
          ),
          TRUE ~ "Small squad: guests were charged only when present."
        )
      )
    }
  )
)

fee_rule_set <- function(name) {
  rules <- FEE_RULE_SETS[[name]]
  if (is.null(rules)) {
    stop("Unknown fee rule set: ", name, ". Add it to FEE_RULE_SETS in R/fees.R.")
  }
  rules
}

# Charge engine ----

#' Every charge for one season, one row per player per match.
#'
#' Archived seasons are read back from their frozen ledger rather than repriced,
#' so retiring a rule set never rewrites history.
season_charges <- function(season, matches, attendance, rosters) {
  if (identical(season$fee_rules, "archived")) {
    return(archived_charges(season$season_id))
  }

  rules <- fee_rule_set(season$fee_rules)
  roster <- rosters %>%
    filter(season_id == season$season_id, active) %>%
    select(player, core)

  season_matches <- matches %>% filter(season_id == season$season_id)
  if (nrow(season_matches) == 0 || nrow(roster) == 0) {
    return(empty_charges())
  }

  squads <- attendance %>%
    filter(season_id == season$season_id) %>%
    split(~ date) %>%
    map(~ .x$player)

  purrr::pmap_dfr(season_matches, function(...) {
    match <- list(...)
    squad <- squads[[as.character(match$date)]]
    if (is.null(squad)) squad <- character(0)

    rules$charge(squad, roster, rules$params) %>%
      mutate(
        season_id = season$season_id,
        date = match$date,
        played = player %in% squad,
        squad_size = length(squad),
        .before = 1
      )
  }) %>%
    select(season_id, date, player, played, squad_size, charge, explanation)
}

empty_charges <- function() {
  tibble(
    season_id = character(), date = as.Date(character()), player = character(),
    played = logical(), squad_size = integer(), charge = numeric(),
    explanation = character()
  )
}

archived_charges <- function(season_id) {
  path <- file.path("data", "archive", paste0("charges_", season_id, ".csv"))
  if (!file.exists(path)) {
    return(empty_charges())
  }

  read_csv(path, show_col_types = FALSE) %>%
    mutate(date = parse_match_date(date))
}

#' All charges across every season, with results attached.
all_charges <- function(seasons, matches, attendance, rosters) {
  purrr::pmap_dfr(seasons, function(...) {
    season <- tibble(...)
    season_charges(season, matches, attendance, rosters)
  }) %>%
    # Ledgers carry no result column; attach it from the fixture list.
    select(-any_of("result")) %>%
    left_join(matches %>% select(date, result), by = "date")
}

# Per-player views ----

player_match_charges <- function(player, charges) {
  charges %>%
    filter(.data$player == .env$player) %>%
    arrange(desc(date)) %>%
    transmute(
      date,
      result,
      played,
      `Squad size` = squad_size,
      charge,
      explanation
    )
}

player_payment_history <- function(player, payments) {
  payments %>%
    filter(.data$player == .env$player) %>%
    arrange(desc(date)) %>%
    transmute(date, amount)
}

player_fee_overview <- function(player, charges, payments) {
  match_charges <- player_match_charges(player, charges)
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
