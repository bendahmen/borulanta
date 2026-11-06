# What each rule set charges, and that the money always adds up.
#
# The invariant worth guarding is that a match's charges sum to the match fee: a
# rule that quietly under- or over-collects looks fine in the app, since every
# row on its own is plausible.

test_roster <- function(core, guests = character()) {
  tibble(
    player = c(core, guests),
    core = c(rep(TRUE, length(core)), rep(FALSE, length(guests)))
  )
}

# Twelve core and four guests, the shape of the real season 3 roster.
CORE <- paste0("C", 1:12)
GUESTS <- paste0("G", 1:4)
big_roster <- test_roster(CORE, GUESTS)

# A squad of `n` that always contains C1 and always leaves some core absent, so
# the floor is exercised rather than the whole-core fallback.
squad_of <- function(n) c("C1", GUESTS, CORE[-1])[seq_len(n)]

charge_for <- function(result, player) {
  result$charge[match(player, result$player)]
}

md_params <- FEE_RULE_SETS$min_denominator$params
md_charge <- function(squad, roster = big_roster) {
  FEE_RULE_SETS$min_denominator$charge(squad, roster, md_params)
}

test_that("min_denominator prices the season 3 fee and threshold", {
  expect_equal(md_params$match_fee, 76)
  expect_equal(md_params$min_players, 8)
})

test_that("min_denominator is a plain even split at or above the threshold", {
  for (n in c(8, 9, 12, 16)) {
    result <- md_charge(squad_of(n))
    expect_equal(charge_for(result, "C1"), 76 / n)
    expect_equal(sum(result$charge), 76)
    # Nobody absent pays anything once the squad is big enough.
    expect_equal(sum(result$charge[!big_roster$player %in% squad_of(n)]), 0)
  }
})

test_that("min_denominator explains a zero charge as not having played", {
  # A full squad leaves no shortfall, so an absent core player must not be told
  # they are covering one: the charge is right either way, the sentence is not.
  result <- md_charge(squad_of(10))
  absent <- result$explanation[!big_roster$player %in% squad_of(10)]

  expect_true(all(absent == "Did not play: players were charged only when present."))
  expect_false(any(grepl("Small squad", absent)))
})

test_that("min_denominator floors the denominator below the threshold", {
  # Five play, so the fee is divided by 8 rather than 5: each pays what they
  # would in a full squad, and the shortfall falls on the absent core.
  squad <- c("C1", "C2", "C3", "G1", "G2")
  result <- md_charge(squad)
  share <- 76 / 8

  expect_equal(charge_for(result, "C1"), share)
  expect_equal(charge_for(result, "G1"), share)   # guests pay the floor too
  expect_equal(charge_for(result, "G3"), 0)       # absent guest pays nothing

  shortfall <- 76 - 5 * share
  expect_equal(charge_for(result, "C4"), shortfall / 9)
  expect_equal(sum(result$charge), 76)
})

test_that("min_denominator never charges an attendee more for a thinner squad", {
  cost <- vapply(4:12, function(n) charge_for(md_charge(squad_of(n)), "C1"), numeric(1))

  expect_true(all(diff(cost) <= 0))
  # Flat at the full-squad rate all the way down to the thinnest turnout.
  expect_equal(cost[1:5], rep(76 / 8, 5))
})

test_that("min_denominator caps what an absent core player can be charged", {
  levy <- vapply(0:7, function(n) charge_for(md_charge(squad_of(n)), "C12"), numeric(1))

  # An absentee is never asked for more than a present player's share.
  expect_true(all(levy <= 76 / 8))
  expect_equal(charge_for(md_charge(squad_of(8)), "C12"), 0)
})

test_that("min_denominator falls back to an even split with the whole core present", {
  # Nobody absent to carry a shortfall, so flooring the denominator would just
  # under-collect; the split is by the four who played instead.
  roster <- test_roster(c("Ada", "Bea", "Cy", "Dee"))
  result <- md_charge(roster$player, roster)

  expect_equal(charge_for(result, "Ada"), 76 / 4)
  expect_equal(sum(result$charge), 76)
})

test_that("every rule set returns one clean row per rostered player", {
  squad <- c("C1", "G1")
  for (name in names(FEE_RULE_SETS)) {
    rules <- FEE_RULE_SETS[[name]]
    result <- rules$charge(squad, big_roster, rules$params)
    expect_setequal(result$player, big_roster$player)
    expect_false(any(is.na(result$charge)), label = name)
    expect_false(any(is.na(result$explanation)), label = name)
  }
})

# The engine rather than a rule called by hand: two matches synced into a
# season 3 style season, with attendance typed for the first only.
FIRST <- as.Date("2026-10-28")
SECOND <- as.Date("2026-11-04")
s3 <- tibble(season_id = "s3", fee_rules = "min_denominator")
synced <- tibble(season_id = "s3", date = c(FIRST, SECOND))
on_roster <- big_roster |> mutate(season_id = "s3", active = TRUE)
present_at <- function(date, squad) tibble(season_id = "s3", date = date, player = squad)

test_that("a match with no attendance rows is not charged until it has some", {
  # Priced on an empty squad, the second match would put the whole fee on the
  # absent core while its attendance waits to be typed.
  charges <- season_charges(s3, synced, present_at(FIRST, squad_of(5)), on_roster)

  expect_false(SECOND %in% charges$date)
  expect_equal(charges$charge, md_charge(squad_of(5))$charge)
  expect_equal(charges$explanation, md_charge(squad_of(5))$explanation)
  expect_equal(unique(charges$squad_size), 5)

  # With nothing typed yet the season has nothing to charge, in the shape the
  # other seasons bind to.
  waiting <- season_charges(s3, synced, present_at(FIRST, character()), on_roster)
  expect_identical(waiting, empty_charges())
})

test_that("typing the attendance prices the waiting match as the rules say", {
  attendance <- bind_rows(present_at(FIRST, squad_of(5)), present_at(SECOND, squad_of(9)))
  charges <- season_charges(s3, synced, attendance, on_roster)
  first <- charges |> filter(date == FIRST)
  second <- charges |> filter(date == SECOND)

  expect_equal(second$charge, md_charge(squad_of(9))$charge)
  expect_equal(second$explanation, md_charge(squad_of(9))$explanation)
  expect_equal(sum(second$charge), 76)
  expect_equal(first$charge, md_charge(squad_of(5))$charge)
})

test_that("an archived season is read back whole, attendance or not", {
  # A frozen ledger is the record, so holding back an unpriced match must not
  # thin it out, even with no attendance on file for any of its dates.
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "data", "archive"), recursive = TRUE)
  ledger <- tibble(
    season_id = "s1", date = c("14/01/2026", "21/01/2026"), player = "Ben",
    played = TRUE, squad_size = 8L, charge = c(9.50, 9.25), explanation = "Played."
  )
  write_csv(ledger, file.path(dir, "data", "archive", "charges_s1.csv"))
  withr::local_dir(dir)

  archived <- tibble(season_id = "s1", fee_rules = "archived")
  matches <- tibble(season_id = "s1", date = parse_match_date(ledger$date))
  charges <- season_charges(archived, matches, present_at(FIRST, character()), on_roster)

  expect_equal(charges$date, parse_match_date(ledger$date))
  expect_equal(charges$charge, ledger$charge)
})

test_that("every balance on file is unchanged by holding back unattended matches", {
  # Priced the way the engine did before: the rules called on every match, an
  # empty squad where nobody is recorded, and each ledger read straight off disk.
  # Every match on file has its attendance today, so the two must agree in full.
  # Right after a sync, the match still waiting is the one difference allowed.
  withr::local_dir(here::here())
  data <- suppressWarnings(load_app_data())

  priced_before <- function(season) {
    if (season$fee_rules == "archived") {
      path <- file.path("data", "archive", paste0("charges_", season$season_id, ".csv"))
      ledger <- read_csv(path, col_types = LEDGER_COLUMNS)
      return(transmute(ledger, season_id, date = parse_match_date(date), player, charge))
    }
    rules <- fee_rule_set(season$fee_rules)
    roster <- data$rosters |> filter(season_id == season$season_id, active)
    dates <- data$matches$date[data$matches$season_id %in% season$season_id]
    map_dfr(as.list(dates), function(date) {
      squad <- data$attendance$player[data$attendance$date == date]
      rules$charge(squad, roster, rules$params) |>
        transmute(season_id = season$season_id, date = date, player, charge)
    })
  }
  before <- map_dfr(split(data$seasons, data$seasons$season_id), priced_before)

  priced <- data$seasons$season_id[data$seasons$fee_rules != "archived"]
  awaiting <- data$matches |>
    filter(season_id %in% priced) |>
    anti_join(data$attendance, by = "date")
  expected <- before |> filter(!date %in% awaiting$date)

  balances <- all_player_balances(data$charges, data$payments)
  expected_balances <- all_player_balances(expected, data$payments)

  expect_equal(nrow(data$charges), nrow(expected))
  expect_equal(balances, expected_balances)
  expect_equal(balance_totals(balances), balance_totals(expected_balances))
})
