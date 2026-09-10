# The per-player card answers "what do I owe". This is the other question, the
# one the person collecting the money has, and the arithmetic underneath it has
# to agree with the per-player view or the two contradict each other on the same
# screen.

charge_row <- function(player, amount, date = "2026-01-14") {
  tibble(
    season_id = "s1", date = as.Date(date), player = player, played = TRUE,
    squad_size = 8L, charge = amount, explanation = "", result = "2-1"
  )
}

payment_row <- function(player, amount, date = "2026-01-21") {
  tibble(date = as.Date(date), player = player, amount = amount, season_id = "s1")
}

test_that("a balance is what was charged less what was paid", {
  balances <- all_player_balances(
    bind_rows(charge_row("Ben", 9.50), charge_row("Ben", 9.50)),
    payment_row("Ben", 10)
  )

  expect_equal(balances$charges, 19)
  expect_equal(balances$payments, 10)
  expect_equal(balances$balance, 9)
})

test_that("it agrees with the per-player view it sits next to", {
  charges <- bind_rows(charge_row("Ben", 9.51), charge_row("Vitto", 9.49))
  payments <- payment_row("Ben", 4)

  balances <- all_player_balances(charges, payments)
  overview <- player_fee_overview("Ben", charges, payments)

  ben <- balances %>% filter(player == "Ben")
  expect_equal(ben$balance, overview$balance)
  expect_equal(ben$charges, overview$total_charges)
})

test_that("someone who has paid but never been charged still appears", {
  # A player who left, settled, and came off the roster. Dropping them would
  # quietly change the totals.
  balances <- all_player_balances(charge_row("Ben", 9.50), payment_row("Ghost", 20))

  ghost <- balances %>% filter(player == "Ghost")
  expect_equal(ghost$charges, 0)
  expect_equal(ghost$balance, -20)
})

test_that("someone charged who has never paid appears with no payment date", {
  balances <- all_player_balances(charge_row("Ben", 9.50), payment_row("Vitto", 5))

  ben <- balances %>% filter(player == "Ben")
  expect_equal(ben$payments, 0)
  expect_true(is.na(ben$last_payment))
})

test_that("the last payment is the most recent one, not the last row", {
  payments <- bind_rows(
    payment_row("Ben", 5, "2026-03-01"),
    payment_row("Ben", 5, "2026-01-05")
  )

  balances <- all_player_balances(charge_row("Ben", 20), payments)

  expect_equal(balances$last_payment, as.Date("2026-03-01"))
})

test_that("the biggest debt is at the top", {
  charges <- bind_rows(charge_row("Ben", 5), charge_row("Vitto", 50))
  balances <- all_player_balances(charges, payment_row("Ben", 0))

  expect_equal(balances$player[[1]], "Vitto")
})

test_that("outstanding and credit are reported apart, not netted", {
  # One person owing 30 and another 30 in credit is not a settled pot: the
  # credit cannot be used to chase the debt.
  charges <- bind_rows(charge_row("Ben", 30), charge_row("Vitto", 0))
  balances <- all_player_balances(charges, payment_row("Vitto", 30))

  totals <- balance_totals(balances)

  expect_equal(totals$outstanding, 30)
  expect_equal(totals$credit, 30)
  expect_equal(totals$net, 0)
  expect_equal(totals$settled, 0)
})

test_that("a pot where everybody is square reports as such", {
  balances <- all_player_balances(charge_row("Ben", 12), payment_row("Ben", 12))

  totals <- balance_totals(balances)

  expect_equal(totals$outstanding, 0)
  expect_equal(totals$credit, 0)
  expect_equal(totals$settled, 1)
})
