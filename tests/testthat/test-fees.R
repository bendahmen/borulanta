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
