# The tables are the layer where a row can go missing without anything on the
# page saying so, which is what these are for. A DT built with dom = "t" draws
# no pager, so its page length is the only thing standing between the data and
# a silent truncation — and DT's default of 10 is shorter than several of these
# tables routinely are.

page_length <- function(widget) widget$x$options$pageLength

column_names <- function(widget) names(widget$x$data)

test_that("every_row covers the data it is given", {
  expect_equal(every_row(tibble(a = 1:45)), 45L)
  expect_equal(every_row(tibble(a = 1:9)), 9L)
})

test_that("every_row never returns zero", {
  # A page length is a count of rows to draw, and an empty table still has its
  # "nothing here" row to draw.
  expect_equal(every_row(tibble(a = integer())), 1L)
})

# The tables that draw no pager. Each of these once relied on a page length,
# and two of them were relying on DT's default of 10.
test_that("pagerless tables show every row they are given", {
  lineup <- tibble(
    player = paste0("P", 1:13),
    season_appearances = 1:13,
    attendance_rate = rep(0.5, 13)
  )
  expect_equal(page_length(match_lineup_table(lineup)), 13L)

  payments <- tibble(date = as.Date("2026-01-01") + 1:14, amount = 1:14)
  expect_equal(page_length(payment_history_table(payments)), 14L)

  matches <- with_result(tibble(
    date = as.Date("2026-01-01") + 1:45,
    opponent = "Someone",
    goals_for = 2L, goals_against = 1L,
    dl_match_id = "x"
  ))
  expect_equal(page_length(match_table(matches)), 45L)
})

# A squad of eleven is the case that used to lose a row: DT's default page
# length is 10, and the biggest squad on file is exactly 10.
test_that("an eleventh player is not dropped from the lineup", {
  lineup <- tibble(
    player = paste0("P", 1:11),
    season_appearances = 1:11,
    attendance_rate = rep(0.5, 11)
  )
  expect_gte(page_length(match_lineup_table(lineup)), 11L)
})

test_that("the display naming happens in the tables and not upstream", {
  charges <- tibble(
    date = as.Date("2026-01-01"), season = "Season 3", result = "2-1",
    played = TRUE, squad_size = 9L, charge = 8.5, explanation = "even split"
  )
  expect_true("squad_size" %in% names(charges))
  expect_true("Squad size" %in% column_names(match_charge_table(charges)))

  attendance_list <- tibble(
    player = "Ben", attendance_rate = 0.8,
    avg_points = 1.5, avg_goals_scored = 2, avg_goals_conceded = 1.5
  )
  expect_equal(
    column_names(attendance_table(attendance_list)),
    c("Player", "Turnout", "Avg points", "Avg goals scored", "Avg goals conceded")
  )
})
