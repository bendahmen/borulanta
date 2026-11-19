# Formatting helpers ----
#
# Small shared bits of presentation: money, dates, the result badge and the
# wording that changes with the estimator. Kept apart from the cards and tables
# that use them because they carry no layout of their own.


# The theme's colours, for the plots and the bslib theme to share. The
# stylesheet keeps its own oklch copies in :root, which R cannot reach.
APP_COLOURS <- c(
  blue = "#0850AB", # oklch(45% 0.16 258)
  amber = "#AB4400", # oklch(52% 0.15 45)
  green = "#006D1E", # oklch(46% 0.15 148)
  ink = "#16241f",
  background = "#f7f9f6",
  rule = "#8a938d",
  faint = "#dfe3e0"
)


format_fee_amount <- function(amount) {
  sign <- if (amount < 0) "-" else ""
  paste0(sign, "£", money(abs(amount)))
}


# Display strings for the site ----
#
# R formats values and the site writes the words around them, so each figure the site
# shows as rounded has its string made here, and the site never rounds. Each takes a
# vector and keeps NA as NA.

#' Goals per game and points per match, to two places as R prints them: 0.5, not 0.50.
format_rate <- function(rate) {
  if_else(is.na(rate), NA_character_, as.character(round(rate, 2)))
}

#' The with-and-without gap, unsigned and always to two places, as in "0.40 points a match".
format_gap <- function(gap) {
  # formatC() rather than format(), which pads a vector to its widest member.
  if_else(is.na(gap), NA_character_, formatC(round(abs(gap), 2), format = "f", digits = 2))
}

#' A player's turnout as a whole percentage, with R's rounding: 12.5% shows as 12%.
format_turnout <- function(rate) {
  if_else(is.na(rate), NA_character_, paste0(round(rate * 100), "%"))
}


# Home page cards ----

#' 1st, 2nd, 3rd, 4th, ... 11th, 12th, 13th, 21st.
ordinal <- function(n) {
  tens <- n %% 100
  suffix <- if (tens %in% 11:13) {
    "th"
  } else {
    switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
  }
  paste0(n, suffix)
}


#' "Wed 19 Aug 2026" — the weekday earns its place on a game that is always
#' on a Wednesday, because a fixture that is not is worth noticing.
format_match_date <- function(date) format(date, "%a %d %b %Y")


result_badge <- function(points) {
  label <- case_when(points == 3 ~ "WIN", points == 1 ~ "DRAW", TRUE ~ "LOSS")
  class <- case_when(points == 3 ~ "win", points == 1 ~ "draw", TRUE ~ "loss")
  span(class = paste("result-badge", class), label)
}


#' A comma list that reads like a sentence, with a tally where there is one.
listed_scorers <- function(scorers) {
  paste(
    if_else(scorers$goals > 1, paste0(scorers$player, " (", scorers$goals, ")"), scorers$player),
    collapse = ", "
  )
}


#' What a coefficient means, which is not the same under both estimators.
#'
#' The OLS fit has no intercept, so a coefficient is a player's additive share
#' of the scoreline. The ridge fit has one, so a coefficient is their deviation
#' from the average player. Putting both under one heading would be wrong about
#' one of them.
estimate_label <- function(estimator) {
  if (estimator == "ridge") {
    "Deviation from the average player, shrunk by how little we know."
  } else {
    "Coefficient on each outcome, with its p-value in brackets."
  }
}
