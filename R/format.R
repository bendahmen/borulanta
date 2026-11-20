# Formatting helpers ----
#
# format_fee_amount() writes money as the site shows it, with the sign before the
# pound sign.


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
