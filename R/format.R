# Formatting helpers ----
#
# Small shared bits of presentation: money, dates, the result badge and the
# wording that changes with the estimator. Kept apart from the cards and tables
# that use them because they carry no layout of their own.


format_fee_amount <- function(amount) {
    sign <- if (amount < 0) "-" else ""
    paste0(sign, "£", formatC(abs(amount), format = "f", digits = 2))
}


# Home page cards ----

#' "Wed 19 Aug 2026" — the weekday earns its place on a game that is always
#' on a Wednesday, because a fixture that is not is worth noticing.
format_match_date <- function(date) format(date, "%a %d %b %Y")


# A match week without a sync. The game is weekly and the sync runs after it,
# so a fixture list older than this has missed at least one.
STALE_FIXTURES_DAYS <- 8L


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
