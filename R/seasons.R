# Season definitions ----
# A season is defined by its start date alone: it runs until the day before the
# next season begins, and the last one is open-ended. Defining them this way
# means there is no gap to fall into, so off-season activity (a settle-up
# payment made in August, say) still lands in the season it belongs to.
#
# Every match, attendance row and payment is assigned to a season by its date,
# so adding a fixture never means editing a season column by hand. `fee_rules`
# names the rule set in R/fees.R that priced the season; the sentinel "archived"
# means charges are read from a frozen ledger in data/archive/ rather than
# recomputed, which is how a season stays reproducible after its rules retire.

load_seasons <- function(path = "data/seasons.csv") {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(start_date = parse_match_date(start_date)) %>%
    arrange(start_date) %>%
    mutate(end_date = coalesce(lead(start_date) - 1, as.Date("9999-12-31")))
}

#' Label each date with the season it falls in (NA if it falls in no season).
assign_season <- function(dates, seasons) {
  dates <- as.Date(dates)
  matched <- vapply(dates, function(date) {
    hit <- which(date >= seasons$start_date & date <= seasons$end_date)
    if (length(hit) == 0) NA_character_ else seasons$season_id[[hit[[1]]]]
  }, character(1))
  matched
}

#' Add a `season_id` column to any table carrying a `date` column.
with_season <- function(data, seasons) {
  data %>% mutate(season_id = assign_season(.data$date, seasons))
}

#' Filter a season-tagged table to any set of seasons.
#'
#' Takes a character vector, so one season, several, or all of them are the same
#' operation. An empty selection yields no rows, which is what the caller wants:
#' nothing is selected, so nothing is in scope.
filter_season <- function(data, season_ids) {
  data %>% filter(.data$season_id %in% .env$season_ids)
}

#' Choices for the season picker, newest first.
season_choices <- function(seasons) {
  seasons %>%
    arrange(desc(start_date)) %>%
    { setNames(.$season_id, .$label) }
}

#' When a season actually ran, as a short caption for the picker.
#'
#' Seasons here are a few months long and irregular, so the useful caption is
#' the span of matches actually played rather than the nominal boundaries. A
#' season with no matches yet is described by its start date instead.
season_span <- function(seasons, matches) {
  month <- function(date) format(date, "%b")
  month_year <- function(date) format(date, "%b %Y")

  played <- matches %>%
    filter(!is.na(season_id)) %>%
    group_by(season_id) %>%
    summarise(first_match = min(date), last_match = max(date), .groups = "drop")

  seasons %>%
    left_join(played, by = "season_id") %>%
    mutate(
      span = case_when(
        is.na(first_match) ~ paste("from", month_year(start_date)),
        month_year(first_match) == month_year(last_match) ~ month_year(first_match),
        # Within one calendar year the year only needs saying once.
        format(first_match, "%Y") == format(last_match, "%Y") ~
          paste0(month(first_match), "\u2013", month_year(last_match)),
        TRUE ~ paste(month_year(first_match), "\u2013", month_year(last_match))
      )
    ) %>%
    select(season_id, label, start_date, span)
}
