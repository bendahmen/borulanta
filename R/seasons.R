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

ALL_SEASONS <- "all"

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

#' Filter a season-tagged table. `ALL_SEASONS` keeps everything.
filter_season <- function(data, season_id) {
  if (identical(season_id, ALL_SEASONS)) {
    return(data)
  }
  data %>% filter(.data$season_id %in% .env$season_id)
}

#' Choices for a season picker: newest season first, then an all-time option.
season_choices <- function(seasons, include_all = TRUE) {
  choices <- seasons %>%
    arrange(desc(start_date)) %>%
    { setNames(.$season_id, .$label) }

  if (include_all) {
    choices <- c(choices, setNames(ALL_SEASONS, "All seasons"))
  }
  choices
}

current_season <- function(seasons, today = Sys.Date()) {
  open <- seasons %>% filter(start_date <= today, end_date >= today)
  if (nrow(open) > 0) {
    return(open$season_id[[nrow(open)]])
  }
  # Between seasons: the one about to start, else the most recent.
  upcoming <- seasons %>% filter(start_date > today) %>% arrange(start_date)
  if (nrow(upcoming) > 0) upcoming$season_id[[1]] else seasons$season_id[[nrow(seasons)]]
}
