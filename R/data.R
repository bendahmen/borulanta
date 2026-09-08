# Data loading ----
#
# Read every CSV once at startup, parse dates, tag each row with its season and
# check the obvious ways the files can drift out of step with each other. Loaded
# at app level rather than inside server(), so the work is done once per process
# rather than once per visitor.

load_app_data <- function(dir = "data") {
  path <- function(...) file.path(dir, ...)

  seasons <- load_seasons(path("seasons.csv"))
  players <- read_csv(path("players.csv"), show_col_types = FALSE)
  rosters <- read_csv(path("player_seasons.csv"), show_col_types = FALSE)

  matches <- read_csv(path("matches.csv"), show_col_types = FALSE) %>%
    mutate(date = parse_match_date(date)) %>%
    with_season(seasons) %>%
    arrange(desc(date))

  attendance <- read_csv(path("attendance.csv"), show_col_types = FALSE) %>%
    mutate(date = parse_match_date(date)) %>%
    with_season(seasons)

  payments <- read_csv(path("payments.csv"), show_col_types = FALSE) %>%
    mutate(date = parse_match_date(date)) %>%
    with_season(seasons)

  data <- list(
    seasons = seasons,
    players = players,
    rosters = rosters,
    matches = matches,
    attendance = attendance,
    payments = payments
  )

  validate_app_data(data)
  data$charges <- all_charges(seasons, matches, attendance, rosters)
  data
}

#' Warn loudly about data that will silently produce wrong numbers.
validate_app_data <- function(data) {
  complain <- function(problem, offenders) {
    if (length(offenders) > 0) {
      warning(problem, ": ", paste(unique(offenders), collapse = ", "), call. = FALSE)
    }
  }

  # Anything dated before the first season has nowhere to go.
  for (table in c("matches", "attendance", "payments")) {
    complain(
      paste(table, "dated before the first season in seasons.csv"),
      format(data[[table]]$date[is.na(data[[table]]$season_id)], "%d/%m/%Y")
    )
  }
  complain(
    "attendance for a date with no match",
    format(setdiff(data$attendance$date, data$matches$date), "%d/%m/%Y")
  )
  complain(
    "attendance for an unknown player",
    setdiff(data$attendance$player, data$players$player)
  )
  complain(
    "payments from an unknown player",
    setdiff(data$payments$player, data$players$player)
  )
  complain(
    "roster entries for an unknown player",
    setdiff(data$rosters$player, data$players$player)
  )
  complain(
    "roster entries for an unknown season",
    setdiff(data$rosters$season_id, data$seasons$season_id)
  )

  # Someone who played but was not on that season's active roster is charged
  # nothing, which is almost always a missing roster row rather than a freebie.
  unrostered <- data$attendance %>%
    anti_join(
      data$rosters %>% filter(active),
      by = c("season_id", "player")
    ) %>%
    pull(player)
  complain("players in attendance but not on that season's active roster", unrostered)

  invisible(data)
}
