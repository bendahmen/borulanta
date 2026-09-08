# Data loading ----
#
# Read every CSV once at startup, parse dates, tag each row with its season and
# check the obvious ways the files can drift out of step with each other. Loaded
# at app level rather than inside server(), so the work is done once per process
# rather than once per visitor.

# Column types ----
#
# Pinned rather than guessed, and shared with the sync so the two never disagree
# about a file: an all-digit fixture id guesses as a number and stops being a
# string, and an empty match_events.csv guesses every column as character,
# minute included.

MATCH_COLUMNS <- readr::cols(
  date = readr::col_character(),
  opponent = readr::col_character(),
  goals_for = readr::col_integer(),
  goals_against = readr::col_integer(),
  dl_match_id = readr::col_character()
)

EVENT_COLUMNS <- readr::cols(
  date = readr::col_character(),
  dl_match_id = readr::col_character(),
  team = readr::col_character(),
  minute = readr::col_integer(),
  event_type = readr::col_character(),
  player = readr::col_character()
)

read_match_file <- function(path) {
  readr::read_csv(path, col_types = MATCH_COLUMNS) %>%
    mutate(date = parse_match_date(date))
}

read_event_file <- function(path) {
  readr::read_csv(path, col_types = EVENT_COLUMNS) %>%
    mutate(date = parse_match_date(date))
}

#' Add the "4-5" display string derived from the two goal columns.
#'
#' Scores are stored as two integers because that is what they are and what the
#' sync writes; the hyphenated form only ever exists to be printed, so it is
#' derived once here rather than parsed back out of a string wherever it is
#' shown.
with_result <- function(matches) {
  matches %>% mutate(result = paste0(goals_for, "-", goals_against))
}

load_app_data <- function(dir = "data") {
  path <- function(...) file.path(dir, ...)

  seasons <- load_seasons(path("seasons.csv"))
  players <- read_csv(path("players.csv"), show_col_types = FALSE)
  rosters <- read_csv(path("player_seasons.csv"), show_col_types = FALSE)

  matches <- read_match_file(path("matches.csv")) %>%
    with_result() %>%
    with_season(seasons) %>%
    arrange(desc(date))

  events <- read_event_file(path("match_events.csv")) %>%
    with_season(seasons)

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
    events = events,
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
  complain(
    "match events for a date with no match",
    format(setdiff(data$events$date, data$matches$date), "%d/%m/%Y")
  )
  complain(
    "match events for an unknown player",
    setdiff(na.omit(data$events$player), data$players$player)
  )

  # A goal tally that disagrees with the scoreline means the event log is
  # partial, and a partial log looks exactly like a complete one to anything
  # that counts it. Say so rather than let it be totted up as fact.
  goal_tally <- data$events %>%
    filter(event_type == "goal") %>%
    count(date, team) %>%
    tidyr::pivot_wider(names_from = team, values_from = n, values_fill = 0L)
  for (side in c("us", "them")) {
    if (!side %in% names(goal_tally)) goal_tally[[side]] <- 0L
  }
  mismatched <- data$matches %>%
    inner_join(goal_tally, by = "date") %>%
    filter(us != goals_for | them != goals_against)
  complain(
    "goal events that do not add up to the scoreline",
    format(mismatched$date, "%d/%m/%Y")
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
