# Data loading ----
#
# Read every CSV once at startup, parse dates, tag each row with its season and
# check the obvious ways the files can drift out of step with each other. Loaded
# at app level rather than inside server(), so the work is done once per process
# rather than once per visitor.

# Who we are on the league site, and so which row of the league table is ours.
# It lives here rather than in R/scrape.R because the app reads it and never
# loads the scraper — sourcing that would pull rvest and httr into a process
# that only ever reads CSVs.
OUR_TEAM <- "Borulanta"

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

# The two snapshot files the sync replaces wholesale each run. Neither is part
# of the permanent record, and neither exists on a fresh clone until the first
# sync — so both are read tolerantly and both come back empty rather than
# stopping the app from starting.

FIXTURE_COLUMNS <- readr::cols(
  date = readr::col_character(),
  opponent = readr::col_character(),
  dl_match_id = readr::col_character()
)

LEAGUE_TABLE_COLUMNS <- readr::cols(
  position = readr::col_integer(),
  team = readr::col_character(),
  played = readr::col_integer(),
  won = readr::col_integer(),
  drawn = readr::col_integer(),
  lost = readr::col_integer(),
  goals_for = readr::col_integer(),
  goals_against = readr::col_integer(),
  goal_difference = readr::col_integer(),
  points = readr::col_integer(),
  scraped_on = readr::col_character()
)

# Every league result the sync has seen, ours included, accumulated across
# seasons. History rather than a snapshot, but read tolerantly for the same
# reason as the two above: it does not exist until the first sync writes it.

LEAGUE_RESULT_COLUMNS <- readr::cols(
  date = readr::col_character(),
  home_team = readr::col_character(),
  away_team = readr::col_character(),
  home_goals = readr::col_integer(),
  away_goals = readr::col_integer(),
  dl_match_id = readr::col_character()
)

read_match_file <- function(path) {
  readr::read_csv(path, col_types = MATCH_COLUMNS) %>%
    mutate(date = parse_match_date(date))
}

read_event_file <- function(path) {
  readr::read_csv(path, col_types = EVENT_COLUMNS) %>%
    mutate(date = parse_match_date(date))
}

read_fixture_file <- function(path) {
  if (!file.exists(path)) {
    return(tibble(
      date = as.Date(character()),
      opponent = character(),
      dl_match_id = character()
    ))
  }
  readr::read_csv(path, col_types = FIXTURE_COLUMNS) %>%
    mutate(date = parse_match_date(date))
}

read_league_table_file <- function(path) {
  if (!file.exists(path)) {
    return(tibble(
      position = integer(), team = character(),
      played = integer(), won = integer(), drawn = integer(), lost = integer(),
      goals_for = integer(), goals_against = integer(),
      goal_difference = integer(), points = integer(),
      scraped_on = as.Date(character())
    ))
  }
  readr::read_csv(path, col_types = LEAGUE_TABLE_COLUMNS) %>%
    mutate(scraped_on = parse_match_date(scraped_on)) %>%
    arrange(position)
}

read_league_result_file <- function(path) {
  if (!file.exists(path)) {
    return(tibble(
      date = as.Date(character()),
      home_team = character(), away_team = character(),
      home_goals = integer(), away_goals = integer(),
      dl_match_id = character()
    ))
  }
  readr::read_csv(path, col_types = LEAGUE_RESULT_COLUMNS) %>%
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

  # Fixtures are season-tagged so the home page can say which season the next
  # match falls in; the league table is not, because its season is the league's
  # own and has nothing to do with the fee seasons the rest of the app counts in.
  fixtures <- read_fixture_file(path("fixtures.csv")) %>%
    with_season(seasons) %>%
    arrange(date)

  league_table <- read_league_table_file(path("league_table.csv"))

  # Season-tagged inside opponent_strength(), which is where the tagging is
  # explained; here it is just the file.
  league_results <- read_league_result_file(path("league_results.csv"))

  data <- list(
    seasons = seasons,
    players = players,
    rosters = rosters,
    matches = matches,
    events = events,
    attendance = attendance,
    payments = payments,
    fixtures = fixtures,
    league_table = league_table,
    league_results = league_results
  )

  data$problems <- validate_app_data(data)
  data$charges <- all_charges(seasons, matches, attendance, rosters)
  data$opponent_strength <- opponent_strength(league_results, seasons)
  data
}

#' Warn loudly about data that will silently produce wrong numbers.
#'
#' The problems are returned as well as warned about. A warning reaches the
#' console or the deployment log, which is exactly where nobody looks; the
#' returned vector is what the app puts on the page.
validate_app_data <- function(data) {
  problems <- character()
  complain <- function(problem, offenders) {
    if (length(offenders) > 0) {
      note <- paste0(problem, ": ", paste(unique(offenders), collapse = ", "))
      problems <<- c(problems, note)
      warning(note, call. = FALSE)
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

  # A fixture whose date already has a result is a snapshot the sync has not
  # caught up with. Harmless to the record, but the home page would offer a
  # played match as the next one, so say so.
  complain(
    "fixtures dated on a match already recorded — the fixture list is stale",
    format(intersect(data$fixtures$date, data$matches$date), "%d/%m/%Y")
  )
  # Not fatal — the table is only context — but if we have dropped out of it
  # the page has changed shape or we have been renamed on the site.
  if (nrow(data$league_table) > 0 && !OUR_TEAM %in% data$league_table$team) {
    complain("missing from data/league_table.csv", OUR_TEAM)
  }

  # The sync keys league results on date and the two teams, so a duplicate can
  # only come from a hand edit. It would count a match twice in the opponent
  # strength index.
  duplicated_results <- data$league_results %>%
    count(date, home_team, away_team) %>%
    filter(n > 1)
  complain(
    "league results recorded more than once",
    format(duplicated_results$date, "%d/%m/%Y")
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

  invisible(problems)
}
