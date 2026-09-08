#!/usr/bin/env Rscript

# Pull this week's results off the league page into data/.
#
#   Rscript scripts/sync.R                 # dry run: say what would change
#   Rscript scripts/sync.R --write         # actually change it
#   Rscript scripts/sync.R --from page.html --write   # parse a saved page
#
# Exits 1 if the page could not be trusted, so a scheduled run fails loudly
# rather than reporting success over an empty scrape.
#
# Attendance is not on the site and is not touched here: after a sync, the
# week's rows in data/attendance.csv are still yours to add.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(rvest)
})

source("R/fees.R") # parse_match_date(), which seasons.R needs at load time
source("R/seasons.R")
source("R/data.R") # the file readers, shared with the app so types cannot drift
source("R/scrape.R")
source("R/sync.R")

args <- commandArgs(trailingOnly = TRUE)
write_changes <- "--write" %in% args
from_file <- if ("--from" %in% args) args[[match("--from", args) + 1]] else NULL

SNAPSHOT_DIR <- "raw"

# Reporting ----

rule <- function(title) cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")

describe <- function(label, rows, columns) {
  if (nrow(rows) == 0) {
    return(invisible(NULL))
  }
  rule(paste0(label, " (", nrow(rows), ")"))
  print(as.data.frame(rows %>%
    mutate(date = format(date, "%d/%m/%Y")) %>%
    select(any_of(columns))), row.names = FALSE)
}

# Fetch and parse ----

html <- if (is.null(from_file)) {
  snapshot <- file.path(SNAPSHOT_DIR, paste0(Sys.Date(), ".html"))
  cat("Fetching", DREAMLEAGUES_URL, "\n")
  fetch_league_page(DREAMLEAGUES_URL, snapshot_path = snapshot)
} else {
  cat("Reading", from_file, "\n")
  paste(readLines(from_file, warn = FALSE), collapse = "\n")
}

all_fixtures <- parse_fixtures(html)
fixtures <- our_fixtures(all_fixtures)
cat("Parsed", nrow(all_fixtures), "league fixtures,",
    nrow(fixtures), "of them ours.\n")

# A page that parses to nothing is a changed page, not an empty league.
if (nrow(all_fixtures) == 0) {
  cat("\nFAILED: no fixtures at all in the page. The markup has changed.\n")
  quit(status = 1)
}

# Reconcile ----

seasons <- load_seasons("data/seasons.csv")

matches <- read_match_file("data/matches.csv")
events <- read_event_file("data/match_events.csv")
name_map <- read_csv("data/name_map.csv", show_col_types = FALSE)
players <- read_csv("data/players.csv", show_col_types = FALSE)

result <- sync_results(fixtures, matches, events, seasons, name_map)

if (!is.null(result$abort)) {
  cat("\nFAILED:", result$abort, "\n")
  cat("Nothing was written.\n")
  quit(status = 1)
}

report <- result$report
describe("New results", report$new, c("date", "opponent", "goals_for", "goals_against"))
describe("Backfilled onto existing rows", report$enriched, c("date", "opponent"))
describe(
  "SCORE CONFLICTS — not written, resolve by hand", report$conflicting,
  c("date", "opponent", "existing_for", "existing_against", "goals_for", "goals_against")
)
describe(
  "Awaiting a scoreline — 0-0 with no goals logged, so unplayed, cancelled, or a real goalless draw",
  report$pending, c("date", "opponent")
)
describe(
  "Refused — outside every season, or in a season already closed",
  report$refused, c("date", "opponent", "season_id", "disposition")
)
describe(
  "Goals logged do not add up to the score — the event list is partial",
  report$miscounted, c("date", "opponent", "goals_for", "goals_against", "goal_events")
)

# A scorer of ours who is neither a player we know nor a name we translate.
# Most of the squad go by the same first name on the site, so only the ones that
# do not are worth mentioning.
unrecognised <- result$events %>%
  filter(team == "us", !is.na(player)) %>%
  distinct(player) %>%
  pull(player) %>%
  setdiff(players$player)
if (length(unrecognised) > 0) {
  rule("Scorers we do not recognise")
  cat("Map these to a player in data/name_map.csv and re-run:\n  ",
      paste(unrecognised, collapse = ", "), "\n", sep = "")
}

# Write ----

outstanding <- nrow(report$conflicting) + nrow(report$pending)

if (!result$changed) {
  rule("Nothing to do")
  if (outstanding > 0) {
    cat("Nothing could be written without a decision from you: ",
        outstanding, " fixture(s) above are waiting on one.\n", sep = "")
  } else {
    cat("Everything on the page is already recorded.\n")
  }
} else if (!write_changes) {
  rule("Dry run")
  cat("Re-run with --write to apply this.\n")
} else {
  write_sync_files(result, "data/matches.csv", "data/match_events.csv")
  rule("Written")
  cat("data/matches.csv and data/match_events.csv updated.\n")
  if (nrow(report$new) > 0) {
    cat("Attendance for", paste(format(report$new$date, "%d/%m/%Y"), collapse = ", "),
        "still needs adding by hand.\n")
  }
}
