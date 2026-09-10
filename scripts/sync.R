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
league_table <- parse_league_table(html)
cat("Parsed", nrow(all_fixtures), "league fixtures,",
    nrow(fixtures), "of them ours, and a", nrow(league_table), "team table.\n")

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
describe(
  paste(
    "Man of the match not recorded — they did not score, so the page does not",
    "say whose it was. Add it to data/match_events.csv by hand if it was ours"
  ),
  report$unattributed_mom, c("date", "opponent", "player")
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

# The snapshot files first, and on their own terms. They are not part of the
# record the reconciliation above is protecting: they describe the current
# league season only, so they are replaced wholesale every run. That is also
# why they are written even when no result changed — the standings move when
# any team plays, not just when we do.
snapshot <- snapshot_tables(fixtures, league_table,
                            scraped_on = Sys.Date(), recorded = result$matches$date)
upcoming <- snapshot$fixtures %>% filter(date >= Sys.Date())

rule("Fixtures and league table")
cat(nrow(fixtures), " fixtures of ours on the page, ",
    nrow(upcoming), " still to play.\n", sep = "")
if (nrow(upcoming) > 0) {
  cat("Next: ", format(upcoming$date[[1]], "%d/%m/%Y"), " v ",
      coalesce(upcoming$opponent[[1]], "?"), "\n", sep = "")
}
if (!OUR_TEAM %in% league_table$team) {
  cat("NOTE: ", OUR_TEAM, " is not in the league table on this page.\n", sep = "")
} else {
  us <- league_table %>% filter(team == OUR_TEAM)
  cat("League position: ", us$position, " of ", nrow(league_table),
      " on ", us$points, " points.\n", sep = "")
}

if (write_changes) {
  write_snapshot_files(snapshot, "data/fixtures.csv", "data/league_table.csv")
  cat("data/fixtures.csv and data/league_table.csv replaced.\n")
} else {
  cat("Dry run — re-run with --write to replace them.\n")
}

# The league's results, ours and everyone else's, kept across seasons so an
# opponent's strength can be read off its record against the rest of the
# league. The page wins for any fixture it lists; fixtures it no longer lists
# are kept.
league <- sync_league_results(all_fixtures, read_league_result_file("data/league_results.csv"))

rule("League results")
describe("New league results", league$added,
         c("date", "home_team", "home_goals", "away_goals", "away_team"))
describe("Corrected on the site", league$corrected,
         c("date", "home_team", "old_home", "old_away", "home_goals", "away_goals", "away_team"))
if (!league$changed) {
  cat("Every played fixture on the page is already recorded.\n")
} else if (write_changes) {
  write_league_result_file(league$league_results, "data/league_results.csv")
  cat("data/league_results.csv updated: ", nrow(league$league_results),
      " results on file.\n", sep = "")
} else {
  cat("Dry run — re-run with --write to record them.\n")
}

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
