suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(rvest)
})

# R/fees.R defines parse_match_date(), which R/seasons.R needs at load time.
for (file in c("fees.R", "seasons.R", "analysis.R", "data.R", "scrape.R", "sync.R")) {
  source(here::here("R", file))
}

fixture_html <- function(name) {
  paste(
    readLines(here::here("tests", "testthat", "fixtures", name), warn = FALSE),
    collapse = "\n"
  )
}

# Zero-row tables in the shape the loaders return them, for the cases where a
# function has to cope with a file that has nothing in it yet.
empty_events <- function() {
  tibble(
    date = as.Date(character()), dl_match_id = character(), team = character(),
    minute = integer(), event_type = character(), player = character()
  )
}

empty_standings <- function() {
  tibble(
    position = integer(), team = character(), played = integer(), won = integer(),
    drawn = integer(), lost = integer(), goals_for = integer(),
    goals_against = integer(), goal_difference = integer(), points = integer()
  )
}
