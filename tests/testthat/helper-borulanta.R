suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(rvest)
  library(ggplot2)
  # The presentation layer below needs these: R/cards.R builds bslib cards at
  # load time, and R/tables.R returns DT widgets.
  library(shiny)
  library(bslib)
  library(DT)
})

# Same order as app.R, and for the same reasons: R/fees.R defines
# parse_match_date(), which R/seasons.R needs at load time, and the
# presentation files read constants out of the data layer.
for (file in c(
  "fees.R", "seasons.R", "analysis.R", "data.R", "scrape.R", "sync.R",
  "format.R", "plots.R", "tables.R", "cards.R"
)) {
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
