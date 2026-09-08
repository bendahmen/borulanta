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
