# Writes r-dates.json, which tests/dates.test.js holds site/src/lib/dates.js against: every
# date in data/ as R writes it in each of the six formats the site uses. Run it from the
# project root, which archived_charges() needs, when the formats change or to bring in newer
# dates:
#
#   Rscript site/tests/fixtures/write-r-dates.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
})
for (r_file in c("fees.R", "seasons.R", "analysis.R", "data.R")) source(here::here("R", r_file))

data <- suppressWarnings(load_app_data(here::here("data")))
dates <- c(
  data$matches$date, data$attendance$date, data$payments$date, data$events$date,
  data$fixtures$date, data$league_table$scraped_on, data$league_results$date,
  data$seasons$start_date, data$charges$date
)
dates <- sort(unique(dates[!is.na(dates)]))

formats <- c("%a %d %b %Y", "%d %b %Y", "%d %B %Y", "%d %b", "%b %Y", "%b")
written <- withr::with_locale(c(LC_TIME = "C"), lapply(formats, function(f) format(dates, f)))

fixture <- list(
  formats = formats,
  dates = format(dates, "%Y-%m-%d"),
  written = setNames(written, formats)
)
jsonlite::write_json(
  fixture, here::here("site", "tests", "fixtures", "r-dates.json"),
  auto_unbox = FALSE, pretty = TRUE
)
