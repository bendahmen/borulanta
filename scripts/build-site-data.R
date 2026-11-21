# Export the numbers for the static site ----
#
# R stays the only source of numbers. This reads the CSVs through the functions in
# R/ and writes what the Astro site renders:
#
#   site/src/generated/*.json        read at build time, for pages with no season picker
#   site/public/data/seasons/*.json  one file per season combination, fetched when ticks change
#
# It adds no statistics and no fee arithmetic. It writes raw values, dates as ISO
# strings, and a *_display string beside each figure the site shows rounded or as
# money, made by the formatters in R/. Every word on the page belongs to the site,
# apart from the validate_app_data() messages and the charge explanations, which are
# data. See docs/adr/0003-r-formats-values-site-writes-words.md.
#
#   Rscript scripts/build-site-data.R
#
# Run it from the project root, so that .Rprofile activates renv. A test can
# source() this file and call build_site_data(data_dir, out_root) instead. Keep
# this file ASCII: Rscript parses it in the session's encoding.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
})

# parse() rather than source(). source() re-encodes a file to the session's
# encoding, so under LC_ALL=C every pound sign would be written out as <U+00A3>.
# With an encoding given, parse() takes the bytes as they are and marks them.
source_utf8 <- function(path) {
  eval(parse(path, encoding = "UTF-8", keep.source = FALSE), envir = globalenv())
}

# fees.R first, because seasons.R needs its parse_match_date() at load time. scrape.R
# and sync.R are left out because nothing here syncs.
for (r_file in c("fees.R", "seasons.R", "analysis.R", "data.R", "format.R")) {
  source_utf8(here::here("R", r_file))
}

# Sort order is locale-dependent, and base sort() is what player_regression_results()
# and fee_players use. No date name is written here, so LC_TIME has no say. LC_CTYPE
# stays as it is, because the strings are marked UTF-8 and are written out as bytes.
EXPORT_LOCALE <- c(LC_COLLATE = "C")

# Significant digits, not decimal places: ridge estimates here go as small as
# 6e-53, and rounding to decimal places would write them as 0.
JSON_SIGNIFICANT_DIGITS <- 10L

# Passed to run_in() as today, so that every unplayed fixture comes back and the
# browser does the filtering on the Europe/London date. A static build would
# otherwise freeze the answer on the day it ran.
FAR_PAST <- as.Date("1900-01-01")

#' NA stays NA, which the JSON writes as null.
iso_date <- function(date) format(date, "%Y-%m-%d")

#' format_fee_amount() takes one amount at a time.
fee_strings <- function(amounts) {
  vapply(amounts, format_fee_amount, character(1), USE.NAMES = FALSE)
}

# Season-scoped: one file per combination ----

#' Every season-scoped table, filtered to the ticked seasons.
scope_to_seasons <- function(data, season_ids) {
  list(
    matches = filter_season(data$matches, season_ids),
    attendance = filter_season(data$attendance, season_ids),
    events = filter_season(data$events, season_ids),
    payments = filter_season(data$payments, season_ids),
    charges = filter_season(data$charges, season_ids)
  )
}

#' One player's estimates, in the order the table shows them.
#'
#' The value is for sorting and plotting, and the string is what is displayed:
#' format_estimate() stays the only formatter of an estimate.
regression_rows <- function(results) {
  shown <- player_regression_table(results)

  map(seq_len(nrow(shown)), function(row) {
    player_results <- results |> filter(player == shown$Player[[row]])
    outcomes <- map(REGRESSION_OUTCOMES, function(this_outcome) {
      estimate <- player_results |> filter(outcome == this_outcome)
      list(
        estimate = estimate$estimate,
        std_error = estimate$std_error,
        conf_low = estimate$conf_low,
        conf_high = estimate$conf_high,
        p_value = estimate$p_value,
        display = shown[[this_outcome]][[row]]
      )
    })
    list(
      player = shown$Player[[row]],
      appearances = shown$Appearances[[row]],
      outcomes = set_names(outcomes, REGRESSION_OUTCOMES)
    )
  })
}

#' cv.glmnet warns on any fold under three matches, which ridge_coefficients()
#' accepts on purpose. Every other warning still surfaces.
muffle_glmnet_folds <- function(code) {
  withCallingHandlers(code, warning = function(w) {
    if (grepl("grouped=FALSE enforced", conditionMessage(w), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })
}

#' The site writes the estimator note and the explanation from the estimator, the
#' number of players, the appearance threshold and the opponent coverage.
export_regression <- function(scope, strength, estimator) {
  results <- muffle_glmnet_folds(
    player_regression_results(scope$attendance, scope$matches, strength, estimator = estimator)
  )
  # The site shows the player effects only with a fit, and player_regression_table()
  # cannot pivot an empty frame.
  has_regression <- nrow(results) > 0
  coverage <- opponent_coverage(scope$matches, strength)

  list(
    has_regression = has_regression,
    min_appearances = MIN_REGRESSION_APPEARANCES,
    opponent_coverage = list(observed = coverage$observed, total = coverage$total),
    players = if (has_regression) regression_rows(results) else list()
  )
}

export_matches <- function(matches) {
  outcomes <- match_outcomes(matches) |> arrange(desc(date))

  tibble(
    date = iso_date(outcomes$date),
    season_id = outcomes$season_id,
    opponent = outcomes$opponent,
    result = outcomes$result,
    goals_for = outcomes$goals_for,
    goals_against = outcomes$goals_against,
    goal_difference = outcomes$goal_difference,
    points = outcomes$points
  )
}

export_season_form <- function(form) {
  form |>
    mutate(date = iso_date(date)) |>
    select(
      date, opponent, result, points, goals_scored, goals_conceded, goal_difference,
      squad_size, starts_with("rolling_")
    )
}

#' The site writes each match's summary line from its row in `matches`.
export_match_details <- function(form, attendance) {
  newest_first <- as.character(sort(form$date, decreasing = TRUE))

  map(newest_first, function(selected_date) {
    detail <- match_detail_data(form, attendance, selected_date)
    list(date = selected_date, lineup = detail$lineup)
  })
}

#' The site writes the coverage line from `coverage`.
export_scorers <- function(scope) {
  scorers <- scorer_table(scope$events, scope$attendance, scope$matches)
  coverage <- event_coverage(scope$matches)

  list(
    rows = scorers |>
      mutate(goals_per_appearance_display = format_rate(goals_per_appearance)),
    coverage = list(
      observed = coverage$observed,
      total = coverage$total,
      dates = I(iso_date(sort(coverage$dates, decreasing = TRUE)))
    )
  )
}

export_opponent_record <- function(matches) {
  opponent_record(matches) |>
    mutate(points_per_match_display = format_rate(points_per_match))
}

with_rate_display <- function(side) {
  c(as.list(side), list(points_per_match_display = format_rate(side$points_per_match)))
}

#' The site writes the headline tiles and the with-and-without note from these.
export_profile <- function(player, scope) {
  profile <- player_profile(player, scope$attendance, scope$matches, scope$events)

  list(
    player = profile$player,
    matches = profile$matches,
    appearances = profile$appearances,
    attendance_rate = profile$attendance_rate,
    turnout_display = format_turnout(profile$attendance_rate),
    last_appearance = if (is.na(profile$last_appearance)) {
      NA_character_
    } else {
      iso_date(profile$last_appearance)
    },
    present = with_rate_display(profile$present),
    absent = with_rate_display(profile$absent),
    differential = profile$differential,
    differential_display = format_gap(profile$differential),
    goals = profile$goals,
    mom = profile$mom,
    covered = profile$covered,
    timeline = profile$timeline |> mutate(date = iso_date(date))
  )
}

#' Everything the season picker scopes, for one combination of seasons.
export_subset <- function(data, season_ids) {
  scope <- scope_to_seasons(data, season_ids)
  form <- season_form_data(scope$attendance, scope$matches)
  # The people who have a page in this window.
  attendance_list <- create_attendance_list(scope$attendance, scope$matches)

  list(
    season_ids = I(season_ids),
    has_matches = nrow(scope$matches) > 0,
    regression = list(
      ols = export_regression(scope, data$opponent_strength, "ols"),
      ridge = export_regression(scope, data$opponent_strength, "ridge")
    ),
    matches = export_matches(scope$matches),
    season_form = export_season_form(form),
    match_details = export_match_details(form, scope$attendance),
    scorers = export_scorers(scope),
    opponent_record = export_opponent_record(scope$matches),
    attendance_list = attendance_list,
    # By name rather than by turnout, so a week's results move fewer lines in
    # the diff. attendance_list keeps the order the roster table shows.
    profiles = map(
      sort(attendance_list$player, method = "radix"),
      export_profile,
      scope = scope
    )
  )
}

#' Every non-empty combination of seasons, each with its ids in sorted order.
#'
#' The count doubles with each season: three give 7 files. Around season 10,
#' about 2030, we would limit this to consecutive seasons. It is not built yet.
season_subsets <- function(season_ids) {
  ids <- sort(season_ids, method = "radix")
  unlist(
    lapply(seq_along(ids), function(size) combn(ids, size, simplify = FALSE)),
    recursive = FALSE
  )
}

#' The browser builds the same name with ids.sort().join("-"). Both sort
#' bytewise, so s10 sorts before s2 in each.
subset_file_name <- function(season_ids) {
  paste0(paste(sort(season_ids, method = "radix"), collapse = "-"), ".json")
}

# No season picker: read at build time ----

# The stale-deploy check. A CSV commit without regenerated JSON would silently
# publish old numbers, so the Astro build recomputes this and fails on a mismatch.
# It has to be reproducible with only node:crypto and node:fs, so exactly:
#
#   1. Take the visible files matching data/*.csv and data/archive/*.csv. The
#      archive is in because it feeds the balances, and leaving it out would let
#      a stale ledger through.
#   2. Write each path relative to the repository root with forward slashes, as
#      in data/archive/charges_s1.csv, and sort the paths bytewise.
#   3. For each path in that order, form "<path>\n<sha256 of the file's bytes, in
#      lowercase hex>\n", and concatenate those into one string.
#   4. data_hash is the sha256 of that string's UTF-8 bytes, in lowercase hex.
data_hash <- function(data_dir) {
  relative <- c(
    file.path("data", list.files(data_dir, pattern = "\\.csv$")),
    file.path("data", "archive", list.files(file.path(data_dir, "archive"), pattern = "\\.csv$"))
  )
  relative <- sort(relative, method = "radix")
  absolute <- file.path(dirname(data_dir), relative)

  file_hashes <- vapply(
    absolute,
    function(path) digest::digest(path, algo = "sha256", file = TRUE),
    character(1),
    USE.NAMES = FALSE
  )
  manifest <- paste0(relative, "\n", file_hashes, "\n", collapse = "")
  digest::digest(charToRaw(manifest), algo = "sha256", serialize = FALSE)
}

#' The latest date in a column, as ISO, or NA with no rows.
latest_date <- function(dates) if (length(dates) > 0) iso_date(max(dates)) else NA_character_

export_meta <- function(data, data_dir) {
  list(
    data_hash = data_hash(data_dir),
    our_team = OUR_TEAM,
    min_regression_appearances = MIN_REGRESSION_APPEARANCES,
    stale_fixtures_days = STALE_FIXTURES_DAYS,
    # Newest first, as the picker shows them, with the dates the site writes each
    # caption from.
    seasons = season_span(data$seasons, data$matches) |>
      arrange(desc(start_date)) |>
      mutate(across(c(start_date, first_match, last_match), iso_date)),
    subset_files = I(map_chr(season_subsets(data$seasons$season_id), subset_file_name)),
    problems = I(data$problems),
    # The two dates that bound the data, for the line under the strapline.
    freshness = list(
      last_result = latest_date(data$matches$date),
      scraped_on = latest_date(data$league_table$scraped_on)
    )
  )
}

#' What a match page shows about one summary, each absent where we hold nothing.
export_match_summary <- function(summary, seasons) {
  played <- summary$match

  list(
    date = iso_date(played$date),
    result = played$result,
    points = played$points,
    opponent = played$opponent,
    season_label = seasons$label[match(played$season_id, seasons$season_id)],
    squad_size = summary$squad_size,
    # In the order last_match_summary() gives them, which is the order they are listed.
    scorers = summary$scorers |> select(player, goals),
    man_of_the_match = summary$mom
  )
}

#' What each match page says, newest first, for /matches/<date>/.
#'
#' Each page is about its own night, so last_match_summary() is given that match alone.
#' A match dated before the first season is in no season file and no tick's table.
#' It stays a note in the banner and gets no page.
export_match_pages <- function(data) {
  played <- data$matches |> filter(!is.na(season_id))

  map(as.character(sort(played$date, decreasing = TRUE)), function(selected_date) {
    summary <- last_match_summary(
      played |> filter(date == as.Date(selected_date)), data$attendance, data$events
    )
    c(list(season_id = summary$match$season_id), export_match_summary(summary, data$seasons))
  })
}

#' Every unplayed fixture, whatever today is, with its standings columns.
#'
#' run_in() drops kickoff and pitch, so they are joined back. On the opponent as
#' well as the date, so that two fixtures on one date could not fan out. The site
#' says the fixture list is stale once scraped_on is over stale_fixtures_days old.
export_fixtures <- function(data) {
  fixtures <- run_in(data$fixtures, data$matches, data$league_table, today = FAR_PAST) |>
    left_join(
      data$fixtures |> select(date, opponent, kickoff, pitch),
      by = c("date", "opponent")
    ) |>
    arrange(date, opponent)

  list(
    have_fixtures = nrow(data$fixtures) > 0,
    scraped_on = latest_date(data$league_table$scraped_on),
    fixtures = tibble(
      date = iso_date(fixtures$date),
      opponent = fixtures$opponent,
      kickoff = fixtures$kickoff,
      pitch = fixtures$pitch,
      position = fixtures$position,
      played = fixtures$played,
      goal_difference = fixtures$goal_difference,
      points = fixtures$points
    )
  )
}

export_league <- function(data) {
  standings <- data$league_table

  list(
    scraped_on = latest_date(standings$scraped_on),
    rows = standings |> mutate(scraped_on = iso_date(scraped_on))
  )
}

export_fee_overview <- function(player, data) {
  overview <- player_fee_overview(player, data$charges, data$payments, data$seasons)
  charges <- overview$match_charges
  payments <- overview$payment_history

  list(
    player = player,
    total_charges = overview$total_charges,
    total_charges_display = format_fee_amount(overview$total_charges),
    total_payments = overview$total_payments,
    total_payments_display = format_fee_amount(overview$total_payments),
    balance = overview$balance,
    balance_display = format_fee_amount(overview$balance),
    match_charges = tibble(
      date = iso_date(charges$date),
      season = charges$season,
      result = charges$result,
      played = charges$played,
      squad_size = charges$squad_size,
      charge = charges$charge,
      charge_display = fee_strings(charges$charge),
      explanation = charges$explanation
    ),
    payment_history = tibble(
      date = iso_date(payments$date),
      amount = payments$amount,
      amount_display = fee_strings(payments$amount)
    )
  )
}

export_fees <- function(data) {
  balances <- all_player_balances(data$charges, data$payments)
  totals <- balance_totals(balances)
  # Everyone on any season's roster.
  fee_players <- data$rosters |>
    pull(player) |>
    unique() |>
    sort()
  # Everyone the two views can offer. The picker lists the roster and the
  # Everyone table lists anyone with a charge or a payment. Neither has to contain
  # the other: a signing with no match yet is only on the first, and a payment from
  # somebody on no roster is only on the second.
  overview_players <- sort(union(fee_players, balances$player), method = "radix")

  list(
    fee_players = I(fee_players),
    balances = tibble(
      player = balances$player,
      charges = balances$charges,
      charges_display = fee_strings(balances$charges),
      payments = balances$payments,
      payments_display = fee_strings(balances$payments),
      last_payment = iso_date(balances$last_payment),
      balance = balances$balance,
      balance_display = fee_strings(balances$balance)
    ),
    totals = list(
      outstanding = totals$outstanding,
      outstanding_display = format_fee_amount(totals$outstanding),
      credit = totals$credit,
      credit_display = format_fee_amount(totals$credit),
      net = totals$net,
      net_display = format_fee_amount(totals$net),
      settled = totals$settled
    ),
    overviews = map(overview_players, export_fee_overview, data = data)
  )
}

# Loading ----

# Every problem validate_app_data() lists goes in the site's banner except these two,
# which stop the export, because both make fees wrong: a match date on file twice
# charges that night's squad again, and a player down twice for one night changes
# everybody's share of the fee in any season the rules still price. Correct fees come
# first, and last week's files are better than a wrong balance under a quiet note.
#
# The answer is read from what validate_app_data() returns, so there is one duplicate
# check. Each name is the start of its wording in R/data.R and each value is the file
# to fix. If that wording changes the stop goes quiet, which the tests guard against.
STOPPING_PROBLEMS <- c(
  "matches recorded more than once" = "data/matches.csv",
  "attendance recorded twice for the same player and date" = "data/attendance.csv"
)

#' Stop, naming the rows and the file to fix, if any problem is one of those two.
stop_on_wrong_fees <- function(problems) {
  found <- unlist(map(names(STOPPING_PROBLEMS), function(prefix) {
    rows <- problems[startsWith(problems, paste0(prefix, ": "))]
    if (length(rows) > 0) paste0(rows, ". Fix ", STOPPING_PROBLEMS[[prefix]], ".")
  }))
  if (length(found) > 0) {
    stop(
      "Export stopped: these rows can make fees wrong. Nothing was written.\n",
      paste0("  ", found, collapse = "\n"),
      call. = FALSE
    )
  }
}

#' load_app_data(), with two of its habits accounted for.
#'
#' archived_charges() reads data/archive relative to the working directory
#' whatever dir load_app_data() was given, and a ledger it cannot find comes back
#' as no charges at all rather than as an error. So the working directory has to
#' be the folder holding data/, or every closed season's charges silently vanish.
#'
#' validate_app_data() warns about each problem and also returns it. Two of them
#' stop the export here, before anything is built or written, and the joins' warnings
#' about the same rows go with them. The rest become the page's banner and should not
#' stop or clutter the export. Any other warning is real, so it is raised again.
load_export_data <- function(data_dir) {
  if (basename(data_dir) != "data") {
    stop("data_dir must be a folder called data, because of archived_charges(): ", data_dir)
  }
  withr::local_dir(dirname(data_dir))

  warnings_seen <- character()
  data <- withCallingHandlers(
    load_app_data("data"),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  stop_on_wrong_fees(data$problems)
  for (text in setdiff(warnings_seen, data$problems)) {
    warning(text, call. = FALSE)
  }
  data
}

#' The matches with no attendance row, as dates, oldest first.
#'
#' The sync writes a match and attendance is typed afterwards. Until then the fee rules
#' price that match without its squad, so its fees are not right, and its page says there
#' was no lineup. The export carries on, because Ben decided only a repeated row stops it.
#' scripts/publish-site.sh stops on it instead.
matches_without_attendance <- function(matches, attendance) {
  sort(unique(matches$date[!matches$date %in% attendance$date]))
}

#' The same for the files on disk. Through load_app_data() rather than load_export_data(),
#' so that a repeated row is left for the export to stop on and name.
matches_awaiting_attendance <- function(data_dir = here::here("data")) {
  data <- withr::with_dir(dirname(data_dir), suppressWarnings(load_app_data("data")))
  matches_without_attendance(data$matches, data$attendance)
}

# Writing ----

#' Adding 0 changes a number only if it is -0, which balance_totals() returns as
#' the credit when nobody is in credit, because it negates an empty sum. That is
#' valid JSON, but it should not be in a file people read the diff of.
without_negative_zero <- function(x) {
  rapply(x, function(value) value + 0, classes = "numeric", how = "replace")
}

#' One document as UTF-8 text, with one trailing newline.
#'
#' Unnamed lists and data frames are arrays at any length, and a zero-row table
#' is []. auto_unbox would otherwise turn a length-1 vector into a scalar, so a
#' vector that is meant as an array is wrapped in I().
to_json <- function(x) {
  json <- jsonlite::toJSON(
    without_negative_zero(x),
    auto_unbox = TRUE, na = "null", null = "null",
    digits = I(JSON_SIGNIFICANT_DIGITS), pretty = 2L
  )
  paste0(enc2utf8(as.character(json)), "\n")
}

#' Written as raw bytes, so the session's encoding and line endings have no say.
write_text <- function(text, path) {
  connection <- file(path, open = "wb")
  on.exit(close(connection))
  writeBin(charToRaw(text), connection)
}

#' Replace everything in a folder this script owns, leaving no stale file behind:
#' a renamed season would otherwise leave its old files to be fetched.
write_documents <- function(documents, folder) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  stale <- setdiff(list.files(folder, pattern = "\\.json$"), names(documents))
  unlink(file.path(folder, stale))
  iwalk(documents, ~ write_text(.x, file.path(folder, .y)))
}

#' Build every JSON file. Returns the paths written and what the caller may want
#' to say about them.
#'
#' Everything is built before anything is written, so an export that fails part
#' of the way through leaves the last good files in place.
build_site_data <- function(data_dir = here::here("data"), out_root = here::here("site")) {
  withr::local_locale(EXPORT_LOCALE)
  data_dir <- normalizePath(data_dir, mustWork = TRUE)
  out_root <- normalizePath(out_root, mustWork = FALSE)

  data <- load_export_data(data_dir)

  subsets <- season_subsets(data$seasons$season_id)
  season_documents <- map(subsets, ~ to_json(export_subset(data, .x))) |>
    set_names(map_chr(subsets, subset_file_name))

  meta <- export_meta(data, data_dir)
  generated_documents <- list(
    meta.json = to_json(meta),
    fixtures.json = to_json(export_fixtures(data)),
    league.json = to_json(export_league(data)),
    fees.json = to_json(export_fees(data)),
    matches.json = to_json(export_match_pages(data))
  )

  seasons_folder <- file.path(out_root, "public", "data", "seasons")
  generated_folder <- file.path(out_root, "src", "generated")
  write_documents(season_documents, seasons_folder)
  write_documents(generated_documents, generated_folder)

  invisible(list(
    files = c(
      file.path(seasons_folder, names(season_documents)),
      file.path(generated_folder, names(generated_documents))
    ),
    data_hash = meta$data_hash,
    problems = data$problems
  ))
}

if (sys.nframe() == 0L) {
  # The question scripts/publish-site.sh asks before exporting. It answers with one
  # date per line, in the style of the CSVs, and writes nothing.
  if ("--matches-without-attendance" %in% commandArgs(trailingOnly = TRUE)) {
    writeLines(format(matches_awaiting_attendance(), "%d/%m/%Y"))
    quit(save = "no")
  }

  built <- build_site_data()
  message("Wrote ", length(built$files), " files. data_hash ", built$data_hash)
  if (length(built$problems) > 0) {
    message(
      "Data problems, exported to the banner:\n",
      paste0("  ", built$problems, collapse = "\n")
    )
  }
}
