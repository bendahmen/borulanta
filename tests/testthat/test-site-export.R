# The export is the only road from R's numbers to the static site, and the site
# has no way to notice a wrong one. So every field here is held against the
# function it came from, recomputed the way app.R computes it rather than read
# back through the export's own helpers, on the real files. expect_rows() and the
# names() checks fail on any exported field that has no source to be held against.
#
# Nothing below hard-codes a count from the data, because it runs again every
# week as the data grows. Season 3 has no OLS fit today and will have one soon.

source(here::here("scripts", "build-site-data.R"), local = TRUE)

snapshot_folder <- function(folder) {
  files <- list.files(folder, recursive = TRUE, all.files = TRUE)
  set_names(unname(tools::md5sum(file.path(folder, files))), files)
}

json_files_under <- function(root) {
  sort(list.files(root, pattern = "\\.json$", recursive = TRUE), method = "radix")
}

data_before <- snapshot_folder(here::here("data"))

run_one <- withr::local_tempdir(.local_envir = teardown_env())
run_two <- withr::local_tempdir(.local_envir = teardown_env())
built_one <- build_site_data(here::here("data"), file.path(run_one, "site"))

# The second run is in another locale where there is one, so that a date name or
# a sort order the export failed to pin shows up as a difference between the runs.
other_locale <- if ("de_DE.UTF-8" %in% system("locale -a", intern = TRUE)) {
  c(LC_TIME = "de_DE.UTF-8", LC_COLLATE = "de_DE.UTF-8")
} else {
  c(LC_TIME = Sys.getlocale("LC_TIME"), LC_COLLATE = Sys.getlocale("LC_COLLATE"))
}
built_two <- withr::with_locale(
  other_locale,
  build_site_data(here::here("data"), file.path(run_two, "site"))
)

data_after <- snapshot_folder(here::here("data"))

# Everything below is worked out in the export's locale, for the end of this file.
# The design's column order comes from base sort(), and glmnet's estimates depend
# on that order at this tolerance.
withr::local_locale(EXPORT_LOCALE)

# What Shiny loads: from the project root, because archived_charges() reads
# data/archive relative to the working directory.
shiny_data <- withr::with_dir(here::here(), suppressWarnings(load_app_data()))

read_exported <- function(...) {
  jsonlite::fromJSON(file.path(run_one, "site", ...), simplifyVector = FALSE)
}

read_generated <- function(name) read_exported("src", "generated", paste0(name, ".json"))

season_subset_ids <- function() {
  ids <- sort(shiny_data$seasons$season_id, method = "radix")
  unlist(
    lapply(seq_along(ids), function(size) combn(ids, size, simplify = FALSE)),
    recursive = FALSE
  )
}

read_subset <- function(ids) {
  read_exported("public", "data", "seasons", paste0(paste(ids, collapse = "-"), ".json"))
}

# As app.R's scoped().
scoped_like_app <- function(ids) {
  list(
    matches = filter_season(shiny_data$matches, ids),
    attendance = filter_season(shiny_data$attendance, ids),
    events = filter_season(shiny_data$events, ids)
  )
}

expected_results <- function(ids, estimator) {
  scope <- scoped_like_app(ids)
  suppressWarnings(player_regression_results(
    scope$attendance, scope$matches, shiny_data$opponent_strength, estimator = estimator
  ))
}

as_number <- function(value) if (is.null(value)) NA_real_ else as.numeric(value)

numbers_of <- function(rows, field) vapply(rows, function(row) as_number(row[[field]]), numeric(1))

strings_of <- function(rows, field) {
  vapply(
    rows,
    function(row) if (is.null(row[[field]])) NA_character_ else row[[field]],
    character(1)
  )
}

round_trip <- function(document) jsonlite::fromJSON(to_json(document), simplifyVector = FALSE)

CHILD_TIMEOUT_SECONDS <- 120L

# Every child R goes through here. A stalled child comes back with status 124 instead
# of stalling the suite, and stderr is in the text, for a failing test to show. The
# profile is skipped and packages come through R_LIBS, so that no child runs renv.
run_rscript <- function(args, dir, timeout = CHILD_TIMEOUT_SECONDS) {
  output <- withr::with_dir(dir, withr::with_envvar(
    c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)),
    suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"), c("--no-init-file", args),
      stdout = TRUE, stderr = TRUE, timeout = timeout
    ))
  ))
  list(status = attr(output, "status") %||% 0L, text = paste(output, collapse = "\n"))
}

fee_strings_of <- function(amounts) unname(vapply(amounts, format_fee_amount, character(1)))

# Wording read off the tags another way than tag_text() does, so that the two check
# each other. Each tag selected has one child, which is the string.
query_tags <- function(tag, selector) {
  unname(htmltools::tagQuery(div(tag))$find(selector)$selectedTags())
}

leaf_texts <- function(tag, selector) {
  map_chr(query_tags(tag, selector), ~ as.character(.x$children[[1]]))
}

leaf_text_or_na <- function(tag, selector) {
  found <- leaf_texts(tag, selector)
  if (length(found) == 0) NA_character_ else found[[1]]
}

# Per element and relative. An overall tolerance would pass a 6e-53 estimate
# exported as 0, and all.equal() scales by the mean of the column.
expect_same_numbers <- function(exported, expected, label, tolerance = 1e-8) {
  expected <- as.numeric(expected)
  expect_equal(is.na(exported), is.na(expected), label = paste(label, "null positions"))
  known <- !is.na(expected)
  off <- abs(exported[known] - expected[known]) > tolerance * abs(expected[known])
  expect_false(any(off), label = paste(label, "off by more than the relative tolerance"))
}

# The penny check, and tighter: these are rounded to two places, and ten
# significant digits carries that exactly.
expect_same_money <- function(exported, expected, label) {
  expect_length(exported, length(expected))
  expect_true(all(abs(exported - expected) < 1e-9), label = paste(label, "to the penny"))
}

# Rows against a frame of what they should hold, field by field. A field the frame
# does not name fails too, so nothing gets exported without a source to check it by.
expect_rows <- function(rows, expected, label = "rows") {
  expect_equal(length(rows), nrow(expected), label = paste(label, "row count"))
  if (length(rows) == 0) {
    return(invisible())
  }
  expect_setequal(names(rows[[1]]), names(expected))

  for (field in names(expected)) {
    context <- paste(label, field)
    column <- expected[[field]]
    if (is.numeric(column)) {
      expect_same_numbers(numbers_of(rows, field), column, context)
    } else if (is.logical(column)) {
      expect_equal(vapply(rows, function(row) row[[field]], logical(1)), column, label = context)
    } else {
      expect_equal(strings_of(rows, field), as.character(column), label = context)
    }
  }
}

# A small data folder for the states the real files are not in today. Two seasons,
# the second with nothing played. Ghost is a data problem: on no roster and in no
# players.csv, which validate_app_data() reports twice.
write_tiny_data <- function(with_matches = TRUE, ghost = TRUE, env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  data_dir <- file.path(root, "data")
  dir.create(data_dir)
  write_file <- function(name, ...) writeLines(c(...), file.path(data_dir, name))

  write_file(
    "seasons.csv",
    "season_id,label,start_date,fee_rules,status",
    "s1,Season 1,01/01/2026,even_split,open",
    "s2,Season 2,01/06/2030,even_split,open"
  )
  write_file("players.csv", "player,guest", "Ann,FALSE", "Bob,FALSE")
  write_file(
    "player_seasons.csv", "season_id,player,core,active", "s1,Ann,TRUE,TRUE", "s1,Bob,TRUE,TRUE"
  )
  write_file(
    "matches.csv", "date,opponent,goals_for,goals_against,dl_match_id",
    if (with_matches) c("07/01/2026,,3,1,", "14/01/2026,,2,2,")
  )
  write_file("match_events.csv", "date,dl_match_id,team,minute,event_type,player")
  write_file(
    "attendance.csv", "date,player",
    if (with_matches) {
      c(
        "07/01/2026,Ann", "07/01/2026,Bob", "14/01/2026,Ann",
        if (ghost) "14/01/2026,Ghost" else "14/01/2026,Bob"
      )
    }
  )
  write_file("payments.csv", "date,player,amount", "08/01/2026,Ann,10")
  data_dir
}

# Two seasons with enough matches for both estimators. Nothing is wrong with this one yet.
write_two_season_data <- function(env = parent.frame()) {
  data_dir <- write_tiny_data(env = env)
  write_file <- function(name, ...) writeLines(c(...), file.path(data_dir, name))
  dates <- c(
    "07/01/2026", "14/01/2026", "21/01/2026", "28/01/2026", "04/02/2026", "11/02/2026",
    "04/03/2026", "11/03/2026", "18/03/2026", "25/03/2026", "01/04/2026", "08/04/2026"
  )
  lineups <- rep(list(
    c("Ann", "Bob", "Cy"), c("Ann", "Bob", "Di"), c("Ann", "Cy", "Di"),
    c("Bob", "Cy", "Di"), c("Ann", "Bob", "Cy", "Di"), c("Ann", "Cy")
  ), 2)

  write_file(
    "seasons.csv",
    "season_id,label,start_date,fee_rules,status",
    "s1,Season 1,01/01/2026,even_split,open",
    "s2,Season 2,01/03/2026,even_split,open"
  )
  write_file("players.csv", "player,guest", paste0(c("Ann", "Bob", "Cy", "Di"), ",FALSE"))
  write_file(
    "player_seasons.csv", "season_id,player,core,active",
    paste0(rep(c("s1", "s2"), each = 4), ",", rep(c("Ann", "Bob", "Cy", "Di"), 2), ",TRUE,TRUE")
  )
  write_file(
    "matches.csv", "date,opponent,goals_for,goals_against,dl_match_id",
    paste0(
      dates, ",,", c(3, 1, 4, 2, 5, 0, 2, 2, 6, 1, 3, 4), ",",
      c(2, 2, 1, 3, 0, 1, 1, 4, 2, 1, 3, 0), ","
    )
  )
  write_file(
    "attendance.csv", "date,player",
    unlist(map2(dates, lineups, ~ paste0(.x, ",", .y)))
  )
  data_dir
}

read_from <- function(out, ...) jsonlite::fromJSON(file.path(out, ...), simplifyVector = FALSE)

# Files ----

test_that("every combination of seasons has a file and nothing else does", {
  ids <- shiny_data$seasons$season_id
  expected <- paste0(map_chr(season_subset_ids(), paste, collapse = "-"), ".json")

  # 7 today.
  expect_length(expected, 2^length(ids) - 1)
  expect_setequal(
    list.files(file.path(run_one, "site", "public", "data", "seasons")),
    expected
  )
  expect_setequal(read_generated("meta")$subset_files, expected)
  expect_setequal(
    list.files(file.path(run_one, "site", "src", "generated")),
    c(
      "meta.json", "home.json", "fixtures.json", "league.json", "fees.json", "matches.json",
      "wording.json"
    )
  )
})

test_that("a child R that stalls is cut off, and one that fails says why", {
  stalled <- run_rscript(c("-e", shQuote("Sys.sleep(60)")), tempdir(), timeout = 1L)
  expect_equal(stalled$status, 124L, info = stalled$text)

  failed <- run_rscript(c("-e", shQuote("stop('said on stderr')")), tempdir())
  expect_equal(failed$status, 1L, info = failed$text)
  expect_match(failed$text, "said on stderr", fixed = TRUE)
})

test_that("a stale file from an earlier run does not survive", {
  # A renamed season would otherwise leave its old file there to be fetched.
  out <- withr::local_tempdir()
  stale <- file.path(out, "public", "data", "seasons", "s99.json")
  dir.create(dirname(stale), recursive = TRUE)
  writeLines("{}", stale)

  build_site_data(write_tiny_data(), out)

  expect_false(file.exists(stale))
})

test_that("two runs are byte-identical across every file, in any locale", {
  files_one <- json_files_under(run_one)
  expect_equal(files_one, json_files_under(run_two))
  expect_gt(length(files_one), 0)

  for (file_name in files_one) {
    expect_identical(
      readBin(file.path(run_one, file_name), "raw", n = 1e8),
      readBin(file.path(run_two, file_name), "raw", n = 1e8),
      label = file_name
    )
  }
})

test_that("the export changes nothing under data/", {
  expect_identical(data_after, data_before)
})

test_that("files are UTF-8, end in one newline and hold no negative zero", {
  for (file_name in json_files_under(run_one)) {
    bytes <- readBin(file.path(run_one, file_name), "raw", n = 1e8)
    text <- rawToChar(bytes)
    Encoding(text) <- "UTF-8"

    expect_true(validUTF8(text), label = file_name)
    expect_equal(tail(bytes, 1), charToRaw("\n"), label = file_name)
    expect_false(identical(tail(bytes, 2)[[1]], charToRaw("\n")), label = file_name)
    expect_false(grepl("<U+", text, fixed = TRUE), label = file_name)
    expect_false(grepl(": -0,?\n", text), label = paste(file_name, "negative zero"))
  }
  fees <- rawToChar(readBin(
    file.path(run_one, "site", "src", "generated", "fees.json"), "raw", n = 1e8
  ))
  expect_match(fees, "\xc2\xa3", fixed = TRUE, useBytes = TRUE)
})

test_that("to_json keeps arrays as arrays and small numbers as numbers", {
  json <- to_json(list(
    one = I("s1"), none = I(character()), rows = tibble(x = numeric()),
    row = tibble(x = 1, y = NA_character_), nothing = NULL, missing = NA_character_,
    tiny = 5.932243214e-53, money = 1234.56, negative_zero = -0
  ))
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)

  expect_match(json, '"one": ["s1"]', fixed = TRUE)
  expect_match(json, '"none": []', fixed = TRUE)
  expect_match(json, '"rows": []', fixed = TRUE)
  expect_length(parsed$row, 1)
  expect_true("y" %in% names(parsed$row[[1]]))
  expect_null(parsed$row[[1]]$y)
  expect_true(all(c("nothing", "missing") %in% names(parsed)))
  expect_lt(abs(parsed$tiny - 5.932243214e-53) / 5.932243214e-53, 1e-8)
  expect_equal(parsed$money, 1234.56)
  expect_match(json, '"negative_zero": 0\n', fixed = TRUE)
})

# Scoping ----

test_that("a subset filters every table the way app.R's scoped() does", {
  # Not visible through the statistics alone: scorer_table() and player_profile()
  # filter events to the covered dates themselves, so most windows would come out
  # the same with events left unscoped.
  for (ids in season_subset_ids()) {
    scope <- scope_to_seasons(shiny_data, ids)
    for (table in c("matches", "attendance", "events", "payments", "charges")) {
      expect_true(all(scope[[table]]$season_id %in% ids), label = table)
      expect_equal(nrow(scope[[table]]), sum(shiny_data[[table]]$season_id %in% ids), label = table)
    }
  }
})

# Regressions ----

test_that("each subset holds both estimators and all four outcomes", {
  for (ids in season_subset_ids()) {
    exported <- read_subset(ids)
    expect_setequal(names(exported), c(
      "season_ids", "has_matches", "regression", "matches", "season_form", "match_details",
      "scorers", "opponent_record", "attendance_list", "profiles"
    ))
    expect_setequal(names(exported$regression), c("ols", "ridge"))
    expect_equal(unlist(exported$season_ids), ids)
    # Parsed without simplifying, a JSON array is a list whatever its length.
    expect_type(exported$season_ids, "list")
    expect_type(exported$scorers$coverage$dates, "list")
    expect_equal(exported$has_matches, nrow(scoped_like_app(ids)$matches) > 0)

    for (estimator in c("ols", "ridge")) {
      label <- paste(c(ids, estimator), collapse = " ")
      results <- expected_results(ids, estimator)
      block <- exported$regression[[estimator]]

      expect_setequal(names(block), c(
        "has_regression", "note", "explanation", "opponent_coverage", "players"
      ))
      expect_equal(block$has_regression, nrow(results) > 0, label = label)
      expect_equal(block$note, estimate_label(estimator), label = label)
      expect_type(block$explanation, "list")
      expect_length(block$players, n_distinct(results$player))
      for (row in block$players) {
        expect_setequal(names(row), c("player", "appearances", "outcomes"))
        expect_equal(names(row$outcomes), REGRESSION_OUTCOMES, label = label)
        for (this_outcome in REGRESSION_OUTCOMES) {
          expect_setequal(names(row$outcomes[[this_outcome]]), c(
            "estimate", "std_error", "conf_low", "conf_high", "p_value", "display"
          ))
        }
      }
    }
  }
})

test_that("a window with no fit is empty rather than zero, under that estimator alone", {
  # Season 3 alone until it has enough matches: OLS has no residual degrees of
  # freedom and ridge is defined anyway. Worked out from the function, so that
  # this stays true of whichever windows that describes.
  for (ids in season_subset_ids()) {
    exported <- read_subset(ids)
    for (estimator in c("ols", "ridge")) {
      if (nrow(expected_results(ids, estimator)) == 0) {
        block <- exported$regression[[estimator]]
        expect_false(block$has_regression)
        expect_length(block$players, 0)
        expect_length(block$explanation, 0)
      }
    }
  }
})

test_that("every regression number equals player_regression_results()", {
  for (ids in season_subset_ids()) {
    exported <- read_subset(ids)
    for (estimator in c("ols", "ridge")) {
      results <- expected_results(ids, estimator)

      for (row in exported$regression[[estimator]]$players) {
        for (this_outcome in REGRESSION_OUTCOMES) {
          expected <- results |> filter(player == row$player, outcome == this_outcome)
          expect_equal(nrow(expected), 1L)
          for (column in c("estimate", "std_error", "conf_low", "conf_high", "p_value")) {
            expect_same_numbers(
              as_number(row$outcomes[[this_outcome]][[column]]), expected[[column]],
              paste(c(ids, estimator, row$player, this_outcome, column), collapse = " ")
            )
          }
          expect_equal(row$appearances, expected$appearances)
        }
      }
    }
  }
})

test_that("rows and their display strings are player_regression_table()", {
  for (ids in season_subset_ids()) {
    exported <- read_subset(ids)
    for (estimator in c("ols", "ridge")) {
      results <- expected_results(ids, estimator)
      rows <- exported$regression[[estimator]]$players
      if (nrow(results) == 0) next
      shown <- player_regression_table(results)

      expect_equal(strings_of(rows, "player"), shown$Player)
      expect_equal(numbers_of(rows, "appearances"), as.numeric(shown$Appearances))
      for (this_outcome in REGRESSION_OUTCOMES) {
        expect_equal(
          vapply(rows, function(row) row$outcomes[[this_outcome]]$display, character(1)),
          shown[[OUTCOME_LABELS[[this_outcome]]]]
        )
      }
    }
  }
})

test_that("the explanation and the opponent coverage come from the cards", {
  for (ids in season_subset_ids()) {
    exported <- read_subset(ids)
    scope <- scoped_like_app(ids)
    coverage <- opponent_coverage(scope$matches, shiny_data$opponent_strength)
    for (estimator in c("ols", "ridge")) {
      results <- expected_results(ids, estimator)
      block <- exported$regression[[estimator]]
      expect_equal(block$opponent_coverage$observed, coverage$observed)
      expect_equal(block$opponent_coverage$total, coverage$total)
      if (nrow(results) == 0) next

      card <- regression_explanation_ui(
        results, MIN_REGRESSION_APPEARANCES, coverage, estimator
      )
      paragraphs <- htmltools::tagQuery(card)$find("p")$selectedTags()
      expect_equal(unlist(block$explanation), unname(map_chr(paragraphs, ~ .x$children[[1]])))
    }
  }
})

# Everything else the season picker scopes ----

test_that("matches come newest first with R's strings", {
  for (ids in season_subset_ids()) {
    matches <- scoped_like_app(ids)$matches
    rows <- read_subset(ids)$matches
    expected <- match_outcomes(matches) |> arrange(desc(date))

    expect_rows(rows, tibble(
      date = format(expected$date, "%Y-%m-%d"),
      date_label = format_match_date(expected$date),
      date_short = match_table(expected)$x$data$Date,
      season_id = expected$season_id,
      opponent = expected$opponent,
      result = expected$result,
      goals_for = expected$goals_for,
      goals_against = expected$goals_against,
      goal_difference = expected$goal_difference,
      points = expected$points,
      badge = case_when(expected$points == 3 ~ "WIN", expected$points == 1 ~ "DRAW", TRUE ~ "LOSS")
    ), paste(ids, collapse = "-"))
    # English whatever the locale the run was started in.
    expect_true(all(grepl("^(Mon|Tue|Wed|Thu|Fri|Sat|Sun) ", strings_of(rows, "date_label"))))
  }
})

test_that("season form and lineups equal the functions they come from", {
  for (ids in season_subset_ids()) {
    scope <- scoped_like_app(ids)
    exported <- read_subset(ids)
    form <- season_form_data(scope$attendance, scope$matches)

    expect_rows(
      exported$season_form,
      form |>
        transmute(
          date = format(date, "%Y-%m-%d"), opponent, result, points, goals_scored,
          goals_conceded, goal_difference, squad_size, rolling_points, rolling_goals_scored,
          rolling_goals_conceded, rolling_goal_difference, rolling_squad_size
        ),
      "season form"
    )

    expect_equal(
      strings_of(exported$match_details, "date"),
      format(sort(form$date, decreasing = TRUE), "%Y-%m-%d")
    )
    for (detail in exported$match_details) {
      expected <- match_detail_data(form, scope$attendance, detail$date)
      expect_setequal(names(detail), c("date", "summary_note", "lineup"))
      expect_rows(detail$lineup, expected$lineup, paste(detail$date, "lineup"))
      expect_equal(
        detail$summary_note,
        leaf_texts(match_summary_ui(expected$match), ".match-detail-note")
      )
    }
  }
})

test_that("scorers, opponents and the roster equal the functions they come from", {
  for (ids in season_subset_ids()) {
    scope <- scoped_like_app(ids)
    exported <- read_subset(ids)

    scorers <- scorer_table(scope$events, scope$attendance, scope$matches)
    coverage <- event_coverage(scope$matches)
    expect_setequal(names(exported$scorers), c("rows", "coverage", "note"))
    expect_rows(
      exported$scorers$rows,
      scorers |> mutate(goals_per_appearance_rounded = round(goals_per_appearance, 2)),
      "scorers"
    )
    expect_setequal(names(exported$scorers$coverage), c("observed", "total", "dates"))
    expect_equal(exported$scorers$coverage$observed, coverage$observed)
    expect_equal(exported$scorers$coverage$total, coverage$total)
    expect_equal(
      as.character(unlist(exported$scorers$coverage$dates)),
      format(sort(coverage$dates, decreasing = TRUE), "%Y-%m-%d")
    )
    expect_equal(exported$scorers$note, leaf_texts(scorer_coverage_ui(coverage), "p"))

    expect_rows(
      exported$opponent_record,
      opponent_record(scope$matches) |>
        mutate(points_per_match_rounded = round(points_per_match, 2)),
      "opponent record"
    )

    expect_rows(
      exported$attendance_list,
      create_attendance_list(scope$attendance, scope$matches),
      "attendance list"
    )
  }
})

test_that("every roster player has a profile, with its wording and its timeline", {
  for (ids in season_subset_ids()) {
    scope <- scoped_like_app(ids)
    exported <- read_subset(ids)
    roster <- create_attendance_list(scope$attendance, scope$matches)

    expect_equal(
      strings_of(exported$profiles, "player"),
      sort(roster$player, method = "radix")
    )

    for (row in exported$profiles) {
      profile <- player_profile(row$player, scope$attendance, scope$matches, scope$events)
      label <- paste(c(ids, row$player), collapse = " ")

      expect_setequal(names(row), c(
        "player", "matches", "appearances", "attendance_rate", "last_appearance", "present",
        "absent", "differential", "differential_note", "goals", "mom", "covered", "headline",
        "timeline"
      ))
      expect_rows(
        list(row[c(
          "player", "matches", "appearances", "attendance_rate", "last_appearance",
          "differential", "goals", "mom", "covered"
        )]),
        tibble(
          player = profile$player,
          matches = profile$matches,
          appearances = profile$appearances,
          attendance_rate = profile$attendance_rate,
          last_appearance = format(profile$last_appearance, "%Y-%m-%d"),
          differential = profile$differential,
          goals = profile$goals,
          mom = profile$mom,
          covered = profile$covered
        ),
        label
      )
      expect_rows(
        list(row$present),
        profile$present |> mutate(points_per_match_rounded = round(points_per_match, 2)),
        paste(label, "present")
      )
      expect_rows(
        list(row$absent),
        profile$absent |> mutate(points_per_match_rounded = round(points_per_match, 2)),
        paste(label, "absent")
      )
      expect_equal(
        row$differential_note, as.character(differential_note(profile)$children[[1]]),
        label = label
      )

      headline <- player_headline_ui(profile)
      expect_rows(row$headline, tibble(
        label = leaf_texts(headline, ".match-detail-label"),
        value = leaf_texts(headline, ".match-detail-value"),
        note = leaf_texts(headline, ".match-detail-note")
      ), paste(label, "headline"))
      expect_rows(
        row$timeline, profile$timeline |> mutate(date = format(date, "%Y-%m-%d")),
        paste(label, "timeline")
      )
    }
  }
})

# Fees ----

test_that("every balance equals all_player_balances() to the penny", {
  fees <- read_generated("fees")
  expected <- all_player_balances(shiny_data$charges, shiny_data$payments)

  # Closed seasons are read from the archive relative to the working directory,
  # and this test does not run from the project root. This is what would go.
  expect_gt(sum(shiny_data$charges$season_id %in% c("s1", "s2")), 0)

  expect_setequal(names(fees), c("fee_players", "balances", "totals", "overviews", "empty"))
  expect_equal(strings_of(fees$balances, "player"), expected$player)
  expect_same_money(numbers_of(fees$balances, "charges"), expected$charges, "charges")
  expect_same_money(numbers_of(fees$balances, "payments"), expected$payments, "payments")
  expect_same_money(numbers_of(fees$balances, "balance"), expected$balance, "balance")

  expect_rows(fees$balances, tibble(
    player = expected$player,
    charges = expected$charges,
    charges_display = fee_strings_of(expected$charges),
    payments = expected$payments,
    payments_display = fee_strings_of(expected$payments),
    last_payment = format(expected$last_payment, "%Y-%m-%d"),
    last_payment_display = balance_table(expected)$x$data[["Last payment"]],
    balance = expected$balance,
    balance_display = fee_strings_of(expected$balance)
  ), "balances")
})

# The Everyone table and the Home picker read the first and Shiny's card reads the second.
# One rounds before subtracting and the other after, so a half-penny can part them.
test_that("all_player_balances() and player_fee_overview() give every player one balance", {
  balances <- all_player_balances(shiny_data$charges, shiny_data$payments)
  expect_equal(fee_strings_of(balances$balance), map_chr(balances$player, ~ format_fee_amount(player_fee_overview(.x, shiny_data$charges, shiny_data$payments, shiny_data$seasons)$balance)))
})

test_that("the totals equal balance_totals()", {
  totals <- read_generated("fees")$totals
  expected <- balance_totals(all_player_balances(shiny_data$charges, shiny_data$payments))

  expect_same_money(totals$outstanding, expected$outstanding, "outstanding")
  expect_same_money(totals$credit, expected$credit, "credit")
  expect_same_money(totals$net, expected$net, "net")
  expect_rows(list(totals), tibble(
    outstanding = expected$outstanding,
    outstanding_display = format_fee_amount(expected$outstanding),
    credit = expected$credit,
    credit_display = format_fee_amount(expected$credit),
    net = expected$net,
    net_display = format_fee_amount(expected$net),
    settled = expected$settled
  ), "totals")
})

test_that("each player's charges and payments equal player_fee_overview()", {
  fees <- read_generated("fees")
  everyone <- union(
    shiny_data$rosters |> filter(active) |> pull(player),
    all_player_balances(shiny_data$charges, shiny_data$payments)$player
  )
  expect_equal(strings_of(fees$overviews, "player"), sort(everyone, method = "radix"))

  for (row in fees$overviews) {
    expected <- player_fee_overview(
      row$player, shiny_data$charges, shiny_data$payments, shiny_data$seasons
    )
    charges <- expected$match_charges
    payments <- expected$payment_history

    expect_setequal(names(row), c(
      "player", "total_charges", "total_charges_display", "total_payments",
      "total_payments_display", "balance", "balance_display", "match_charges", "payment_history"
    ))
    expect_same_money(row$total_charges, expected$total_charges, row$player)
    expect_same_money(row$total_payments, expected$total_payments, row$player)
    expect_same_money(row$balance, expected$balance, row$player)
    expect_equal(row$total_charges_display, format_fee_amount(expected$total_charges))
    expect_equal(row$total_payments_display, format_fee_amount(expected$total_payments))
    expect_equal(row$balance_display, format_fee_amount(expected$balance))

    expect_rows(row$match_charges, tibble(
      date = format(charges$date, "%Y-%m-%d"),
      date_short = match_charge_table(charges)$x$data$Date,
      season = charges$season,
      result = charges$result,
      played = charges$played,
      squad_size = charges$squad_size,
      charge = charges$charge,
      charge_display = fee_strings_of(charges$charge),
      explanation = charges$explanation
    ), paste(row$player, "charges"))
    expect_rows(row$payment_history, tibble(
      date = format(payments$date, "%Y-%m-%d"),
      date_short = payment_history_table(payments)$x$data$Date,
      amount = payments$amount,
      amount_display = fee_strings_of(payments$amount)
    ), paste(row$player, "payments"))
  }
})

test_that("fee_players is what app.R offers, and the empty wording is the tables'", {
  fees <- read_generated("fees")
  expected <- shiny_data$rosters |> filter(active) |> pull(player) |> unique() |> sort()
  expect_equal(unlist(fees$fee_players), expected)

  balances <- all_player_balances(shiny_data$charges, shiny_data$payments)
  expect_equal(fees$empty$balances, balance_table(balances[0, ])$x$options$language$emptyTable)
  no_payments <- payment_history_table(tibble(date = as.Date(character()), amount = numeric()))
  expect_equal(fees$empty$payments, no_payments$x$options$language$emptyTable)
})

# Fixtures, whatever today is ----

crafted_fixture_data <- function() {
  list(
    fixtures = tibble(
      date = as.Date(c("2031-03-05", "2001-01-03", "2031-03-12", "2031-03-19")),
      opponent = c("Rovers", "Long Gone", "Already Played", NA_character_),
      kickoff = c("20:00", "19:00", "19:40", NA_character_),
      pitch = c("Pitch 1", "Front Pitch", "Pitch 1", NA_character_)
    ),
    matches = tibble(date = as.Date("2031-03-12")),
    league_table = tibble(
      team = c("Rovers", "Borulanta"), position = 1:2, played = c(4L, 4L),
      goal_difference = c(5L, -5L), points = c(10L, 3L),
      scraped_on = as.Date("2031-03-01")
    )
  )
}

test_that("the fixtures hold every unplayed fixture, whatever today is", {
  exported <- to_json(export_fixtures(crafted_fixture_data())) |>
    jsonlite::fromJSON(simplifyVector = FALSE)
  rows <- exported$fixtures

  # In date order, the one that is years old included: filtering on today is the
  # browser's job, because a static build would freeze the answer.
  expect_equal(strings_of(rows, "date"), c("2001-01-03", "2031-03-05", "2031-03-19"))
  expect_true(exported$have_fixtures)

  expect_equal(rows[[2]]$opponent, "Rovers")
  expect_equal(rows[[2]]$kickoff, "20:00")
  expect_equal(rows[[2]]$pitch, "Pitch 1")
  expect_equal(rows[[2]]$kickoff_pitch, "20:00 \u00b7 Pitch 1")
  expect_equal(rows[[2]]$position, 1)
  expect_equal(rows[[2]]$goal_difference, 5)
  expect_equal(rows[[2]]$points, 10)
  expect_equal(rows[[2]]$date_label, format_match_date(as.Date("2031-03-05")))
})

test_that("a fixture dated on a recorded match is left out", {
  rows <- export_fixtures(crafted_fixture_data())$fixtures
  expect_false("Already Played" %in% rows$opponent)
  expect_false("2031-03-12" %in% rows$date)
})

test_that("a fixture the standings do not carry keeps its row, with those columns empty", {
  rows <- to_json(export_fixtures(crafted_fixture_data())) |>
    jsonlite::fromJSON(simplifyVector = FALSE)
  long_gone <- rows$fixtures[[1]]
  unnamed <- rows$fixtures[[3]]

  expect_equal(long_gone$opponent, "Long Gone")
  expect_null(long_gone$position)
  expect_equal(long_gone$kickoff, "19:00")
  expect_null(unnamed$opponent)
  expect_null(unnamed$kickoff_pitch)
})

test_that("the real fixtures equal run_in() from the far past", {
  rows <- read_generated("fixtures")$fixtures
  expected <- run_in(
    shiny_data$fixtures, shiny_data$matches, shiny_data$league_table,
    today = as.Date("1900-01-01")
  ) |>
    left_join(
      shiny_data$fixtures |> select(date, opponent, kickoff, pitch),
      by = c("date", "opponent")
    ) |>
    arrange(date, opponent)
  # With today's date it can only be fewer.
  live <- run_in(shiny_data$fixtures, shiny_data$matches, shiny_data$league_table)
  expect_gte(length(rows), nrow(live))

  expect_rows(rows, tibble(
    date = format(expected$date, "%Y-%m-%d"),
    date_label = format_match_date(expected$date),
    date_short = run_in_table(expected)$x$data$Date,
    opponent = expected$opponent,
    kickoff = expected$kickoff,
    pitch = expected$pitch,
    # The page does not always give these, and the line is dropped when it gives neither.
    kickoff_pitch = map_chr(
      seq_len(nrow(expected)),
      ~ leaf_text_or_na(kickoff_pitch_line(expected[.x, ]), ".home-card-meta")
    ),
    position = expected$position,
    played = expected$played,
    goal_difference = expected$goal_difference,
    points = expected$points
  ), "fixtures")
})

test_that("R owns the words around a fixture, for the browser to pick from", {
  exported <- read_generated("fixtures")
  wording <- exported$wording

  expect_setequal(names(exported), c(
    "have_fixtures", "scraped_on", "stale_note", "wording", "run_in_empty", "fixtures"
  ))
  expect_setequal(names(wording), c(
    "tag", "no_fixtures_left", "no_fixture_list", "opponent_unknown", "tonight", "tomorrow",
    "in_days"
  ))
  expect_equal(exported$have_fixtures, nrow(shiny_data$fixtures) > 0)
  expect_equal(exported$scraped_on, format(max(shiny_data$league_table$scraped_on), "%Y-%m-%d"))
  no_fixtures <- tibble(date = as.Date(character()), opponent = character(), position = integer())
  expect_equal(exported$run_in_empty, run_in_table(no_fixtures)$x$options$language$emptyTable)

  expect_equal(
    wording$no_fixtures_left,
    "No fixtures left on the league page. Run the sync once the new season is up."
  )
  expect_equal(
    wording$no_fixture_list,
    tag_text(next_match_card_ui(NULL, have_fixtures = FALSE), "empty-state")
  )

  scraped_on <- shiny_data$league_table$scraped_on
  expect_equal(
    exported$stale_note,
    tag_text(
      stale_fixture_note(scraped_on, max(scraped_on) + STALE_FIXTURES_DAYS + 1L),
      "home-stale"
    )
  )
  # Not on the far side of the threshold, which is why the browser has to choose.
  expect_null(stale_fixture_note(scraped_on, max(scraped_on) + STALE_FIXTURES_DAYS))

  fixture <- tibble(
    date = as.Date("2031-03-05"), opponent = NA_character_,
    kickoff = NA_character_, pitch = NA_character_
  )
  card <- next_match_card_ui(fixture, TRUE, today = fixture$date - 3L)
  expect_equal(wording$tag, leaf_texts(card, ".section-tag"))
  expect_equal(wording$opponent_unknown, leaf_texts(card, ".home-opponent"))

  when <- function(days) {
    card <- next_match_card_ui(fixture, TRUE, today = fixture$date - days)
    tag_text(card, "home-when")
  }
  expect_equal(wording$tonight, when(0))
  expect_equal(wording$tomorrow, when(1))
  expect_equal(sub("{days}", "12", wording$in_days, fixed = TRUE), when(12))
})

# Home, league and meta ----

# The card drops what a match does not have, so each of these can be absent, and a
# week with no man of the match is an ordinary week.
expect_home_is_the_card <- function(home, data) {
  summary <- last_match_summary(data$matches, data$attendance, data$events)
  played <- summary$match
  card <- last_match_card_ui(summary, data$seasons)
  last <- home$last_match

  expect_setequal(names(last), c(
    "date", "date_label", "result", "points", "badge", "opponent", "season_label", "meta_line",
    "squad_size", "scorers", "man_of_the_match", "details"
  ))
  expect_equal(last$date, format(played$date, "%Y-%m-%d"))
  expect_equal(last$date_label, format_match_date(played$date))
  expect_equal(last$result, played$result)
  expect_equal(last$points, played$points)
  expect_equal(
    last$badge,
    case_when(played$points == 3 ~ "WIN", played$points == 1 ~ "DRAW", TRUE ~ "LOSS")
  )
  expect_equal(last$opponent, if (!is.na(played$opponent)) played$opponent)
  expect_equal(
    last$season_label,
    data$seasons$label[match(played$season_id, data$seasons$season_id)]
  )
  expect_equal(last$meta_line, leaf_texts(card, ".home-card-meta"))
  expect_equal(last$squad_size, summary$squad_size)
  expect_equal(last$scorers, if (nrow(summary$scorers) > 0) listed_scorers(summary$scorers))
  expect_equal(last$man_of_the_match, if (!is.na(summary$mom)) summary$mom)

  lines <- query_tags(card, ".home-detail")
  expect_rows(last$details, tibble(
    label = map_chr(lines, ~ .x$children[[1]]$children[[1]]),
    value = map_chr(lines, ~ as.character(.x$children[[2]]$children[[1]]))
  ), "last match details")
}

test_that("home is what the cards show", {
  home <- read_generated("home")
  expect_setequal(names(home), c(
    "last_match", "last_match_tag", "last_match_empty", "league_position", "league_link_label"
  ))
  expect_home_is_the_card(home, shiny_data)

  no_matches <- last_match_card_ui(NULL, shiny_data$seasons)
  expect_equal(home$last_match_tag, leaf_texts(no_matches, ".section-tag"))
  expect_equal(home$last_match_empty, leaf_texts(no_matches, ".empty-state"))

  strip <- query_tags(league_position_ui(shiny_data$league_table), "span")
  expect_equal(home$league_position, as.character(strip[[1]]$children[[1]]))
  link <- query_tags(league_position_ui(shiny_data$league_table), "a")
  expect_equal(home$league_link_label, trimws(gsub("<[^>]+>", "", as.character(link[[1]]))))
})

test_that("home drops what the last match does not have", {
  last_date <- max(shiny_data$matches$date)

  for (types in list("mom", "goal", c("goal", "mom"))) {
    data <- shiny_data
    data$events <- data$events |> filter(!(date == last_date & event_type %in% types))
    home <- round_trip(export_home(data))

    expect_home_is_the_card(home, data)
    if ("mom" %in% types) expect_null(home$last_match$man_of_the_match)
    if ("goal" %in% types) expect_null(home$last_match$scorers)
  }
})

test_that("the league position line follows the table", {
  data <- shiny_data
  data$league_table <- tibble(
    position = 1:3, team = c("Rovers", OUR_TEAM, "City"), played = c(4L, 4L, 3L),
    points = c(10L, 9L, 1L), scraped_on = as.Date("2031-03-01")
  )
  expect_equal(export_home(data)$league_position, "2nd of 3 \u00b7 9 points from 4")

  # Absent when the table does not list us, as the card is.
  data$league_table <- data$league_table[-2, ]
  expect_identical(export_home(data)$league_position, NA_character_)
  expect_identical(export_home(data)$league_link_label, NA_character_)
})

test_that("league rows and wording come from the table and the card", {
  league <- read_generated("league")
  standings <- shiny_data$league_table
  card <- league_table_ui(standings)
  no_table <- league_table_ui(standings[0, ])

  expect_setequal(names(league), c("scraped_on", "tag", "title", "subtitle", "empty", "rows"))
  expect_rows(
    league$rows, standings |> mutate(scraped_on = format(scraped_on, "%Y-%m-%d")), "league"
  )
  expect_equal(league$scraped_on, format(max(standings$scraped_on), "%Y-%m-%d"))
  expect_equal(league$tag, leaf_texts(card, ".section-tag"))
  expect_equal(league$title, leaf_texts(card, ".table-title"))
  expect_equal(league$subtitle, leaf_texts(card, ".table-subtitle"))
  expect_equal(league$empty, leaf_texts(no_table, ".empty-state"))
})

test_that("meta carries what the app holds as constants", {
  meta <- read_generated("meta")
  freshness <- freshness_note(shiny_data$matches, shiny_data$league_table)

  expect_setequal(names(meta), c(
    "data_hash", "our_team", "min_regression_appearances", "stale_fixtures_days", "outcome_labels",
    "seasons", "subset_files", "problems", "problems_title", "freshness", "empty_states"
  ))
  expect_equal(meta$our_team, OUR_TEAM)
  expect_equal(meta$min_regression_appearances, MIN_REGRESSION_APPEARANCES)
  expect_equal(meta$stale_fixtures_days, STALE_FIXTURES_DAYS)
  expect_equal(unlist(meta$outcome_labels), OUTCOME_LABELS)
  expect_equal(as.character(unlist(meta$problems)), as.character(shiny_data$problems))
  expect_equal(meta$problems_title, leaf_texts(data_health_banner("x"), ".data-health-title"))
  expect_setequal(names(meta$freshness), c("full", "short"))
  expect_equal(meta$freshness$full, leaf_texts(freshness, ".freshness-full"))
  expect_equal(meta$freshness$short, leaf_texts(freshness, ".freshness-short"))

  spans <- season_span(shiny_data$seasons, shiny_data$matches) |> arrange(desc(start_date))
  expect_rows(
    meta$seasons, spans |> mutate(start_date = format(start_date, "%Y-%m-%d")), "seasons"
  )

  no_scorers <- scorer_display_table(scorer_table(
    shiny_data$events[0, ], shiny_data$attendance[0, ], shiny_data$matches[0, ]
  ))
  expect_equal(meta$empty_states$no_scorers, no_scorers$x$options$language$emptyTable)
})

test_that("the Fees and League card wording is what the cards and tables say", {
  wording <- read_generated("wording")
  fee_cards <- wording$fee_cards
  league_cards <- wording$league_cards
  headings <- function(widget) unname(unlist(names(widget$x$data)))

  expect_setequal(names(fee_cards), c("summary_tiles", "payments", "charges", "everyone"))
  expect_setequal(names(league_cards), c("headings", "run_in"))

  # Against the real frames, which the export's stand-ins have to agree with.
  player <- unlist(read_generated("fees")$fee_players)[[1]]
  overview <- player_fee_overview(
    player, shiny_data$charges, shiny_data$payments, shiny_data$seasons
  )
  balances <- all_player_balances(shiny_data$charges, shiny_data$payments)

  summary_tiles <- fee_overview_summary(overview)
  expect_equal(
    unlist(fee_cards$summary_tiles, use.names = FALSE),
    leaf_texts(summary_tiles, ".fee-overview-label")
  )
  expect_equal(fee_cards$summary_tiles$charges, "Match charges")
  expect_equal(fee_cards$summary_tiles$payments, "Payments recorded")

  history <- query_tags(fee_history_panel, ".table-card")
  expect_equal(fee_cards$payments$tag, leaf_texts(history[[1]], ".section-tag"))
  expect_equal(fee_cards$payments$title, leaf_texts(history[[1]], ".table-title"))
  expect_null(fee_cards$payments$subtitle)
  expect_equal(fee_cards$charges$tag, leaf_texts(history[[2]], ".section-tag"))
  expect_equal(fee_cards$charges$title, leaf_texts(history[[2]], ".table-title"))
  expect_equal(fee_cards$charges$subtitle, leaf_texts(history[[2]], ".fee-history-note"))
  # In the order the tab has them, which a reorder of the panel would change.
  expect_equal(fee_cards$payments$title, "Payments made")
  expect_equal(fee_cards$charges$title, "Match-by-match charges")

  expect_equal(
    unlist(fee_cards$payments$headings, use.names = FALSE),
    headings(payment_history_table(overview$payment_history))
  )
  expect_equal(
    names(fee_cards$payments$headings),
    c("date", "payment")
  )
  charge_table <- match_charge_table(overview$match_charges)
  expect_equal(unlist(fee_cards$charges$headings, use.names = FALSE), headings(charge_table))
  expect_equal(fee_cards$charges$headings$played, "Played?")
  expect_equal(fee_cards$charges$headings$squad_size, "Squad size")
  expect_setequal(
    unlist(fee_cards$charges$played, use.names = FALSE),
    unique(charge_table$x$data$`Played?`)
  )
  expect_equal(fee_cards$charges$played$yes, "Yes")
  expect_equal(fee_cards$charges$played$no, "No")

  expect_equal(fee_cards$everyone$tag, leaf_texts(everyone_panel, ".section-tag"))
  expect_equal(fee_cards$everyone$title, leaf_texts(everyone_panel, ".table-title"))
  expect_equal(fee_cards$everyone$subtitle, leaf_texts(everyone_panel, ".table-subtitle"))
  expect_equal(
    unlist(fee_cards$everyone$tiles, use.names = FALSE),
    leaf_texts(balance_totals_ui(balance_totals(balances)), ".fee-overview-label")
  )
  expect_equal(fee_cards$everyone$tiles$outstanding, "Still owed")
  expect_equal(fee_cards$everyone$tiles$credit, "In credit")
  expect_equal(fee_cards$everyone$tiles$settled, "Settled up")
  expect_equal(
    unlist(fee_cards$everyone$headings, use.names = FALSE),
    headings(balance_table(balances))
  )
  expect_equal(fee_cards$everyone$headings$last_payment, "Last payment")

  expect_equal(
    unlist(league_cards$headings, use.names = FALSE),
    headings(league_table_table(shiny_data$league_table))
  )
  expect_equal(league_cards$headings$goal_difference, "GD")
  expect_equal(league_cards$headings$points, "Pts")

  expect_equal(league_cards$run_in$tag, leaf_texts(run_in_card, ".section-tag"))
  expect_equal(league_cards$run_in$title, leaf_texts(run_in_card, ".table-title"))
  expect_equal(league_cards$run_in$subtitle, leaf_texts(run_in_card, ".table-subtitle"))
  fixtures <- run_in(shiny_data$fixtures, shiny_data$matches, shiny_data$league_table, today = FAR_PAST)
  expect_equal(unlist(league_cards$run_in$headings, use.names = FALSE), headings(run_in_table(fixtures)))
  expect_equal(league_cards$run_in$headings$position, "Pos")
})

test_that("the wording file is what the source functions return", {
  wording <- read_generated("wording")
  expect_setequal(names(wording), c(
    "fee_panel", "settle_up", "estimator_control", "effects_cards", "plot_labels",
    "fee_cards", "league_cards", "matches_cards", "players_cards"
  ))

  fee_panel <- fee_panel_ui("a", "b")
  expect_rows(list(wording$fee_panel), tibble(
    tag = leaf_texts(fee_panel, ".section-tag"),
    picker_label = leaf_texts(fee_panel, ".control-label"),
    balance_label = leaf_texts(fee_panel, ".fee-label"),
    footnote = leaf_texts(fee_panel, ".fee-footnote")
  ), "fee panel")

  settle_up <- payment_panel_ui()
  expect_setequal(names(wording$settle_up), c("tag", "copy", "button_label", "button_url"))
  expect_equal(wording$settle_up$tag, leaf_texts(settle_up, ".section-tag"))
  expect_equal(wording$settle_up$copy, leaf_texts(settle_up, ".payment-copy"))
  expect_equal(wording$settle_up$button_label, leaf_texts(settle_up, ".payment-button"))
  expect_equal(
    wording$settle_up$button_url,
    query_tags(settle_up, ".payment-button")[[1]]$attribs$href
  )

  control <- estimator_control
  expect_setequal(names(wording$estimator_control), c("legend", "options"))
  expect_equal(wording$estimator_control$legend, leaf_texts(control, ".control-label"))
  expect_rows(wording$estimator_control$options, tibble(
    value = map_chr(query_tags(control, "input"), ~ .x$attribs$value),
    label = leaf_texts(control, ".radio-inline span")
  ), "estimator options")
  # These are the keys the season files use.
  expect_equal(strings_of(wording$estimator_control$options, "value"), c("ols", "ridge"))

  cards <- query_tags(player_effects_page, ".table-card")
  names(cards) <- c("attack_defence", "coefficient_plot", "player_effects", "method")
  expect_setequal(names(wording$effects_cards), names(cards))
  for (name in names(cards)) {
    expect_rows(list(wording$effects_cards[[name]]), tibble(
      tag = leaf_texts(cards[[name]], ".section-tag"),
      title = leaf_texts(cards[[name]], ".table-title"),
      subtitle = leaf_text_or_na(cards[[name]], ".table-subtitle"),
      picker_label = leaf_text_or_na(cards[[name]], ".control-label")
    ), name)
  }
  # That the names above are on the right cards.
  expect_equal(wording$effects_cards$attack_defence$title, "Attack and defence")
  expect_equal(wording$effects_cards$coefficient_plot$picker_label, "Outcome")
  expect_equal(wording$effects_cards$method$tag, "Method")

  # Against real results, which the export's stand-in has to agree with.
  everything <- shiny_data$seasons$season_id
  labels <- wording$plot_labels
  expect_setequal(names(labels), c("attack_defence", "coefficient", "season_form"))
  for (estimator in c("ols", "ridge")) {
    results <- expected_results(everything, estimator)
    expect_gt(nrow(results), 0)

    attack <- attack_defence_plot(results)$labels
    expect_equal(labels$attack_defence[[estimator]]$x, attack$x)
    expect_equal(labels$attack_defence[[estimator]]$y, attack$y)
    expect_type(labels$attack_defence[[estimator]]$y, "character")

    expect_setequal(names(labels$coefficient[[estimator]]), REGRESSION_OUTCOMES)
    for (this_outcome in REGRESSION_OUTCOMES) {
      coefficient <- coefficient_plot(results, this_outcome)$labels
      exported <- labels$coefficient[[estimator]][[this_outcome]]
      expect_true(all(c("x", "y") %in% names(exported)))
      expect_equal(exported$x, coefficient$x)
      expect_equal(exported$y, coefficient$y)
      expect_type(exported$y, "character")
    }
  }
})

test_that("the Matches card wording is what the cards and tables say", {
  cards <- read_generated("wording")$matches_cards
  headings <- function(widget) unname(unlist(names(widget$x$data)))
  shown <- function(group) unname(unlist(group))
  expect_setequal(names(cards), c(
    "season_form", "scorers", "head_to_head", "every_result", "match_detail"
  ))

  # Against the real frames, which the export's stand-ins have to agree with.
  everything <- shiny_data$seasons$season_id
  scope <- scoped_like_app(everything)
  form <- season_form_data(scope$attendance, scope$matches)
  newest <- as.character(max(form$date))
  detail <- match_detail_data(form, scope$attendance, newest)

  form_card <- cards$season_form
  expect_setequal(names(form_card), c("tag", "title", "subtitle", "metric_label", "metrics"))
  expect_equal(form_card$tag, leaf_texts(season_form_card, ".section-tag"))
  expect_equal(form_card$title, leaf_texts(season_form_card, ".table-title"))
  expect_equal(form_card$subtitle, leaf_texts(season_form_card, ".table-subtitle"))
  expect_equal(form_card$metric_label, leaf_texts(season_form_card, ".control-label"))
  expect_equal(form_card$metric_label, "Show")
  # Spelled out once, because the export reads them out of selectInput()'s string of HTML.
  expect_rows(form_card$metrics, tibble(
    value = c("points", "goals", "goal_difference", "squad_size"),
    label = c(
      "Points per match", "Goals scored and conceded", "Goal difference", "Squad size"
    )
  ), "metrics")

  # The scorers' subtitle is the coverage line, in each season file.
  expect_setequal(names(cards$scorers), c("tag", "title", "subtitle", "headings"))
  expect_equal(cards$scorers$tag, leaf_texts(scorer_card, ".section-tag"))
  expect_equal(cards$scorers$title, leaf_texts(scorer_card, ".table-title"))
  expect_null(cards$scorers$subtitle)
  expect_equal(
    shown(cards$scorers$headings),
    headings(scorer_display_table(
      scorer_table(scope$events, scope$attendance, scope$matches)
    ))
  )
  expect_setequal(
    names(cards$scorers$headings),
    c("player", "goals", "mom", "appearances", "goals_per_game")
  )

  expect_setequal(names(cards$head_to_head), c("tag", "title", "subtitle", "headings"))
  expect_equal(cards$head_to_head$tag, leaf_texts(opponent_record_card, ".section-tag"))
  expect_equal(cards$head_to_head$title, leaf_texts(opponent_record_card, ".table-title"))
  expect_equal(cards$head_to_head$subtitle, leaf_texts(opponent_record_card, ".table-subtitle"))
  expect_equal(
    shown(cards$head_to_head$headings),
    headings(opponent_record_table(opponent_record(scope$matches)))
  )

  expect_setequal(names(cards$every_result), c("tag", "title", "subtitle", "headings", "no_opponent"))
  expect_equal(cards$every_result$tag, leaf_texts(every_result_card, ".section-tag"))
  expect_equal(cards$every_result$title, leaf_texts(every_result_card, ".table-title"))
  expect_equal(cards$every_result$subtitle, leaf_texts(every_result_card, ".table-subtitle"))
  expect_equal(shown(cards$every_result$headings), headings(match_table(scope$matches)))
  # The mark on a real match from before the sync.
  no_opponent <- scope$matches |> filter(is.na(opponent)) |> slice(1)
  expect_equal(nrow(no_opponent), 1L)
  expect_equal(cards$every_result$no_opponent, match_table(no_opponent)$x$data$Opponent)

  expect_setequal(names(cards$match_detail), c(
    "tag", "title", "subtitle", "result_label", "lineup_title", "headings"
  ))
  expect_equal(cards$match_detail$tag, leaf_texts(match_detail_card, ".section-tag"))
  expect_equal(cards$match_detail$title, leaf_texts(match_detail_card, ".table-title"))
  expect_equal(cards$match_detail$subtitle, leaf_texts(match_detail_card, ".table-subtitle"))
  expect_equal(cards$match_detail$lineup_title, leaf_texts(match_detail_card, ".match-lineup-title"))
  expect_equal(
    cards$match_detail$result_label,
    leaf_texts(match_summary_ui(detail$match), ".match-detail-label")
  )
  expect_equal(shown(cards$match_detail$headings), headings(match_lineup_table(detail$lineup)))
})

test_that("the Players card wording is what the cards, tables and plot say", {
  cards <- read_generated("wording")$players_cards
  headings <- function(widget) unname(unlist(names(widget$x$data)))
  shown <- function(group) unname(unlist(group))
  expect_setequal(names(cards), c("who_turns_up", "player", "with_without"))

  # Against the real frames, which the export's stand-ins have to agree with.
  scope <- scoped_like_app(shiny_data$seasons$season_id)
  roster <- create_attendance_list(scope$attendance, scope$matches)
  profile <- player_profile(roster$player[[1]], scope$attendance, scope$matches, scope$events)

  # In the order player_page lays them out.
  expect_equal(
    c(cards$who_turns_up$tag, cards$player$tag, cards$with_without$tag),
    leaf_texts(player_page, ".section-tag")
  )
  expect_equal(
    c(cards$who_turns_up$title, cards$player$title, cards$with_without$title),
    leaf_texts(player_page, ".table-title")
  )
  # Only the first has a fixed subtitle: the other two are uiOutput()s, one of them the
  # note below and the other the note in each profile.
  expect_equal(cards$who_turns_up$subtitle, leaf_texts(player_page, ".table-subtitle"))
  expect_null(cards$player$subtitle)
  expect_null(cards$with_without$subtitle)

  expect_setequal(names(cards$who_turns_up), c("tag", "title", "subtitle", "headings"))
  expect_equal(
    shown(cards$who_turns_up$headings),
    setdiff(headings(attendance_table(roster)), "rank_class")
  )
  expect_equal(
    names(cards$who_turns_up$headings),
    c("player", "turnout", "avg_points", "avg_goals_scored", "avg_goals_conceded")
  )
  expect_equal(cards$who_turns_up$headings$turnout, "Turnout")
  expect_equal(cards$who_turns_up$headings$avg_goals_conceded, "Avg goals conceded")

  expect_setequal(names(cards$player), c(
    "tag", "title", "subtitle", "note_after_name", "appearances_title", "strip_key"
  ))
  expect_equal(cards$player$appearances_title, leaf_texts(player_page, ".match-lineup-title"))
  key <- appearance_timeline_plot(profile)$scales$get_scales("fill")$labels
  expect_equal(cards$player$strip_key$played, key[["TRUE"]])
  expect_equal(cards$player$strip_key$missed, key[["FALSE"]])
  expect_equal(cards$player$strip_key$played, "Played")
  expect_equal(cards$player$strip_key$missed, "Missed")

  # Only app.R writes this. If it rewords the note, this fails and the copy gets updated.
  source_text <- paste(readLines(here::here("app.R"), encoding = "UTF-8"), collapse = "\n")
  expect_equal(cards$player$note_after_name, APP_PLAYER_NOTE_AFTER_NAME)
  expect_true(grepl(
    paste0('paste0(profile_player(), "', APP_PLAYER_NOTE_AFTER_NAME, '")'),
    source_text,
    fixed = TRUE
  ))

  expect_setequal(names(cards$with_without), c("tag", "title", "subtitle", "headings", "sides"))
  both_sides <- with_without_table(profile)
  expect_equal(shown(cards$with_without$headings), headings(both_sides))
  expect_equal(cards$with_without$headings$points_per_match, "Pts/match")
  expect_equal(shown(cards$with_without$sides), both_sides$x$data[[" "]])
  expect_equal(cards$with_without$sides$played, "Played")
  expect_equal(cards$with_without$sides$missed, "Missed")
})

test_that("the season form labels are what season_form_plot() calls its axes and lines", {
  labels <- read_generated("wording")$plot_labels$season_form
  scope <- scoped_like_app(shiny_data$seasons$season_id)
  form <- season_form_data(scope$attendance, scope$matches)
  metrics <- strings_of(read_generated("wording")$matches_cards$season_form$metrics, "value")

  expect_setequal(names(labels), c(metrics, "series"))
  for (metric in metrics) {
    plot_labels <- season_form_plot(form, metric)$labels
    expect_equal(labels[[metric]]$y, plot_labels$y)
    expect_type(labels[[metric]]$y, "character")
    expect_true(all(c("x", "y") %in% names(labels[[metric]])))
    expect_null(labels[[metric]]$x)
  }

  # Each name belongs to the column whose values it is drawn with.
  goals <- season_form_plot(form, "goals")$data
  expect_setequal(names(labels$series), c("rolling_goals_scored", "rolling_goals_conceded"))
  for (column in names(labels$series)) {
    drawn <- goals |> filter(metric == labels$series[[column]]) |> arrange(date)
    expect_equal(drawn$value, form[[column]])
  }
})

test_that("the empty-state wording is still what app.R says", {
  # paste()d across lines there, so the pieces are joined up before looking. If
  # app.R rewords one, this fails and the copy in the export gets updated.
  source_text <- paste(readLines(here::here("app.R"), encoding = "UTF-8"), collapse = "\n")
  joined <- gsub('",\\s*"', " ", source_text)

  empty_states <- read_generated("meta")$empty_states
  for (name in names(APP_EMPTY_STATES)) {
    expect_equal(empty_states[[name]], APP_EMPTY_STATES[[name]])
    expect_true(grepl(APP_EMPTY_STATES[[name]], joined, fixed = TRUE), label = name)
  }
  expect_equal(
    empty_states$no_regression,
    "Not enough matches in the selected seasons to estimate player effects. Tick more seasons."
  )
})

# Reading wording out of tags ----

test_that("tag_text reads a real card", {
  card <- next_match_card_ui(NULL, have_fixtures = TRUE)
  empty <- "No fixtures left on the league page. Run the sync once the new season is up."

  expect_equal(tag_text(card, "empty-state"), empty)
  expect_equal(tag_text(card), c("Next match", empty))
  expect_equal(first_text(card, "section-tag"), "Next match")
  expect_identical(first_text(card, "no-such-class"), NA_character_)
  expect_identical(tag_text(NULL), character())
  expect_identical(tag_text(NULL, "empty-state"), character())
})

test_that("tag_text skips code, and finds a class among several", {
  markup <- div(
    class = "outer first",
    tags$script("var notWording = 1;"),
    tags$style(".also-not {}"),
    tagList(span(class = "a b", "one"), span(class = "b", "two", span("three"))),
    NULL,
    12L
  )

  expect_equal(tag_text(markup), c("one", "two", "three", "12"))
  expect_equal(tag_text(markup, "b"), c("one", "two three"))
  expect_equal(tag_text(markup, "first"), "one two three 12")
})

test_that("stat_tiles reads the tiles, a dropped note included", {
  profile <- list(
    appearances = 3L, matches = 7L, attendance_rate = 3 / 7, covered = 0L,
    goals = 0L, mom = 0L
  )
  tiles <- stat_tiles(player_headline_ui(profile))

  # A window before the sync has no goals tile: the card drops it.
  expect_length(tiles, 1)
  expect_equal(tiles[[1]]$label, "Appearances")
  expect_equal(tiles[[1]]$value, "3 of 7")
  expect_equal(tiles[[1]]$note, "43% turnout")

  fee_tiles <- stat_tiles(
    fee_overview_summary(list(total_charges = 1, total_payments = 2)), "fee-overview"
  )
  expect_identical(fee_tiles[[1]]$note, NA_character_)
})

# The stale-deploy check ----

# From the definition in the script, sharing none of its code: the file hashes
# and the final hash through tools rather than digest.
independent_hash <- function(data_dir) {
  paths <- c(
    Sys.glob(file.path(data_dir, "*.csv")), Sys.glob(file.path(data_dir, "archive", "*.csv"))
  )
  relative <- substring(paths, nchar(dirname(data_dir)) + 2L)
  in_order <- order(relative, method = "radix")
  manifest <- paste0(
    relative[in_order], "\n", unname(tools::sha256sum(paths[in_order])), "\n",
    collapse = ""
  )
  manifest_file <- withr::local_tempfile()
  writeBin(charToRaw(manifest), manifest_file)
  unname(tools::sha256sum(manifest_file))
}

copied_data <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  file.copy(here::here("data"), root, recursive = TRUE)
  file.path(root, "data")
}

test_that("data_hash follows its definition", {
  meta <- read_generated("meta")

  expect_equal(meta$data_hash, independent_hash(here::here("data")))
  expect_match(meta$data_hash, "^[0-9a-f]{64}$")
  expect_equal(meta$data_hash, built_one$data_hash)
})

test_that("a plain Node script reproduces data_hash", {
  skip_if(Sys.which("node") == "", "node is not on the path")

  script <- withr::local_tempfile(fileext = ".js")
  writeLines(c(
    "const fs = require('node:fs'), crypto = require('node:crypto'), path = require('node:path');",
    "const root = process.argv[2];",
    "const sha = (bytes) => crypto.createHash('sha256').update(bytes).digest('hex');",
    "const csvs = (dir) => fs.readdirSync(path.join(root, dir))",
    "  .filter((name) => name.endsWith('.csv') && !name.startsWith('.'))",
    "  .map((name) => `${dir}/${name}`);",
    "const files = [...csvs('data'), ...csvs('data/archive')].sort();",
    "console.log(sha(files.map((file) =>",
    "  `${file}\\n${sha(fs.readFileSync(path.join(root, file)))}\\n`).join('')));"
  ), script)

  from_node <- system2(
    "node", c(shQuote(script), shQuote(here::here())),
    stdout = TRUE, timeout = CHILD_TIMEOUT_SECONDS
  )
  expect_equal(from_node, read_generated("meta")$data_hash)
})

test_that("data_hash changes when any CSV changes, and only then", {
  data_dir <- copied_data()
  original <- data_hash(data_dir)
  expect_equal(original, independent_hash(data_dir))
  # The path is hashed relative to the repository, so a copy elsewhere agrees.
  expect_equal(original, data_hash(here::here("data")))

  writeLines("not data", file.path(data_dir, "notes.txt"))
  expect_equal(data_hash(data_dir), original)

  cat("04/11/2026,Ben,1\n", file = file.path(data_dir, "payments.csv"), append = TRUE)
  changed <- data_hash(data_dir)
  expect_false(identical(changed, original))
  expect_equal(changed, independent_hash(data_dir))

  # The superset of the plan's wording: a stale ledger has to be caught too.
  cat("\n", file = file.path(data_dir, "archive", "charges_s1.csv"), append = TRUE)
  expect_false(identical(data_hash(data_dir), changed))
})

# A window with nothing in it ----

test_that("a season with no matches exports empty states rather than zeros", {
  data_dir <- write_tiny_data()
  out <- withr::local_tempdir()
  expect_no_warning(build_site_data(data_dir, out))

  empty <- jsonlite::fromJSON(
    file.path(out, "public", "data", "seasons", "s2.json"), simplifyVector = FALSE
  )
  expect_equal(unlist(empty$season_ids), "s2")
  expect_false(empty$has_matches)
  expect_false(empty$regression$ols$has_regression)
  expect_false(empty$regression$ridge$has_regression)
  expect_length(empty$regression$ols$players, 0)
  expect_length(empty$regression$ridge$players, 0)
  for (field in c("matches", "season_form", "match_details", "opponent_record",
                  "attendance_list", "profiles")) {
    expect_length(empty[[field]], 0)
  }
  expect_length(empty$scorers$rows, 0)
  expect_equal(empty$scorers$coverage$observed, 0)
  expect_match(empty$scorers$note, "No match in the selected seasons", fixed = TRUE)

  # The raw text as well: an empty array has to be [] and not {} in the file.
  text <- paste(readLines(file.path(out, "public", "data", "seasons", "s2.json")), collapse = "\n")
  expect_match(text, '"matches": []', fixed = TRUE)
  expect_match(text, '"profiles": []', fixed = TRUE)
})

test_that("a problem in the data reaches the banner and does not stop the export", {
  data_dir <- write_tiny_data()
  out <- withr::local_tempdir()
  build_site_data(data_dir, out)

  meta <- jsonlite::fromJSON(
    file.path(out, "src", "generated", "meta.json"), simplifyVector = FALSE
  )
  problems <- unlist(meta$problems)
  expect_true("attendance for an unknown player: Ghost" %in% problems)
  expect_equal(meta$problems_title, "Data check")

  # A lone problem is still a list of problems for the banner to loop over.
  one_problem <- write_tiny_data(ghost = FALSE)
  writeLines(
    c("date,opponent,dl_match_id,kickoff,pitch", "07/01/2026,Rovers,1,20:00,Pitch 1"),
    file.path(one_problem, "fixtures.csv")
  )
  build_site_data(one_problem, out)
  text <- paste(readLines(file.path(out, "src", "generated", "meta.json")), collapse = "\n")
  expect_match(text, '"problems": ["fixtures dated on a match already recorded', fixed = TRUE)
})

test_that("a run on another day is byte-identical", {
  # Two runs on one day cannot catch a date stamp between them, whatever its format.
  # So the clock is moved, in a process of its own because this rebinds base R's.
  out <- withr::local_tempdir()
  runner <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    "moved <- list(",
    "  Sys.time = function() as.POSIXct('2031-06-15 12:00:00', tz = 'UTC'),",
    "  Sys.Date = function() as.Date('2031-06-15'),",
    "  date = function() 'Sun Jun 15 12:00:00 2031'",
    ")",
    "for (name in names(moved)) {",
    "  unlockBinding(name, baseenv())",
    "  assign(name, moved[[name]], envir = baseenv())",
    "}",
    "stopifnot(format(Sys.Date()) == '2031-06-15')",
    "source('scripts/build-site-data.R', local = TRUE)",
    paste0("build_site_data('data', ", deparse(out), ")")
  ), runner)

  result <- run_rscript(shQuote(runner), here::here())

  expect_equal(result$status, 0L, info = result$text)
  expect_identical(snapshot_folder(out), snapshot_folder(file.path(run_one, "site")))
})

# Rows that stop the export ----

# Adds the row each kind of stop needs to crafted data that has nothing wrong with it.
STOPPING_ROWS <- list(
  list(
    prefix = "matches recorded more than once",
    file = "matches.csv", row = "07/01/2026,,1,1,",
    named = "matches recorded more than once: 07/01/2026. Fix data/matches.csv."
  ),
  list(
    prefix = "attendance recorded twice for the same player and date",
    file = "attendance.csv", row = "07/01/2026,Ann",
    named = paste(
      "attendance recorded twice for the same player and date: Ann 07/01/2026.",
      "Fix data/attendance.csv."
    )
  )
)

add_row_to <- function(data_dir, file, row) {
  cat(paste0(row, "\n"), file = file.path(data_dir, file), append = TRUE)
}

# The folder around crafted data made into a project, so that the real command can
# run in it. Without .Rprofile or renv/: a child reading those in a copy would have
# renv install itself under ~/.local/share/renv.
make_project_around <- function(data_dir) {
  root <- dirname(data_dir)
  file.copy(here::here("R"), root, recursive = TRUE)
  dir.create(file.path(root, "scripts"))
  file.copy(here::here("scripts", "build-site-data.R"), file.path(root, "scripts"))
  file.copy(here::here("borulanta.Rproj"), root)
  root
}

# Rscript scripts/build-site-data.R, the script's own main guard included.
run_export_command <- function(root) run_rscript("scripts/build-site-data.R", root)

test_that("each stopping prefix still starts what validate_app_data() says", {
  # The failure that matters most here. If R/data.R rewords one of these, the problem
  # goes back to the banner, nothing stops, and fees publish wrongly without a sound.
  expect_setequal(names(STOPPING_PROBLEMS), map_chr(STOPPING_ROWS, "prefix"))

  for (kind in STOPPING_ROWS) {
    data_dir <- write_two_season_data()
    add_row_to(data_dir, kind$file, kind$row)
    problems <- withr::with_dir(dirname(data_dir), suppressWarnings(load_app_data()))$problems

    expect_true(any(startsWith(problems, paste0(kind$prefix, ": "))), label = kind$prefix)
    # Each one names the file it would tell you to fix, and that file exists.
    expect_true(file.exists(file.path(dirname(data_dir), STOPPING_PROBLEMS[[kind$prefix]])))
  }
})

test_that("the real command exits 0 on clean data, so a non-zero exit below means something", {
  root <- make_project_around(write_two_season_data())
  expect_false(any(file.exists(file.path(root, c(".Rprofile", "renv")))))

  result <- run_export_command(root)

  expect_equal(result$status, 0L, info = result$text)
  expect_true(file.exists(file.path(root, "site", "src", "generated", "meta.json")))
})

test_that("each repeated row stops the real command, names the row and writes nothing", {
  for (kind in STOPPING_ROWS) {
    data_dir <- write_two_season_data()
    root <- make_project_around(data_dir)
    site <- file.path(root, "site")

    # A populated folder, with a file a finished run would have deleted as stale.
    build_site_data(data_dir, site)
    writeLines("{}", file.path(site, "public", "data", "seasons", "s99.json"))
    before <- snapshot_folder(site)

    add_row_to(data_dir, kind$file, kind$row)
    result <- run_export_command(root)

    # R's own status for an error, which a timeout's 124 is not.
    expect_equal(result$status, 1L, info = result$text)
    expect_match(result$text, kind$named, fixed = TRUE)
    expect_match(result$text, "Nothing was written.", fixed = TRUE)
    expect_identical(snapshot_folder(site), before)
  }
})

test_that("both kinds at once are both named, every row of each", {
  data_dir <- write_two_season_data()
  add_row_to(data_dir, "matches.csv", "07/01/2026,,1,1,")
  add_row_to(data_dir, "matches.csv", "14/01/2026,,1,1,")
  add_row_to(data_dir, "attendance.csv", "21/01/2026,Ann")
  out <- withr::local_tempdir()

  error <- tryCatch(build_site_data(data_dir, out), error = conditionMessage)

  # In the match table's order, which is newest first.
  expect_match(error, "more than once: 14/01/2026, 07/01/2026. Fix data/matches.csv.", fixed = TRUE)
  expect_match(error, "and date: Ann 21/01/2026. Fix data/attendance.csv.", fixed = TRUE)
  expect_length(list.files(out, recursive = TRUE), 0)
})

test_that("only a problem that starts with a prefix stops the export", {
  expect_silent(stop_on_wrong_fees(character()))
  expect_silent(stop_on_wrong_fees(c(
    "attendance for an unknown player: Ghost",
    # The wording alone in the middle of another problem is not that problem.
    "payments from an unknown player: matches recorded more than once: Ghost"
  )))
  expect_error(
    stop_on_wrong_fees("matches recorded more than once: 07/01/2026"),
    "Nothing was written"
  )
})

test_that("every other problem validate_app_data() can list goes in the banner", {
  data_dir <- write_two_season_data()
  add <- function(name, ...) {
    cat(paste0(c(...), "\n"), file = file.path(data_dir, name), sep = "", append = TRUE)
  }
  add("matches.csv", "01/01/2020,,1,1,")
  add("attendance.csv", "01/01/2020,Ann", "15/01/2026,Ann", "21/01/2026,Ghost")
  add("payments.csv", "01/01/2020,Ann,5", "08/01/2026,Ghost,5")
  add("player_seasons.csv", "s1,Nobody,FALSE,TRUE", "s9,Ann,TRUE,TRUE")
  add(
    "match_events.csv",
    "15/01/2026,,us,10,goal,Ann", "21/01/2026,1,us,10,goal,Ghost", "28/01/2026,2,them,10,goal,Them"
  )
  writeLines(
    c("date,opponent,dl_match_id,kickoff,pitch", "07/01/2026,Rovers,1,20:00,Pitch 1"),
    file.path(data_dir, "fixtures.csv")
  )
  writeLines(c(
    "position,team,played,won,drawn,lost,goals_for,goals_against,goal_difference,points,scraped_on",
    "1,Rovers,1,1,0,0,2,0,2,3,01/03/2026"
  ), file.path(data_dir, "league_table.csv"))
  writeLines(c(
    "date,home_team,away_team,home_goals,away_goals,dl_match_id",
    "07/01/2026,Rovers,City,2,0,9", "07/01/2026,Rovers,City,2,0,9"
  ), file.path(data_dir, "league_results.csv"))
  out <- withr::local_tempdir()

  expect_no_error(suppressWarnings(build_site_data(data_dir, out)))

  problems <- as.character(unlist(read_from(out, "src", "generated", "meta.json")$problems))
  for (kind in c(
    "matches dated before the first season",
    "attendance dated before the first season",
    "payments dated before the first season",
    "attendance for a date with no match",
    "attendance for an unknown player",
    "payments from an unknown player",
    "roster entries for an unknown player",
    "roster entries for an unknown season",
    "match events for a date with no match",
    "match events for an unknown player",
    "match events for a team other than ours",
    "goal events that do not add up to the scoreline",
    "fixtures dated on a match already recorded",
    "missing from data/league_table.csv",
    "league results recorded more than once",
    "players in attendance but not on that season's active roster"
  )) {
    expect_true(any(startsWith(problems, kind)), label = kind)
  }
  expect_setequal(problems, suppressWarnings(load_export_data(data_dir))$problems)
  for (prefix in names(STOPPING_PROBLEMS)) {
    expect_false(any(startsWith(problems, prefix)), label = prefix)
  }
})

test_that("a warning that is not a data problem still surfaces", {
  data_dir <- write_tiny_data()
  cat("21/01/2026,,x,1,\n", file = file.path(data_dir, "matches.csv"), append = TRUE)
  expect_warning(load_export_data(data_dir), "parsing issues")
})

test_that("load_export_data refuses a folder archived_charges() could not read from", {
  expect_error(load_export_data(file.path(tempdir(), "elsewhere")), "called data")
})

test_that("a fresh checkout with nothing played and no snapshots still exports", {
  data_dir <- write_tiny_data(with_matches = FALSE)
  out <- withr::local_tempdir()
  # season_span() warns on a match table with no rows at all, then answers correctly.
  suppressWarnings(build_site_data(data_dir, out))
  read <- function(name) {
    jsonlite::fromJSON(
      file.path(out, "src", "generated", paste0(name, ".json")), simplifyVector = FALSE
    )
  }

  home <- read("home")
  expect_true("last_match" %in% names(home))
  expect_null(home$last_match)
  expect_equal(home$last_match_empty, "No matches recorded yet.")
  expect_null(home$league_position)

  fixtures <- read("fixtures")
  expect_false(fixtures$have_fixtures)
  expect_length(fixtures$fixtures, 0)
  expect_null(fixtures$scraped_on)
  expect_null(fixtures$stale_note)

  league <- read("league")
  expect_length(league$rows, 0)
  expect_null(league$title)
  expect_equal(league$empty, tag_text(league_table_ui(empty_standings()), "empty-state"))

  meta <- read("meta")
  expect_length(meta$problems, 0)
  expect_null(meta$freshness$full)

  # Ann paid without having been charged, so she is in credit.
  fees <- read("fees")
  expect_equal(fees$balances[[1]]$player, "Ann")
  expect_equal(fees$balances[[1]]$balance, -10)
  expect_equal(fees$balances[[1]]$balance_display, "-\u00a310.00")
  expect_equal(fees$totals$credit, 10)
})

test_that("each match page's card is what the last-match card would say on that night", {
  rows <- read_generated("matches")
  played <- shiny_data$matches |> filter(!is.na(season_id))

  expect_equal(strings_of(rows, "date"), format(sort(played$date, decreasing = TRUE), "%Y-%m-%d"))

  for (row in rows) {
    summary <- last_match_summary(
      played |> filter(date == as.Date(row$date)), shiny_data$attendance, shiny_data$events
    )
    card <- last_match_card_ui(summary, shiny_data$seasons)
    expect_setequal(names(row), c(
      "season_id", "date", "date_label", "result", "points", "badge", "opponent",
      "season_label", "meta_line", "squad_size", "scorers", "man_of_the_match", "details"
    ))

    expect_equal(row$season_id, summary$match$season_id)
    expect_equal(row$date_label, format_match_date(summary$match$date))
    expect_equal(row$result, summary$match$result)
    expect_equal(row$points, summary$match$points)
    expect_equal(row$badge, leaf_texts(result_badge(summary$match$points), "span"))
    if (is.na(summary$match$opponent)) {
      expect_null(row$opponent)
    } else {
      expect_equal(row$opponent, summary$match$opponent)
    }
    expect_equal(
      row$season_label,
      shiny_data$seasons$label[match(summary$match$season_id, shiny_data$seasons$season_id)]
    )
    expect_equal(row$meta_line, leaf_texts(card, ".home-card-meta"))
    expect_equal(row$squad_size, summary$squad_size)
    expect_equal(row$squad_size, n_distinct(
      shiny_data$attendance$player[shiny_data$attendance$date == as.Date(row$date)]
    ))

    # What the card shows, and what it drops.
    if (nrow(summary$scorers) > 0) {
      expect_equal(row$scorers, listed_scorers(summary$scorers))
    } else {
      expect_null(row$scorers)
    }
    if (is.na(summary$mom)) {
      expect_null(row$man_of_the_match)
    } else {
      expect_equal(row$man_of_the_match, summary$mom)
    }
    lines <- query_tags(card, ".home-detail")
    expect_equal(length(row$details), length(lines))
    for (index in seq_along(lines)) {
      expect_equal(
        c(row$details[[index]]$label, row$details[[index]]$value),
        as.character(unlist(tag_text(lines[[index]])))
      )
    }
  }

  # The newest is the card Home shows, field for field.
  home_last <- read_generated("home")$last_match
  rows[[1]]$season_id <- NULL
  expect_equal(rows[[1]], home_last)
})

test_that("every match page's lineup has its entry in that match's own season file", {
  for (row in read_generated("matches")) {
    season_file <- read_subset(row$season_id)
    expect_true(row$date %in% strings_of(season_file$match_details, "date"))
  }
})

test_that("a match dated before the first season stays out of the match pages", {
  data <- shiny_data
  early <- data$matches |> slice(1) |> mutate(date = as.Date("2001-01-03"), season_id = NA_character_)
  data$matches <- bind_rows(data$matches, early)

  rows <- export_match_pages(data)
  expect_false("2001-01-03" %in% map_chr(rows, "date"))
  expect_equal(length(rows), sum(!is.na(shiny_data$matches$season_id)))
})

test_that("a synced match with no attendance yet has its page, with nobody in the lineup", {
  data <- shiny_data
  newest <- max(data$matches$date)
  data$attendance <- data$attendance |> filter(date != newest)

  rows <- round_trip(export_match_pages(data))
  expect_equal(rows[[1]]$date, format(newest, "%Y-%m-%d"))
  expect_equal(rows[[1]]$squad_size, 0L)

  scope <- scope_to_seasons(data, rows[[1]]$season_id)
  details <- round_trip(export_match_details(
    season_form_data(scope$attendance, scope$matches), scope$attendance
  ))
  expect_equal(details[[1]]$date, rows[[1]]$date)
  expect_length(details[[1]]$lineup, 0)
})

test_that("a last match with no opponent or events drops those from the card", {
  data_dir <- write_tiny_data()
  out <- withr::local_tempdir()
  build_site_data(data_dir, out)
  last <- jsonlite::fromJSON(
    file.path(out, "src", "generated", "home.json"), simplifyVector = FALSE
  )$last_match

  expect_equal(last$result, "2-2")
  expect_equal(last$badge, "DRAW")
  expect_null(last$opponent)
  expect_null(last$scorers)
  expect_null(last$man_of_the_match)
  expect_equal(last$season_label, "Season 1")
  expect_equal(strings_of(last$details, "label"), "Squad")
  expect_equal(last$details[[1]]$value, "2 players")
})

# The attendance guard in scripts/publish-site.sh ----

test_that("matches_without_attendance() names matches with no row, oldest first", {
  matches <- tibble(date = as.Date(c("2026-03-04", "2026-01-07", "2026-02-04")))
  attendance <- tibble(date = as.Date("2026-02-04"), player = c("Ann", "Bob"))

  expect_identical(
    matches_without_attendance(matches, attendance),
    as.Date(c("2026-01-07", "2026-03-04"))
  )
  expect_length(matches_without_attendance(matches[3, ], attendance), 0)
  # Attendance with no match is a different problem, and it goes in the banner.
  expect_length(matches_without_attendance(matches[0, ], attendance), 0)
})

test_that("a real match with its attendance taken away is named", {
  newest <- max(shiny_data$matches$date)
  rest <- shiny_data$attendance |> filter(date != newest)

  expect_true(newest %in% matches_without_attendance(shiny_data$matches, rest))
})

# Rscript scripts/build-site-data.R --matches-without-attendance, as the guard runs it.
ask_for_missing_attendance <- function(root) {
  result <- run_rscript(c("scripts/build-site-data.R", "--matches-without-attendance"), root)
  lines <- strsplit(result$text, "\n", fixed = TRUE)[[1]]
  c(result, list(dates = grep("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", lines, value = TRUE)))
}

test_that("the guard's command names the dates and writes nothing, and the export carries on", {
  data_dir <- write_two_season_data()
  root <- make_project_around(data_dir)

  clean <- ask_for_missing_attendance(root)
  expect_equal(clean$status, 0L, info = clean$text)
  expect_length(clean$dates, 0)

  add_row_to(data_dir, "matches.csv", "22/04/2026,,1,1,")
  add_row_to(data_dir, "matches.csv", "15/04/2026,,1,1,")
  missing <- ask_for_missing_attendance(root)
  expect_equal(missing$status, 0L, info = missing$text)
  expect_identical(missing$dates, c("15/04/2026", "22/04/2026"))
  expect_false(file.exists(file.path(root, "site")))

  # Ben's decision: only a repeated match date or attendance row stops the export.
  exported <- run_export_command(root)
  expect_equal(exported$status, 0L, info = exported$text)
  expect_true(file.exists(file.path(root, "site", "src", "generated", "meta.json")))
})

test_that("the guard's command leaves a repeated row for the export to stop on", {
  data_dir <- write_two_season_data()
  root <- make_project_around(data_dir)
  add_row_to(data_dir, "attendance.csv", "07/01/2026,Ann")

  asked <- ask_for_missing_attendance(root)

  expect_equal(asked$status, 0L, info = asked$text)
  expect_length(asked$dates, 0)
})
