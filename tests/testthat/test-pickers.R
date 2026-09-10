# Two tables are pickers now — Every result drives the match detail card, and
# the roster drives the player page — so the thing to hold still is that a row
# index means the same to the server as it does to the reader looking at it.
#
# These run the real server against the real files. testServer() drives the
# reactives directly, which is the only way to reach this wiring: the outputs
# it feeds are inside conditionalPanels on a tab, and a headless browser
# reports them hidden and suspends them.

app_dir <- here::here()

test_that("the match picker defaults to the newest match", {
  testServer(app_dir, {
    session$setInputs(seasons = c("s1", "s2", "s3"))
    expect_equal(selected_match_date(), as.character(max(scoped()$matches$date)))
  })
})

test_that("the match picker follows the selected row", {
  testServer(app_dir, {
    session$setInputs(seasons = c("s1", "s2", "s3"))
    matches <- scoped()$matches

    session$setInputs(matches_rows_selected = 5L)
    expect_equal(selected_match_date(), as.character(matches$date[[5]]))
    expect_equal(selected_match_details()$match$date, matches$date[[5]])

    session$setInputs(matches_rows_selected = 1L)
    expect_equal(selected_match_date(), as.character(matches$date[[1]]))
  })
})

test_that("a cleared or stale match selection falls back to the newest", {
  # Changing the season clears the selection, and the row that was selected
  # may be past the end of the shorter table. An empty detail card under a
  # full table reads as a fault rather than as nothing being chosen.
  testServer(app_dir, {
    session$setInputs(seasons = c("s1", "s2", "s3"))
    session$setInputs(matches_rows_selected = integer(0))
    expect_equal(selected_match_date(), as.character(max(scoped()$matches$date)))

    session$setInputs(matches_rows_selected = 9999L)
    expect_equal(selected_match_date(), as.character(max(scoped()$matches$date)))
  })
})

test_that("the player picker follows the selected roster row", {
  testServer(app_dir, {
    session$setInputs(seasons = c("s1", "s2", "s3"))
    roster <- roster_list()
    expect_gt(nrow(roster), 1)

    session$setInputs(attendance_list_rows_selected = 3L)
    expect_equal(profile_player(), roster$player[[3]])
    expect_equal(profile()$player, roster$player[[3]])

    session$setInputs(attendance_list_rows_selected = 1L)
    expect_equal(profile_player(), roster$player[[1]])
  })
})

test_that("no roster selection lands on the top of the table", {
  testServer(app_dir, {
    session$setInputs(seasons = c("s1", "s2", "s3"))
    session$setInputs(attendance_list_rows_selected = integer(0))
    expect_equal(profile_player(), roster_list()$player[[1]])
  })
})

test_that("both pickers open on a row rather than on nothing", {
  # The page under each of these reads the selected row. Opening with nothing
  # picked would show the fallback below a table with no row highlighted, so
  # the highlight and the page would disagree about what is being shown.
  roster <- tibble(
    player = c("Ada", "Ben"), attendance_rate = c(0.9, 0.5),
    avg_points = 2, avg_goals_scored = 3, avg_goals_conceded = 2
  )
  expect_equal(attendance_table(roster)$x$selection$selected, 1L)
  expect_equal(attendance_table(roster, selected_row = 2L)$x$selection$selected, 2L)

  matches <- with_result(tibble(
    date = as.Date(c("2026-09-09", "2026-09-02")), opponent = "Someone",
    goals_for = 2L, goals_against = 1L, dl_match_id = "x"
  ))
  expect_equal(match_table(matches)$x$selection$selected, 1)
})

test_that("the roster opens on the fee player where they turned out", {
  testServer(app_dir, {
    session$setInputs(seasons = c("s1", "s2", "s3"))
    roster <- roster_list()
    # Whoever the fee picker landed on: in scope here, their row; out of scope,
    # the top of the table. The two lists are allowed to disagree about who
    # exists, so both branches are real.
    seed <- dplyr::coalesce(match(selected_fee_player(), roster$player), 1L)

    # testServer hands back the serialised widget rather than the object.
    rendered <- jsonlite::fromJSON(output$attendance_list)
    expect_equal(rendered$x$selection$selected, seed)

    # And once the browser echoes that preselected row back as an input —
    # which is the half testServer has no client to do — the page agrees with
    # the row the table has highlighted.
    session$setInputs(attendance_list_rows_selected = seed)
    expect_equal(profile_player(), roster$player[[seed]])
  })
})

test_that("the season picker only claims to apply where it does", {
  testServer(app_dir, {
    session$setInputs(seasons = c("s1", "s2", "s3"))

    session$setInputs(app_tabs = "Home")
    expect_false(season_picker_applies())
    session$setInputs(app_tabs = "Fees")
    expect_false(season_picker_applies())

    for (tab in c("Matches", "Players", "Player effects")) {
      session$setInputs(app_tabs = tab)
      expect_true(season_picker_applies())
    }
  })
})

test_that("the roster is scoped by the season picker and the fee list is not", {
  # The two pickers answer different questions and are allowed to disagree
  # about who exists: a balance is a running total, a player page is about
  # matches that happened.
  testServer(app_dir, {
    session$setInputs(seasons = "s3")
    expect_true(all(roster_list()$player %in% scoped()$attendance$player))

    session$setInputs(seasons = c("s1", "s2", "s3"))
    expect_gte(nrow(roster_list()), 1)
  })
})
