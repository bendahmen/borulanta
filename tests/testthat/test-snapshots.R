# The two snapshot files the fixtures and the league table are read out of.

test_that("the fixture snapshot keeps every fixture and drops the scores", {
  # Unplayed fixtures included: the site decides which is next, using the date
  # it is actually being read on rather than the date the sync ran.
  fixtures <- tibble(
    date = as.Date(c("2026-09-16", "2026-09-09")),
    dl_match_id = c("96099", "96094"),
    opponent = c("Brother Man FC", "Shamrock Posers"),
    goals_for = c(0L, 4L),
    goals_against = c(0L, 2L)
  )

  snapshot <- snapshot_tables(fixtures, empty_standings(), as.Date("2026-09-10"))

  expect_equal(names(snapshot$fixtures), c("date", "opponent", "dl_match_id", "kickoff", "pitch"))
  expect_equal(snapshot$fixtures$date, as.Date(c("2026-09-09", "2026-09-16")))
  # matches.csv is the only source of truth for a result.
  expect_false(any(c("goals_for", "goals_against") %in% names(snapshot$fixtures)))
})

test_that("a fixture whose result is recorded drops off the list", {
  # The page keeps the fixture up all season with the score written into it.
  # Once the result is in matches.csv the fixture is history, and writing it
  # back would have the app filtering it out on every page that reads the file.
  fixtures <- tibble(
    date = as.Date(c("2026-09-09", "2026-09-16")),
    dl_match_id = c("96094", "96099"),
    opponent = c("Shamrock Posers", "Brother Man FC"),
    goals_for = c(2L, 0L), goals_against = c(3L, 0L)
  )

  snapshot <- snapshot_tables(fixtures, empty_standings(), as.Date("2026-09-10"),
                              recorded = as.Date("2026-09-09"))

  expect_equal(snapshot$fixtures$date, as.Date("2026-09-16"))
})

test_that("the standings snapshot is dated and ordered by position", {
  standings <- tibble(
    position = c(2L, 1L), team = c("Borulanta", "Ball FC"),
    played = 1L, won = c(0L, 1L), drawn = 0L, lost = c(1L, 0L),
    goals_for = c(2L, 5L), goals_against = c(5L, 2L),
    goal_difference = c(-3L, 3L), points = c(0L, 3L)
  )

  snapshot <- snapshot_tables(
    tibble(date = as.Date(character()), dl_match_id = character(),
           opponent = character(), goals_for = integer(), goals_against = integer()),
    standings, as.Date("2026-09-10")
  )

  expect_equal(snapshot$league_table$position, c(1L, 2L))
  expect_true(all(snapshot$league_table$scraped_on == as.Date("2026-09-10")))
})

test_that("both snapshot files round-trip through disk unchanged", {
  # The writers format dates as dd/mm/yyyy and the readers parse them back;
  # a mismatch there would be invisible until the home page showed a fixture
  # in the year 10.
  fixture_path <- withr::local_tempfile(fileext = ".csv")
  table_path <- withr::local_tempfile(fileext = ".csv")

  fixtures <- tibble(
    date = as.Date(c("2026-09-09", "2026-09-16")),
    dl_match_id = c("96094", "96099"),
    opponent = c("Shamrock Posers", "Brother Man FC"),
    goals_for = 0L, goals_against = 0L
  )
  standings <- tibble(
    position = 1L, team = "Borulanta", played = 1L, won = 1L, drawn = 0L,
    lost = 0L, goals_for = 4L, goals_against = 2L, goal_difference = 2L, points = 3L
  )

  snapshot <- snapshot_tables(fixtures, standings, as.Date("2026-09-10"))
  write_snapshot_files(snapshot, fixture_path, table_path)

  expect_equal(read_fixture_file(fixture_path), snapshot$fixtures)
  expect_equal(read_league_table_file(table_path), snapshot$league_table)
})

test_that("a missing snapshot file reads as empty rather than failing", {
  # True of a fresh clone, and of any checkout from before these files existed.
  # The app has to start either way.
  missing <- file.path(tempdir(), "definitely-not-here.csv")

  expect_equal(nrow(read_fixture_file(missing)), 0L)
  expect_equal(nrow(read_league_table_file(missing)), 0L)
  expect_s3_class(read_fixture_file(missing)$date, "Date")
})
