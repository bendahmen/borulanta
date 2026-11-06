# Goals that count for the team and for no player: an own goal, which the page
# lists under a name that is nobody's, and a guest's goal, which it lists under
# the name of one of ours. Both are credited to nobody as match_events.csv is
# read, so these go through load_app_data() on files in a temporary folder.

# One match won 3-1, with Ben and Daniel on the roster and both playing.
write_goal_data <- function(events, uncredited = NULL, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  write_file <- function(name, ...) writeLines(c(...), file.path(dir, name))

  write_file(
    "seasons.csv",
    "season_id,label,start_date,fee_rules,status",
    "s1,Season 1,01/01/2026,even_split,open"
  )
  write_file("players.csv", "player,guest", "Ben,FALSE", "Daniel,FALSE")
  write_file(
    "player_seasons.csv", "season_id,player,core,active", "s1,Ben,TRUE,TRUE", "s1,Daniel,TRUE,TRUE"
  )
  write_file(
    "matches.csv", "date,opponent,goals_for,goals_against,dl_match_id", "14/01/2026,Ball FC,3,1,1"
  )
  write_file("attendance.csv", "date,player", "14/01/2026,Ben", "14/01/2026,Daniel")
  write_file("payments.csv", "date,player,amount")
  write_file("match_events.csv", "date,dl_match_id,team,minute,event_type,player", events)
  if (!is.null(uncredited)) {
    write_file("uncredited_goals.csv", uncredited)
  }
  dir
}

GUEST_DANIEL <- c(
  "date,minute,player,reason",
  "14/01/2026,20,Daniel,\"A guest also called Daniel, not on the roster\""
)

test_that("an own goal is credited to nobody and still adds up to the scoreline", {
  dir <- write_goal_data(c(
    "14/01/2026,1,us,10,goal,Ben",
    "14/01/2026,1,us,20,goal,Og",
    "14/01/2026,1,us,30,goal,Daniel"
  ))

  expect_warning(data <- load_app_data(dir), NA)

  # No unknown player in the banner, and three goal rows for three goals.
  expect_identical(data$problems, character())
  expect_equal(data$events$player, c("Ben", NA, "Daniel"))
  expect_false(anyNA(scorer_table(data$events, data$attendance, data$matches)$player))
})

test_that("every spelling of an own goal is credited to nobody, and only goals", {
  events <- tibble(
    date = as.Date("2026-01-14"), minute = c(10L, 20L, 30L, 40L, NA),
    event_type = c("goal", "goal", "goal", "goal", "mom"),
    player = c("OG", "o.g.", "Own Goal", "Ben", "Og")
  )

  credited <- credit_nobody(events, empty_uncredited_goals())

  expect_equal(credited$player, c(NA, NA, NA, "Ben", "Og"))
})

test_that("a listed guest's goal is credited to nobody, and Daniel keeps his own", {
  dir <- write_goal_data(
    c(
      "14/01/2026,1,us,10,goal,Ben",
      "14/01/2026,1,us,20,goal,Daniel",
      "14/01/2026,1,us,30,goal,Daniel"
    ),
    GUEST_DANIEL
  )

  expect_warning(data <- load_app_data(dir), NA)

  expect_identical(data$problems, character())
  expect_equal(data$events$player, c("Ben", NA, "Daniel"))
  expect_true(data$uncredited_goals$matched)

  table <- scorer_table(data$events, data$attendance, data$matches)
  expect_false(anyNA(table$player))
  expect_equal(table$goals[table$player == "Daniel"], 1L)
  expect_equal(
    player_profile("Daniel", data$attendance, data$matches, data$events)$goals, 1L
  )
  expect_equal(
    last_match_summary(data$matches, data$attendance, data$events)$scorers,
    tibble(player = c("Ben", "Daniel"), goals = 1L)
  )
})

test_that("a listed goal that no longer matches is named in the banner", {
  # Say the sync corrected the minute on the page from 20 to 21.
  dir <- write_goal_data(
    c(
      "14/01/2026,1,us,10,goal,Ben",
      "14/01/2026,1,us,21,goal,Daniel",
      "14/01/2026,1,us,30,goal,Daniel"
    ),
    GUEST_DANIEL
  )

  expect_warning(data <- load_app_data(dir), "match no goal event")

  expect_identical(
    data$problems,
    "goals in data/uncredited_goals.csv that match no goal event: Daniel 14/01/2026 minute 20"
  )
  # Credited to the roster Daniel again until the list is put right.
  expect_equal(data$events$player, c("Ben", "Daniel", "Daniel"))
})

test_that("no list, an empty file or a header alone credits only own goals to nobody", {
  events <- c("14/01/2026,1,us,10,goal,Ben", "14/01/2026,1,us,20,goal,Og",
              "14/01/2026,1,us,30,goal,Daniel")

  for (uncredited in list(NULL, character(), "date,minute,player,reason")) {
    dir <- write_goal_data(events, uncredited)
    if (identical(uncredited, character())) {
      expect_equal(file.size(file.path(dir, "uncredited_goals.csv")), 0)
    }

    expect_warning(data <- load_app_data(dir), NA)

    expect_identical(data$problems, character())
    expect_equal(data$events$player, c("Ben", NA, "Daniel"))
    expect_equal(nrow(data$uncredited_goals), 0)
  }
})
