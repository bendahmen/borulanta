# Parsing the league page.
#
# Both fixtures are real pages, trimmed to the weeks needed: hoxton-played is a
# league mid-season with actual results in it, and shoreditch-unplayed is ours
# on the day the season opened — every fixture still 0 : 0, which is also
# exactly what the page looks like after a season rolls over and is wiped.

test_that("every fixture in a played league parses", {
  fixtures <- parse_fixtures(fixture_html("hoxton-played.html"))

  expect_equal(nrow(fixtures), 12) # four weeks, three fixtures a week
  expect_false(anyNA(fixtures$date))
  expect_false(anyNA(fixtures$home_goals))
  expect_false(anyNA(fixtures$away_goals))
  expect_true(all(nzchar(fixtures$home_team) & nzchar(fixtures$away_team)))
  expect_true(all(str_detect(fixtures$dl_match_id, "^\\d+$")))
})

test_that("goals logged reconcile with every scoreline", {
  # The strongest check available without a second source: the site records
  # each goal separately from the score it displays, so if the parser is
  # picking up the wrong rows the two will not agree.
  fixtures <- parse_fixtures(fixture_html("hoxton-played.html"))

  tallies <- fixtures %>%
    mutate(logged = map_int(events, ~ sum(.x$event_type == "goal")))

  expect_equal(tallies$logged, tallies$home_goals + tallies$away_goals)
})

test_that("man of the match is picked up and empty rows are not", {
  fixtures <- parse_fixtures(fixture_html("hoxton-played.html"))
  events <- bind_rows(fixtures$events)

  expect_setequal(unique(events$event_type), c("goal", "mom"))
  # Every fixture renders a CARDS heading whether or not a card was shown, and
  # an unawarded MOM renders an empty medal row; neither should become an event.
  expect_false(any(is.na(events$player)))
  expect_true(all(nzchar(events$player)))
  expect_true(all(is.na(events$minute[events$event_type == "mom"])))
  expect_false(any(is.na(events$minute[events$event_type == "goal"])))
})

test_that("a fixture list with nothing played yet parses to zero-zero", {
  fixtures <- parse_fixtures(fixture_html("shoreditch-unplayed.html"))

  expect_equal(nrow(fixtures), 24) # six weeks, four fixtures a week
  expect_true(all(fixtures$home_goals == 0 & fixtures$away_goals == 0))
  expect_equal(sum(map_int(fixtures$events, nrow)), 0)
})

test_that("our fixtures are oriented to us whichever side we were listed on", {
  fixtures <- parse_fixtures(fixture_html("hoxton-played.html"))
  ours <- our_fixtures(fixtures, team = "Hackney Hedgehogs")

  expect_equal(nrow(ours), 4)
  expect_false(any(ours$opponent == "Hackney Hedgehogs"))

  # 22 Jul: listed at home, won 8-1. 29 Jul: listed away, lost 2-3. The score
  # has to follow us across the fixture, not stay with the home column.
  home_match <- ours %>% filter(date == as.Date("2026-07-22"))
  expect_equal(home_match$goals_for, 8L)
  expect_equal(home_match$goals_against, 1L)

  away_match <- ours %>% filter(date == as.Date("2026-07-29"))
  expect_equal(away_match$opponent, "AFC Cognizant")
  expect_equal(away_match$goals_for, 2L)
  expect_equal(away_match$goals_against, 3L)

  # ...and so do the goals inside it.
  away_goals <- away_match$events[[1]] %>% filter(event_type == "goal")
  expect_equal(sum(away_goals$team == "us"), 2L)
  expect_equal(sum(away_goals$team == "them"), 3L)
})

# Man of the match ----
#
# The site renders the winner's name in the home column of every fixture,
# whichever team he plays for, so the column is not evidence and the only thing
# tying him to a side is the goal list above him.

test_that("the man of the match parses with no side of his own", {
  events <- bind_rows(parse_fixtures(fixture_html("hoxton-played.html"))$events)

  expect_true(all(is.na(events$side[events$event_type == "mom"])))
  expect_false(any(is.na(events$side[events$event_type == "goal"])))
})

test_that("a man of the match who scored is put on the side he scored for", {
  ours <- our_fixtures(
    parse_fixtures(fixture_html("hoxton-played.html")),
    team = "Hackney Hedgehogs"
  )

  # 22 Jul, at home: Ferg won it and scored twice for us.
  won_by_us <- ours %>%
    filter(date == as.Date("2026-07-22")) %>%
    pull(events) %>%
    first() %>%
    filter(event_type == "mom")
  expect_equal(won_by_us$player, "Ferg")
  expect_equal(won_by_us$team, "us")

  # 29 Jul, away: Harry won it and scored twice for AFC Cognizant. The site
  # still put him in the home column, which on that fixture was theirs anyway —
  # the point is that the goal list is what decided it.
  won_by_them <- ours %>%
    filter(date == as.Date("2026-07-29")) %>%
    pull(events) %>%
    first() %>%
    filter(event_type == "mom")
  expect_equal(won_by_them$player, "Harry")
  expect_equal(won_by_them$team, "them")
})

test_that("a name that scored for both teams decides nothing", {
  both <- tibble(
    side = c("home", "away", NA),
    minute = c(20L, 25L, NA),
    event_type = c("goal", "goal", "mom"),
    player = c("Felix", "Felix", "Felix")
  )

  oriented <- orient_events(both, we_are_home = TRUE)

  expect_equal(oriented$team, c("us", "them", NA))
})

test_that("a bye week produces no fixture rather than an empty one", {
  # Nine teams and four pitches means one team sits out each week; ours is out
  # in week 3 of the fixture list.
  ours <- our_fixtures(parse_fixtures(fixture_html("shoreditch-unplayed.html")))

  expect_equal(nrow(ours), 5) # six weeks, one of them a bye
  expect_false(as.Date("2026-09-23") %in% ours$date)
})

test_that("we are found in our own league page", {
  ours <- our_fixtures(parse_fixtures(fixture_html("shoreditch-unplayed.html")))

  expect_gt(nrow(ours), 0)
  expect_setequal(
    ours$opponent,
    c("Shamrock Posers", "Brother Man FC", "Ball FC", "Finessin FC", "Dynamo Mickey CF")
  )
})

test_that("a team that is not in the league yields no fixtures, not an error", {
  fixtures <- parse_fixtures(fixture_html("hoxton-played.html"))

  expect_equal(nrow(our_fixtures(fixtures, team = "Borulanta")), 0)
})

# A page assembled by hand, so a shape neither saved fixture happens to contain
# can be exercised: an icon rendered without a src.
minimal_page <- function(mom_icon = '<img src="/content/images/icons/icon-medal-blue.svg">',
                         mom_player = "Ben") {
  paste0('
<div class="tab-content section-fixtures">
  <div class="accordion-leagues">
    <p class="accordion-date">22 July 2026 - Week 1</p>
    <div class="panel collapse-section">
      <div class="accordion-title">
        <div class="team-info">
          <div class="team-1-wrapper">Borulanta</div>
          <div class="team-wrapper-blue">1 : 0</div>
          <div class="team-2-wrapper">Ball FC</div>
        </div>
        <div class="plus-info" data-toggle="collapse" href="#info-42"></div>
      </div>
      <div id="info-42" class="accordion-content collapse">
        <div class="row-info">
          <div class="date-info">
            <div class="time-info"><p>20&#39;</p></div>
            <div class="icon-info"><i><img src="/content/images/icons/icon-ball-blue.svg"></i></div>
          </div>
          <div class="team-info">
            <div class="team-1-wrapper">Ben</div>
            <div class="team-wrapper">-</div>
            <div class="team-2-wrapper"></div>
          </div>
        </div>
        <div class="row-info info-medal">
          <div class="date-info">
            <div class="time-info"><p>MOM</p></div>
            <div class="icon-info"><i>', mom_icon, '</i></div>
          </div>
          <div class="team-info">
            <div class="team-1-wrapper">', mom_player, '</div>
            <div class="team-wrapper"></div>
            <div class="team-2-wrapper"></div>
          </div>
        </div>
      </div>
    </div>
  </div>
</div>')
}

test_that("the hand-built page parses the same way the saved ones do", {
  fixtures <- parse_fixtures(minimal_page())

  expect_equal(nrow(fixtures), 1)
  expect_equal(fixtures$home_goals, 1L)
  expect_setequal(fixtures$events[[1]]$event_type, c("goal", "mom"))
})

test_that("an icon rendered without a src does not kill the parse", {
  # html_attr("src") gives NA, and an NA reaching an if() is an error rather
  # than a FALSE, so one malformed row would take the whole page down with it.
  fixtures <- parse_fixtures(minimal_page(mom_icon = "<img>"))

  expect_equal(nrow(fixtures), 1)
  events <- fixtures$events[[1]]
  expect_equal(sum(events$event_type == "goal"), 1L)
  # The medal icon is gone, so the row is recognised by its label instead.
  expect_equal(sum(events$event_type == "mom"), 1L)
})

test_that("a man of the match who did not score is left on neither side", {
  # Nothing on the page says whose he is, and guessing from the roster is how
  # an opposition player with one of our first names gets credited with a night
  # he had against us.
  events <- parse_fixtures(minimal_page(mom_player = "Ciaran"))$events[[1]]
  oriented <- orient_events(events, we_are_home = TRUE)

  expect_equal(oriented$team[oriented$event_type == "mom"], NA_character_)
  expect_equal(oriented$team[oriented$event_type == "goal"], "us")
})

# The league table ----
#
# The fixture is the real LEAGUE TABLE block, trimmed off our own page as it
# stood at the start of a season: nine teams, nothing played. That is the state
# the table spends the least time in and the one most likely to be got wrong,
# since every number in it is zero.

test_that("every team in the league table parses", {
  standings <- parse_league_table(fixture_html("league-table.html"))

  expect_equal(nrow(standings), 9)
  expect_false(anyNA(standings$position))
  expect_true(all(nzchar(standings$team)))
  expect_equal(standings$position, 1:9)
  expect_true(OUR_TEAM %in% standings$team)
})

test_that("the table's numbers come back as numbers, not strings", {
  standings <- parse_league_table(fixture_html("league-table.html"))

  counts <- c("played", "won", "drawn", "lost", "goals_for",
              "goals_against", "goal_difference", "points")
  for (column in counts) {
    expect_type(standings[[column]], "integer")
  }
  # Nothing played yet, so the whole grid is zero. A parser reading the wrong
  # cells would still produce integers, but not these.
  expect_true(all(standings$played == 0))
  expect_true(all(standings$points == 0))
})

# Hand-built so the values differ per column and per row: against an all-zero
# table a parser that read the columns in the wrong order would still pass.
standings_page <- function(rows) {
  cell <- function(value) paste0("<td>", value, "</td>")
  row <- function(r) paste0("<tr>", paste0(vapply(r, cell, character(1)), collapse = ""), "</tr>")
  paste0(
    '<table class="table-leagues table-team-leagues"><tbody>',
    paste0(vapply(rows, row, character(1)), collapse = ""),
    "</tbody></table>"
  )
}

test_that("each column lands in the field it belongs to", {
  standings <- parse_league_table(standings_page(list(
    #  pos team      P  W  D  L  GF GA GD PTS
    list(1, "Ball FC", 5, 4, 1, 0, 22, 7, 15, 13),
    list(2, OUR_TEAM,  5, 3, 0, 2, 19, 14, 5, 9)
  )))

  us <- standings %>% filter(team == OUR_TEAM)
  expect_equal(us$position, 2L)
  expect_equal(us$played, 5L)
  expect_equal(us$won, 3L)
  expect_equal(us$drawn, 0L)
  expect_equal(us$lost, 2L)
  expect_equal(us$goals_for, 19L)
  expect_equal(us$goals_against, 14L)
  expect_equal(us$goal_difference, 5L)
  expect_equal(us$points, 9L)
})

test_that("a page with no league table is an error, not an empty table", {
  # Silently returning nothing would put an empty card on the home page and
  # give no clue that the markup had moved.
  expect_error(parse_league_table("<html><body></body></html>"), "no league table")
  expect_error(
    parse_league_table('<table class="table-leagues table-team-leagues"><tbody></tbody></table>'),
    "no rows"
  )
})

test_that("a table with columns added or removed is refused", {
  expect_error(
    parse_league_table(standings_page(list(list(1, "Ball FC", 5, 4, 1)))),
    "columns have changed"
  )
})
