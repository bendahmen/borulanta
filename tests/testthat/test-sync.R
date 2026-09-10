# Reconciling a scrape with the files.
#
# Most of what matters here is what the sync refuses to do, so most of these
# tests assert that something did *not* change.

# Two seasons: one closed and settled, one open, mirroring the real shape of
# data/seasons.csv.
test_seasons <- function() {
  tibble(
    season_id = c("s1", "s2"),
    label = c("Season 1", "Season 2"),
    start_date = as.Date(c("2026-01-01", "2026-06-01")),
    fee_rules = c("archived", "core_backstop"),
    status = c("closed", "open")
  ) %>%
    mutate(end_date = coalesce(lead(start_date) - 1, as.Date("9999-12-31")))
}

no_matches <- function() {
  tibble(
    date = as.Date(character()), opponent = character(),
    goals_for = integer(), goals_against = integer(), dl_match_id = character()
  )
}

no_events <- function() {
  tibble(
    date = as.Date(character()), dl_match_id = character(), team = character(),
    minute = integer(), event_type = character(), player = character()
  )
}

no_names <- function() tibble(site_name = character(), player = character())

#' One scraped fixture, shaped the way `our_fixtures()` returns them.
fixture <- function(date, opponent = "Ball FC", goals_for = 0L, goals_against = 0L,
                    id = "900", events = NULL) {
  tibble(
    date = as.Date(date), dl_match_id = id, opponent = opponent,
    goals_for = as.integer(goals_for), goals_against = as.integer(goals_against),
    events = list(events %||% tibble(
      team = character(), minute = integer(),
      event_type = character(), player = character()
    ))
  )
}

goals <- function(...) {
  scorers <- list(...)
  bind_rows(map(scorers, function(scorer) {
    tibble(
      team = scorer[[1]], minute = as.integer(scorer[[2]]),
      event_type = "goal", player = scorer[[3]]
    )
  }))
}

run <- function(fixtures, matches = no_matches(), events = no_events(),
                names = no_names(), today = as.Date("2026-07-01")) {
  sync_results(fixtures, matches, events, test_seasons(), names, today = today)
}

# Recording a result ----

test_that("a played match is recorded with its opponent and its goals", {
  scraped <- fixture(
    "2026-06-10", "Ball FC", 2, 1, "901",
    goals(c("us", "12", "Felix"), c("them", "20", "Andy"), c("us", "31", "Ferg"))
  )

  out <- run(scraped)

  expect_equal(nrow(out$matches), 1)
  expect_equal(out$matches$opponent, "Ball FC")
  expect_equal(out$matches$goals_for, 2L)
  expect_equal(out$matches$goals_against, 1L)
  expect_equal(out$matches$dl_match_id, "901")

  # Two goals of ours, and the opposition's is read but not kept.
  expect_equal(nrow(out$events), 2)
  expect_true(all(out$events$team == "us"))
  expect_setequal(out$events$player, c("Felix", "Ferg"))
  expect_equal(nrow(out$report$new), 1)
})

test_that("the opposition's scorers are dropped, name collision and all", {
  # Both sides had a Felix. Ours is Vitto on the roster; theirs is a different
  # person entirely, and keeping his goal would either rename him into our squad
  # or leave a name in the file that nothing in the app can account for.
  scraped <- fixture(
    "2026-06-10", goals_for = 1, goals_against = 1,
    events = goals(c("us", "12", "Felix"), c("them", "20", "Felix"))
  )
  names <- tibble(site_name = "Felix", player = "Vitto")

  out <- run(scraped, names = names)

  expect_equal(nrow(out$events), 1)
  expect_equal(out$events$team, "us")
  expect_equal(out$events$player, "Vitto")
})

# Refusing to record ----

test_that("a fixture that has not been played yet is not recorded", {
  out <- run(fixture("2026-08-05"), today = as.Date("2026-07-01"))

  expect_equal(nrow(out$matches), 0)
  expect_equal(nrow(out$report$new), 0)
})

test_that("today's own fixture is not recorded — the game is tonight", {
  out <- run(fixture("2026-07-01"), today = as.Date("2026-07-01"))

  expect_equal(nrow(out$matches), 0)
})

test_that("a past 0-0 with no goals logged is held back for a human", {
  # Unplayed, cancelled, and a genuine goalless draw all look like this.
  out <- run(fixture("2026-06-10"))

  expect_equal(nrow(out$matches), 0)
  expect_equal(nrow(out$report$pending), 1)
  expect_equal(out$report$pending$date, as.Date("2026-06-10"))
})

test_that("a match in a closed season is refused, however it looks", {
  # s1 is settled and its charges are frozen in an archive ledger; adding a
  # match to it would put the app's fee tab at odds with the ledger.
  out <- run(fixture("2026-03-11", goals_for = 3, goals_against = 2))

  expect_equal(nrow(out$matches), 0)
  expect_equal(out$report$refused$disposition, "closed_season")
})

test_that("a match dated before any season exists is refused", {
  out <- run(fixture("2025-11-05", goals_for = 3, goals_against = 2))

  expect_equal(nrow(out$matches), 0)
  expect_equal(out$report$refused$disposition, "no_season")
})

test_that("a score that disagrees with the one on file is reported, not applied", {
  existing <- tibble(
    date = as.Date("2026-06-10"), opponent = "Ball FC",
    goals_for = 4L, goals_against = 3L, dl_match_id = "901"
  )

  out <- run(fixture("2026-06-10", "Ball FC", 2, 1, "901"), matches = existing)

  expect_equal(out$matches$goals_for, 4L)
  expect_equal(out$matches$goals_against, 3L)
  expect_equal(nrow(out$report$conflicting), 1)
  expect_equal(nrow(out$report$new), 0)
})

# Season rollover ----

test_that("a wiped page of fresh 0-0 fixtures changes nothing", {
  # What the league page looks like the day a new season starts: a full
  # fixture list, all goalless, with a brand new set of ids.
  history <- tibble(
    date = as.Date(c("2026-06-10", "2026-06-17")),
    opponent = c("Ball FC", "Finessin FC"),
    goals_for = c(2L, 5L), goals_against = c(1L, 3L),
    dl_match_id = c("901", "902")
  )
  wiped <- bind_rows(
    fixture("2026-09-09", "Shamrock Posers", id = "96094"),
    fixture("2026-09-16", "Brother Man FC", id = "96099"),
    fixture("2026-09-30", "Ball FC", id = "96105")
  )

  out <- run(wiped, matches = history, today = as.Date("2026-09-08"))

  expect_equal(out$matches, history)
})

test_that("a recycled fixture id cannot overwrite an older season's match", {
  # Ids restart with the league, so the same id can name a match we already
  # hold. Keying on the date rather than the id is what makes this harmless.
  history <- tibble(
    date = as.Date("2026-06-10"), opponent = "Ball FC",
    goals_for = 2L, goals_against = 1L, dl_match_id = "901"
  )
  reused <- fixture("2026-06-24", "Finessin FC", 7, 0, id = "901")

  out <- run(reused, matches = history, today = as.Date("2026-07-01"))

  expect_equal(nrow(out$matches), 2)
  june_10 <- out$matches %>% filter(date == as.Date("2026-06-10"))
  expect_equal(june_10$goals_for, 2L)
  expect_equal(june_10$opponent, "Ball FC")
})

test_that("matches missing from the page are kept, not deleted", {
  history <- tibble(
    date = as.Date(c("2026-06-10", "2026-06-17")),
    opponent = c("Ball FC", NA_character_),
    goals_for = c(2L, 5L), goals_against = c(1L, 3L),
    dl_match_id = c("901", NA_character_)
  )

  out <- run(fixture("2026-06-24", "Finessin FC", 1, 0, "903"), matches = history)

  expect_equal(nrow(out$matches), 3)
  expect_true(all(history$date %in% out$matches$date))
})

# Filling blanks ----

test_that("an opponent is backfilled onto a row that has the same score", {
  # Every match before this sync existed was recorded by hand with no opponent.
  existing <- tibble(
    date = as.Date("2026-06-10"), opponent = NA_character_,
    goals_for = 2L, goals_against = 1L, dl_match_id = NA_character_
  )

  out <- run(fixture("2026-06-10", "Ball FC", 2, 1, "901"), matches = existing)

  expect_equal(out$matches$opponent, "Ball FC")
  expect_equal(out$matches$dl_match_id, "901")
  expect_equal(out$matches$goals_for, 2L)
  expect_equal(nrow(out$report$enriched), 1)
})

test_that("an opponent already on file is not rewritten", {
  existing <- tibble(
    date = as.Date("2026-06-10"), opponent = "Ball FC Reserves",
    goals_for = 2L, goals_against = 1L, dl_match_id = "901"
  )

  out <- run(fixture("2026-06-10", "Ball FC", 2, 1, "901"), matches = existing)

  expect_equal(out$matches$opponent, "Ball FC Reserves")
  expect_equal(nrow(out$report$unchanged), 1)
})

# Events ----

test_that("running twice replaces the events rather than doubling them", {
  scraped <- fixture(
    "2026-06-10", goals_for = 2, goals_against = 0,
    events = goals(c("us", "12", "Felix"), c("us", "31", "Ferg"))
  )

  once <- run(scraped)
  twice <- run(scraped, matches = once$matches, events = once$events)

  expect_equal(nrow(twice$events), 2)
  expect_equal(nrow(twice$report$new), 0)
})

# Man of the match ----
#
# The page does not say which team won it, so the sync places him off the goal
# list beside him or not at all. See R/scrape.R for how that is worked out;
# these are about what the sync then does with the answer.

#' A fixture with a man of the match already placed on a side by the scrape.
with_mom <- function(team, player = "Felix", ...) {
  fixture(..., events = bind_rows(
    goals(c("us", "12", "Felix")),
    tibble(team = team, minute = NA_integer_, event_type = "mom", player = player)
  ))
}

test_that("a man of the match of ours is recorded", {
  out <- run(with_mom("us", "Felix", date = "2026-06-10", goals_for = 1))

  mom <- out$events %>% filter(event_type == "mom")
  expect_equal(mom$player, "Felix")
  expect_equal(mom$team, "us")
})

test_that("a man of the match of theirs is recorded for nobody", {
  out <- run(with_mom("them", "Andy", date = "2026-06-10", goals_for = 1))

  expect_equal(nrow(out$events %>% filter(event_type == "mom")), 0)
})

test_that("a man of the match of theirs clears one we wrongly hold", {
  # The state the old parser left behind: it read every MOM as the home team's,
  # so an away win put an opposition player in our file. A re-run has to take
  # him back out, not merely stop adding him.
  wrong <- no_events() %>% add_row(
    date = as.Date("2026-06-10"), dl_match_id = "900", team = "us",
    minute = NA_integer_, event_type = "mom", player = "Andy"
  )

  out <- run(with_mom("them", "Andy", date = "2026-06-10", goals_for = 1), events = wrong)

  expect_equal(nrow(out$events %>% filter(event_type == "mom")), 0)
})

test_that("an unplaceable man of the match is reported and left to a person", {
  scraped <- with_mom(NA_character_, "Ciaran", date = "2026-06-10", goals_for = 1)

  out <- run(scraped)

  expect_equal(nrow(out$events %>% filter(event_type == "mom")), 0)
  expect_equal(out$report$unattributed_mom$player, "Ciaran")
  expect_equal(out$report$unattributed_mom$opponent, "Ball FC")
})

test_that("a man of the match entered by hand survives the next sync", {
  # The whole point of not writing an unplaceable one: if the sync went on
  # rewriting the date's MOM rows regardless, the answer would be wiped every
  # Wednesday and there would be no way to record it at all.
  scraped <- with_mom(NA_character_, "Ciaran", date = "2026-06-10", goals_for = 1)
  once <- run(scraped)
  by_hand <- once$events %>% add_row(
    date = as.Date("2026-06-10"), dl_match_id = "900", team = "us",
    minute = NA_integer_, event_type = "mom", player = "Vitto"
  )

  twice <- run(scraped, matches = once$matches, events = by_hand)

  mom <- twice$events %>% filter(event_type == "mom")
  expect_equal(mom$player, "Vitto")
})

test_that("a correction on the site takes effect on the next run", {
  first <- run(fixture(
    "2026-06-10", goals_for = 1, goals_against = 0,
    events = goals(c("us", "12", "Felix"))
  ))
  corrected <- run(
    fixture("2026-06-10", goals_for = 1, goals_against = 0,
            events = goals(c("us", "12", "Ferg"))),
    matches = first$matches, events = first$events
  )

  expect_equal(nrow(corrected$events), 1)
  expect_equal(corrected$events$player, "Ferg")
})

test_that("hand-recorded event kinds survive a sync of the same match", {
  # The sync owns goals and MOM. Anything else in the file was put there by a
  # person and is not the site's to overwrite.
  by_hand <- tibble(
    date = as.Date("2026-06-10"), dl_match_id = "901", team = "us",
    minute = 12L, event_type = "assist", player = "Ben"
  )
  scraped <- fixture(
    "2026-06-10", goals_for = 1, goals_against = 0,
    events = goals(c("us", "12", "Felix"))
  )

  out <- run(scraped, events = by_hand)

  expect_equal(sum(out$events$event_type == "assist"), 1L)
  expect_equal(out$events$player[out$events$event_type == "assist"], "Ben")
  expect_equal(sum(out$events$event_type == "goal"), 1L)
})

test_that("events for matches outside the scrape are left alone", {
  older <- tibble(
    date = as.Date("2026-06-03"), dl_match_id = "900", team = "us",
    minute = 5L, event_type = "goal", player = "Ben"
  )

  out <- run(fixture("2026-06-10", goals_for = 1, goals_against = 0,
                     events = goals(c("us", "12", "Felix"))),
             events = older)

  expect_equal(sum(out$events$date == as.Date("2026-06-03")), 1L)
})

test_that("goals that do not add up to the score are recorded but flagged", {
  # The result is what the league table runs on, so it is still worth having;
  # the event list simply is not complete enough to count from.
  scraped <- fixture(
    "2026-06-10", goals_for = 3, goals_against = 1,
    events = goals(c("us", "12", "Felix"))
  )

  out <- run(scraped)

  expect_equal(out$matches$goals_for, 3L)
  expect_equal(nrow(out$report$miscounted), 1)
})

# Distrusting the page ----

test_that("a page we do not appear in aborts without writing", {
  history <- tibble(
    date = as.Date("2026-06-10"), opponent = "Ball FC",
    goals_for = 2L, goals_against = 1L, dl_match_id = "901"
  )

  out <- run(no_matches()[0, ] %>%
    transmute(date, dl_match_id, opponent, goals_for, goals_against,
              events = list()), matches = history)

  expect_false(is.null(out$abort))
  expect_match(out$abort, "no fixtures")
  expect_equal(out$matches, history)
})

test_that("two fixtures for us on one date abort rather than pick one", {
  clash <- bind_rows(
    fixture("2026-06-10", "Ball FC", 2, 1, "901"),
    fixture("2026-06-10", "Finessin FC", 0, 3, "902")
  )

  out <- run(clash)

  expect_false(is.null(out$abort))
  expect_match(out$abort, "more than one fixture")
  expect_equal(nrow(out$matches), 0)
})

# End to end ----

test_that("a real page of played matches syncs into empty files", {
  # The two halves joined up: a genuine league page, parsed, oriented and
  # reconciled, with nothing mocked in between.
  seasons <- tibble(
    season_id = "s1", label = "Season 1",
    start_date = as.Date("2026-07-01"), fee_rules = "core_backstop",
    status = "open", end_date = as.Date("9999-12-31")
  )
  fixtures <- our_fixtures(
    parse_fixtures(fixture_html("hoxton-played.html")),
    team = "Hackney Hedgehogs"
  )

  out <- sync_results(
    fixtures, no_matches(), no_events(), seasons,
    tibble(site_name = "Ferg", player = "Fergus"),
    today = as.Date("2026-08-20")
  )

  expect_null(out$abort)
  expect_equal(nrow(out$matches), 4)
  expect_equal(out$matches$date, sort(out$matches$date))
  expect_equal(
    out$matches$opponent,
    c("Get The Rodge In FC", "AFC Cognizant", "Victoria Park Vixens", "Allardyce Ice Baby")
  )
  expect_equal(out$matches$goals_for, c(8L, 2L, 2L, 3L))
  expect_equal(out$matches$goals_against, c(1L, 3L, 1L, 0L))

  # Every goal of ours came through, and none of theirs — though the miscount
  # check still counts both sides off the page, which is why it stays quiet.
  our_goals <- out$events %>% filter(event_type == "goal", team == "us")
  expect_equal(nrow(our_goals), sum(out$matches$goals_for))
  expect_true(all(out$events$team == "us"))
  expect_equal(nrow(out$report$miscounted), 0)

  # ...and our scorers are named as we name them.
  expect_true("Fergus" %in% our_goals$player)
  expect_false("Ferg" %in% our_goals$player)
})

test_that("re-syncing the same real page is a no-op", {
  seasons <- tibble(
    season_id = "s1", label = "Season 1",
    start_date = as.Date("2026-07-01"), fee_rules = "core_backstop",
    status = "open", end_date = as.Date("9999-12-31")
  )
  fixtures <- our_fixtures(
    parse_fixtures(fixture_html("hoxton-played.html")),
    team = "Hackney Hedgehogs"
  )
  again <- function(matches, events) {
    sync_results(fixtures, matches, events, seasons, no_names(),
                 today = as.Date("2026-08-20"))
  }

  once <- again(no_matches(), no_events())
  twice <- again(once$matches, once$events)

  expect_equal(twice$matches, once$matches)
  expect_equal(twice$events, once$events)
  expect_equal(nrow(twice$report$new), 0)
  expect_equal(nrow(twice$report$enriched), 0)
})

# Leaving things alone ----

test_that("a conflicting score leaves that match's goals alone too", {
  # The match row is protected, and so is everything hanging off it: replacing
  # its goals from a page we have just decided not to believe would leave the
  # two files contradicting each other.
  existing <- tibble(
    date = as.Date("2026-06-10"), opponent = "Ball FC",
    goals_for = 4L, goals_against = 3L, dl_match_id = "901"
  )
  ours <- tibble(
    date = as.Date("2026-06-10"), dl_match_id = "901", team = "us",
    minute = 12L, event_type = "goal", player = "Ben"
  )

  out <- run(
    fixture("2026-06-10", "Ball FC", 2, 1, "901",
            goals(c("us", "44", "Felix"))),
    matches = existing, events = ours
  )

  expect_equal(nrow(out$report$conflicting), 1)
  expect_equal(out$events, ours)
  expect_false(out$changed)
})

test_that("a row still waiting for its score is filled in, not duplicated", {
  # A second row for one date would charge the whole squad twice for that game.
  awaiting <- tibble(
    date = as.Date("2026-06-10"), opponent = NA_character_,
    goals_for = NA_integer_, goals_against = NA_integer_, dl_match_id = NA_character_
  )

  out <- run(fixture("2026-06-10", "Ball FC", 2, 1, "901"), matches = awaiting)

  expect_equal(nrow(out$matches), 1)
  expect_equal(out$matches$goals_for, 2L)
  expect_equal(out$matches$opponent, "Ball FC")
  expect_equal(nrow(out$report$enriched), 1)
  expect_equal(nrow(out$report$new), 0)
})

test_that("two rows for one date abort rather than fan out every join", {
  doubled <- tibble(
    date = as.Date(c("2026-06-10", "2026-06-10")),
    opponent = c("Ball FC", "Ball FC"),
    goals_for = c(2L, 2L), goals_against = c(1L, 1L),
    dl_match_id = c("901", "901")
  )

  out <- run(fixture("2026-06-17", "Finessin FC", 1, 0, "902"), matches = doubled)

  expect_false(is.null(out$abort))
  expect_match(out$abort, "more than one row")
  expect_equal(out$matches, doubled)
})

test_that("a goalless draw someone has confirmed stops being reported forever", {
  # Once a person has written the 0-0 down the ambiguity is gone, so the fixture
  # becomes ordinary: its opponent gets filled in and it drops off the list of
  # things awaiting a scoreline.
  confirmed <- tibble(
    date = as.Date("2026-06-10"), opponent = NA_character_,
    goals_for = 0L, goals_against = 0L, dl_match_id = NA_character_
  )

  out <- run(fixture("2026-06-10", "Ball FC", 0, 0, "901"), matches = confirmed)

  expect_equal(nrow(out$report$pending), 0)
  expect_equal(out$matches$opponent, "Ball FC")
  expect_equal(out$matches$goals_for, 0L)
})

test_that("a goalless draw in a closed season stays refused once confirmed", {
  confirmed <- tibble(
    date = as.Date("2026-03-11"), opponent = NA_character_,
    goals_for = 0L, goals_against = 0L, dl_match_id = NA_character_
  )

  out <- run(fixture("2026-03-11", "Ball FC", 0, 0, "901"), matches = confirmed)

  expect_equal(out$report$refused$disposition, "closed_season")
  expect_true(is.na(out$matches$opponent))
})

test_that("a fixture the site gives no id for is not re-reported every week", {
  # is.na(existing_id) alone would stay true forever and rewrite the files on
  # every run for the rest of the season.
  existing <- tibble(
    date = as.Date("2026-06-10"), opponent = "Ball FC",
    goals_for = 2L, goals_against = 1L, dl_match_id = NA_character_
  )
  idless <- fixture("2026-06-10", "Ball FC", 2, 1, id = NA_character_)

  out <- run(idless, matches = existing)

  expect_equal(nrow(out$report$enriched), 0)
  expect_equal(nrow(out$report$unchanged), 1)
  expect_false(out$changed)
})

test_that("a run that changes nothing says so", {
  out <- run(fixture("2026-08-05"))

  expect_false(out$changed)
})

# Half-known results ----

test_that("a half-filled score conflicts rather than being completed from the site", {
  # Keeping the side we hold and taking the side we do not believe would write
  # a score belonging to neither the file nor the page.
  half <- tibble(
    date = as.Date("2026-06-10"), opponent = NA_character_,
    goals_for = 2L, goals_against = NA_integer_, dl_match_id = NA_character_
  )

  out <- run(fixture("2026-06-10", "Ball FC", 5, 3, "901"), matches = half)

  expect_equal(nrow(out$report$conflicting), 1)
  expect_equal(out$matches$goals_for, 2L)
  expect_true(is.na(out$matches$goals_against))
  expect_false(out$changed)
})

test_that("a half-filled score whose known side agrees is completed", {
  half <- tibble(
    date = as.Date("2026-06-10"), opponent = NA_character_,
    goals_for = 5L, goals_against = NA_integer_, dl_match_id = NA_character_
  )

  out <- run(fixture("2026-06-10", "Ball FC", 5, 3, "901"), matches = half)

  expect_equal(nrow(out$report$conflicting), 0)
  expect_equal(out$matches$goals_against, 3L)
  expect_equal(out$matches$opponent, "Ball FC")
})

test_that("a result we hold goes quiet when the page reverts to its default", {
  # The mirror of a confirmed goalless draw: the page showing 0 : 0 over a match
  # we have already recorded is the site's placeholder, not a correction, and
  # reporting it every Friday for the rest of the season is just noise.
  recorded <- tibble(
    date = as.Date("2026-06-10"), opponent = "Ball FC",
    goals_for = 2L, goals_against = 1L, dl_match_id = "901"
  )

  out <- run(fixture("2026-06-10", "Ball FC", 0, 0, "901"), matches = recorded)

  expect_equal(nrow(out$report$pending), 0)
  expect_equal(nrow(out$report$conflicting), 0)
  expect_equal(out$matches, recorded)
  expect_false(out$changed)
})

# Types ----

test_that("a caller that skips the file readers is still given the right types", {
  # The pinned col_types in read_match_file()/read_event_file() are the braces;
  # this is the belt. Anything reaching sync_results() another way — the app, a
  # console, a script written later — gets the same treatment.
  loosely_typed <- tibble(
    date = "10/06/2026", opponent = NA, # all-NA reads back as logical
    goals_for = "2", goals_against = "1", dl_match_id = 901
  )
  loose_events <- tibble(
    date = "03/06/2026", dl_match_id = "900", team = "us",
    minute = "5", event_type = "goal", player = "Ben"
  )

  out <- run(
    fixture("2026-06-10", "Ball FC", 2, 1, "901"),
    matches = loosely_typed, events = loose_events
  )

  expect_null(out$abort)
  expect_equal(out$matches$date, as.Date("2026-06-10"))
  expect_type(out$matches$goals_for, "integer")
  expect_type(out$matches$dl_match_id, "character")
  expect_equal(out$matches$dl_match_id, "901")
  expect_type(out$events$minute, "integer")
  # One row on file, one fixture for the same date: an enrich, never a second row.
  expect_equal(nrow(out$matches), 1)
  expect_equal(out$matches$opponent, "Ball FC")
})
