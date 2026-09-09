# Syncing the scrape into the data files ----
#
# The scrape is a view of one league season; the data files are the permanent
# record of every season. Reconciling them is mostly a matter of refusing to do
# things, because the two ways this can go wrong are both silent:
#
#   * The site pre-populates the entire season's fixture list at 0 : 0, so an
#     unplayed, cancelled or postponed match is indistinguishable from a real
#     goalless draw by its score alone. Writing one is a phantom match that the
#     fee engine will happily charge people for.
#
#   * When the league season ends the page is wiped and starts again from zero,
#     with a fresh set of fixture ids. A sync that keyed on those ids, or that
#     treated absence from the page as deletion, would quietly eat the history.
#
# So: matches are keyed on their date, which is unique across every season and
# is already what attendance joins on; nothing is ever deleted; a season that
# has been closed is untouchable; and anything ambiguous is reported for a
# human rather than resolved by guessing.

#' Which event kinds the sync considers its own.
#'
#' Rows of these kinds are rewritten from the site for any date it accepts, so
#' fixing a name mapping and re-running repairs history. Anything else in
#' match_events.csv — hand-recorded assists, say — is left alone.
SYNCED_EVENT_TYPES <- c("goal", "mom")

#' Reconcile scraped fixtures against the existing files.
#'
#' Pure: takes and returns data, touches neither disk nor network, so the whole
#' decision table is testable without a fixture on the filesystem.
#'
#' @param fixtures our fixtures from `our_fixtures()`, events nested
#' @param matches existing `data/matches.csv`, dates parsed
#' @param events existing `data/match_events.csv`, dates parsed
#' @param seasons from `load_seasons()`
#' @param name_map `site_name` -> `player`, may be empty
#' @param today anything dated today or later is treated as not yet played
#' @return a list with `matches`, `events`, `changed`, `report` and `abort`
sync_results <- function(fixtures, matches, events, seasons, name_map,
                         today = Sys.Date()) {
  # A CSV does not remember what its columns were. An empty match_events.csv
  # reads back as six character columns; once it has rows, `minute` is numeric
  # and an all-digit `dl_match_id` is numeric too, while the scrape produces an
  # integer minute and a character id. Pinning the types here rather than
  # trusting the caller is what stops the first write, and every write after
  # it, from failing on a type clash deep inside a bind_rows.
  matches <- as_match_table(matches)
  events <- as_event_table(events)
  fixtures <- as_fixture_table(fixtures)
  unchanged <- list(matches = matches, events = events, changed = FALSE)

  abort <- sync_objection(fixtures, matches)
  if (!is.null(abort)) {
    return(c(unchanged, list(report = empty_report(), abort = abort)))
  }

  classified <- fixtures %>%
    mutate(
      season_id = assign_season(date, seasons),
      goal_events = purrr::map_int(events, ~ sum(.x$event_type == "goal")),
      disposition = fixture_disposition(
        date, goals_for, goals_against, goal_events, season_id, seasons, today
      )
    ) %>%
    resolve_pending_against_file(matches)

  playable <- classified %>% filter(disposition == "playable")
  decided <- decide_against_existing(playable, matches)

  # A date whose score we could not agree on is left alone completely. Rewriting
  # its goals from a page we have just decided not to believe would leave the
  # two files contradicting each other, which is the failure the conflict
  # branch exists to prevent.
  accepted <- decided %>% filter(action != "conflict")
  writing <- decided %>% filter(action %in% c("new", "enrich"))

  updated_matches <- apply_match_changes(matches, writing)
  updated_events <- apply_event_changes(
    events, playable %>% filter(date %in% accepted$date), name_map
  )

  report <- list(
    new = decided %>% filter(action == "new"),
    enriched = decided %>% filter(action == "enrich"),
    unchanged = decided %>% filter(action == "unchanged"),
    conflicting = decided %>% filter(action == "conflict"),
    pending = classified %>% filter(disposition == "pending"),
    refused = classified %>% filter(disposition %in% c("no_season", "closed_season")),
    miscounted = classified %>%
      filter(disposition == "playable", goal_events > 0,
             goal_events != goals_for + goals_against)
  )

  list(
    matches = updated_matches,
    events = updated_events,
    changed = !identical(updated_matches, matches) || !identical(updated_events, events),
    report = report,
    abort = NULL
  )
}

# Types ----
#
# `as.character()` on a large double would reach for scientific notation, which
# would silently mangle an id, so ids go through format() instead.

as_date_column <- function(x) {
  # as.Date() on a dd/mm/yyyy string does not fail, it returns the year 10.
  if (inherits(x, "Date")) x else parse_match_date(x)
}

as_id <- function(x) {
  if (is.character(x)) return(x)
  if (all(is.na(x))) return(rep(NA_character_, length(x)))
  if_else(is.na(x), NA_character_, format(x, scientific = FALSE, trim = TRUE))
}

as_match_table <- function(matches) {
  matches %>%
    mutate(
      date = as_date_column(date),
      opponent = as.character(opponent),
      goals_for = as.integer(goals_for),
      goals_against = as.integer(goals_against),
      dl_match_id = as_id(dl_match_id)
    )
}

as_event_table <- function(events) {
  events %>%
    mutate(
      date = as_date_column(date),
      dl_match_id = as_id(dl_match_id),
      team = as.character(team),
      minute = as.integer(minute),
      event_type = as.character(event_type),
      player = as.character(player)
    )
}

as_fixture_table <- function(fixtures) {
  fixtures %>%
    mutate(
      date = as_date_column(date),
      dl_match_id = as_id(dl_match_id),
      opponent = as.character(opponent),
      goals_for = as.integer(goals_for),
      goals_against = as.integer(goals_against)
    )
}

# Deciding ----

#' Reasons to write nothing at all.
#'
#' These are the shapes a broken parser or a restructured page takes, plus the
#' one shape a hand-edited matches.csv can take that would make everything
#' downstream wrong. A partial write from a half-understood page is worse than
#' no write, and the failure is loud either way because the run reports it.
sync_objection <- function(fixtures, matches) {
  if (nrow(fixtures) == 0) {
    return(paste(
      "no fixtures for", OUR_TEAM, "in the parsed page —",
      "the team may have been renamed, or the page structure may have changed"
    ))
  }
  if (anyNA(fixtures$date)) {
    return("some fixtures parsed with no date")
  }
  if (anyNA(fixtures$goals_for) || anyNA(fixtures$goals_against)) {
    return("some fixtures parsed with no score")
  }
  if (any(duplicated(fixtures$date))) {
    return(paste(
      "more than one fixture for us on:", listed_dates(fixtures$date[duplicated(fixtures$date)]),
      "— matches are keyed on their date, so this needs sorting out by hand"
    ))
  }
  # Two rows for one date would charge the squad twice for the same game, and
  # would fan out every join from here on. It cannot arise from a sync, so it
  # means the file has been edited into a state nothing else can cope with.
  if (any(duplicated(matches$date))) {
    return(paste(
      "data/matches.csv already has more than one row for:",
      listed_dates(matches$date[duplicated(matches$date)])
    ))
  }
  NULL
}

listed_dates <- function(dates) {
  paste(format(unique(dates), "%d/%m/%Y"), collapse = ", ")
}

#' Decide, for one fixture, whether it is a real result we may record.
fixture_disposition <- function(date, goals_for, goals_against, goal_events,
                                season_id, seasons, today) {
  closed <- seasons$season_id[seasons$status == "closed"]

  dplyr::case_when(
    # A fixture is listed weeks before it is played. Today's own match is not
    # excluded on a technicality: the sync runs in the morning and the game is
    # in the evening, so a same-day fixture has not happened yet either.
    date >= today ~ "future",
    # The season checks come before the goalless one so that a fixture we are
    # not allowed to touch stays refused even after resolve_known_goalless().
    is.na(season_id) ~ "no_season",
    season_id %in% closed ~ "closed_season",
    # 0 : 0 with no goals logged is the site's default, worn equally by a match
    # that has not been played, one that was cancelled, and a genuine goalless
    # draw. Only a person can tell those apart.
    goals_for + goals_against == 0 & goal_events == 0 ~ "pending",
    TRUE ~ "playable"
  )
}

#' Settle a pending fixture against what we already hold for that date.
#'
#' Both directions of "the page says 0 : 0 and we know better". If a person has
#' confirmed a real goalless draw, the ambiguity is gone and the fixture becomes
#' ordinary, so its opponent can be filled in and its man of the match recorded.
#' If we hold some other result and the page has gone back to its default — a
#' correction being re-entered, most likely — there is nothing to do and nothing
#' worth saying; without this it would be reported as awaiting a scoreline every
#' Friday for the rest of the season.
resolve_pending_against_file <- function(classified, matches) {
  recorded <- matches %>%
    filter(!is.na(goals_for), !is.na(goals_against))
  goalless <- recorded$date[recorded$goals_for == 0 & recorded$goals_against == 0]

  classified %>%
    mutate(disposition = dplyr::case_when(
      disposition != "pending" ~ disposition,
      date %in% goalless ~ "playable",
      date %in% recorded$date ~ "settled",
      TRUE ~ "pending"
    ))
}

#' Compare playable fixtures with what is already recorded.
decide_against_existing <- function(playable, matches) {
  existing <- matches %>%
    transmute(
      date,
      on_file = TRUE,
      existing_opponent = opponent,
      existing_for = goals_for,
      existing_against = goals_against,
      existing_id = dl_match_id
    )

  playable %>%
    select(date, season_id, dl_match_id, opponent, goals_for, goals_against) %>%
    left_join(existing, by = "date") %>%
    mutate(
      on_file = coalesce(on_file, FALSE),
      # Only count as filling a blank if there is something to put in it.
      # Otherwise a fixture the site never gave an id would be reported as
      # freshly enriched on every run and the file rewritten every week.
      fills_a_blank =
        (is.na(existing_opponent) & !is.na(opponent)) |
          (is.na(existing_id) & !is.na(dl_match_id)) |
          is.na(existing_for) | is.na(existing_against),
      action = dplyr::case_when(
        # Presence is decided by the join, not by whether the score is filled
        # in: a row awaiting its score is a row, and appending a second one for
        # the same date would charge everybody twice.
        !on_file ~ "new",
        # The score we hold and the score on the site disagree. Either could be
        # right — a late correction, or a fat finger — so say so and change
        # nothing. Overwriting a settled result unprompted is how a scraper
        # silently rewrites the past.
        #
        # Tested a side at a time, because a row with only one side filled in
        # would otherwise enrich: it would keep the half we hold, take the half
        # we do not believe, and end up with a score belonging to neither.
        (!is.na(existing_for) & existing_for != goals_for) |
          (!is.na(existing_against) & existing_against != goals_against) ~ "conflict",
        fills_a_blank ~ "enrich",
        TRUE ~ "unchanged"
      )
    )
}

# Writing ----

#' Append new matches and fill blanks on existing ones; never delete or reorder.
apply_match_changes <- function(matches, writing) {
  if (nrow(writing) == 0) {
    return(matches)
  }

  new_rows <- writing %>%
    filter(action == "new") %>%
    transmute(date, opponent, goals_for, goals_against, dl_match_id)

  enrichment <- writing %>%
    filter(action == "enrich") %>%
    select(date,
      fresh_opponent = opponent, fresh_id = dl_match_id,
      fresh_for = goals_for, fresh_against = goals_against
    )

  matches %>%
    left_join(enrichment, by = "date") %>%
    mutate(
      opponent = coalesce(opponent, fresh_opponent),
      dl_match_id = coalesce(dl_match_id, fresh_id),
      goals_for = coalesce(goals_for, fresh_for),
      goals_against = coalesce(goals_against, fresh_against)
    ) %>%
    select(-fresh_opponent, -fresh_id, -fresh_for, -fresh_against) %>%
    bind_rows(new_rows) %>%
    arrange(date)
}

#' Rewrite the site-derived events for every date the sync accepted.
#'
#' Events are a projection of the page rather than an accumulation, so an
#' accepted date has its goal and MOM rows replaced outright. That is what makes
#' a correction on the site, or a newly added name mapping, take effect on a
#' re-run instead of appending a second copy. Event kinds the sync does not
#' produce, and dates it did not accept, are left exactly as they are — right
#' down to their order, since a file nobody is changing should come back
#' identical rather than merely equivalent.
apply_event_changes <- function(events, accepted, name_map) {
  if (nrow(accepted) == 0) {
    return(events)
  }

  kept <- events %>%
    filter(!(date %in% accepted$date & event_type %in% SYNCED_EVENT_TYPES))

  fresh <- accepted %>%
    select(date, dl_match_id, events) %>%
    tidyr::unnest(events) %>%
    mutate(player = map_player_names(player, team, name_map)) %>%
    select(date, dl_match_id, team, minute, event_type, player) %>%
    as_event_table()

  bind_rows(kept, fresh) %>%
    arrange(date, event_type, minute)
}

#' Translate the site's first names into our player names.
#'
#' The site records "Felix"; the roster says whatever we call him. Only our own
#' side is mapped — the opposition's names are kept verbatim, because they are
#' not our players and exist only so a goal tally reconciles with the score. An
#' unmapped name of ours is left as it came, so nothing is lost and the sync has
#' something to report.
map_player_names <- function(player, team, name_map) {
  if (is.null(name_map) || nrow(name_map) == 0) {
    return(player)
  }
  mapped <- name_map$player[match(player, name_map$site_name)]
  if_else(team == "us" & !is.na(mapped), mapped, player)
}

#' Write both files back in the shape the rest of the project reads them.
write_sync_files <- function(result, match_path, event_path) {
  as_written <- function(data) data %>% mutate(date = format(date, "%d/%m/%Y"))
  readr::write_csv(as_written(result$matches), match_path, na = "")
  readr::write_csv(as_written(result$events), event_path, na = "")
  invisible(result)
}

empty_report <- function() {
  blank <- tibble(date = as.Date(character()))
  list(
    new = blank, enriched = blank, unchanged = blank, conflicting = blank,
    pending = blank, refused = blank, miscounted = blank
  )
}

# Snapshots ----
#
# Two files that are not part of the permanent record: the fixture list and the
# league table. Everything above this point exists to protect history — never
# delete, never touch a closed season, never overwrite a score we already hold.
# None of that applies here, because neither file is history. Both describe the
# current league season only, both are wiped and rebuilt on the site when it
# rolls over, and a sync that replaced them wholesale with next season's is
# doing exactly the right thing.
#
# So they are rewritten outright on every run, and they go nowhere near
# `sync_results()`. The one guarantee they keep from it is the important one:
# the caller writes nothing at all if the page failed to parse.

#' Build both snapshot tables from a parsed page.
#'
#' Pure, like `sync_results()`: the shaping is testable without a filesystem.
#'
#' @param fixtures our fixtures from `our_fixtures()`, played and unplayed alike
#' @param league_table from `parse_league_table()`
#' @param scraped_on the date the page was fetched, recorded so the app can say
#'   how stale the standings are — they age between syncs, and a table nobody
#'   can date is a table nobody can distrust
snapshot_tables <- function(fixtures, league_table, scraped_on = Sys.Date()) {
  list(
    # No score here, deliberately. matches.csv is the only source of truth for
    # results; this is a list of dates and who we are down to play on them.
    fixtures = fixtures %>%
      as_fixture_table() %>%
      transmute(date, opponent, dl_match_id) %>%
      arrange(date),
    league_table = league_table %>%
      mutate(scraped_on = as.Date(scraped_on)) %>%
      arrange(position)
  )
}

#' Replace both snapshot files.
write_snapshot_files <- function(snapshot, fixture_path, table_path) {
  as_written <- function(data, ...) {
    data %>% mutate(across(c(...), ~ format(.x, "%d/%m/%Y")))
  }
  readr::write_csv(as_written(snapshot$fixtures, date), fixture_path, na = "")
  readr::write_csv(as_written(snapshot$league_table, scraped_on), table_path, na = "")
  invisible(snapshot)
}

# League results ----
#
# Every result in the league, not just ours. The page carries all of them and
# the scrape parses all of them; until this file existed everything but our own
# fixtures was thrown away. They are kept so that an opponent's strength can be
# measured from how it fared against everyone else — see `opponent_strength()`
# in R/analysis.R.
#
# Unlike the two snapshots above, this is history: the page is wiped when the
# league season rolls over, so a result that has dropped off it has to survive
# in the file. Unlike matches.csv, nobody hand-edits it and nobody is going to
# resolve a conflict about a match between two other teams, so for any fixture
# that is on the page the page wins. That makes it a projection accumulated
# across seasons: fixtures on the page are rewritten from it, and fixtures that
# are not are kept exactly as they were.

#' Reconcile every played fixture on the page with the league results file.
#'
#' Pure, like `sync_results()`. A fixture counts as played by the same test as
#' our own: its date has passed and it is not the site's default 0 : 0 with
#' nothing logged. A genuine goalless draw between two other teams is therefore
#' never recorded, because nothing distinguishes it from a cancelled one and
#' there is nobody to ask. That drops a data point which says almost nothing
#' about either side's strength anyway.
#'
#' A fixture is keyed on its date and the two teams. A score that has changed on
#' the site since it was recorded is taken as a correction and applied; one that
#' has gone back to 0 : 0 fails the played test and leaves the file alone.
#'
#' @param all_fixtures from `parse_fixtures()`, the whole league, events nested
#' @param league_results existing `data/league_results.csv`, dates parsed
#' @param today anything dated today or later is treated as not yet played
#' @return a list with `league_results`, `added`, `corrected` and `changed`
sync_league_results <- function(all_fixtures, league_results, today = Sys.Date()) {
  league_results <- as_league_result_table(league_results)
  key <- c("date", "home_team", "away_team")

  played <- all_fixtures %>%
    mutate(
      date = as_date_column(date),
      goal_events = purrr::map_int(events, ~ sum(.x$event_type == "goal"))
    ) %>%
    filter(date < today, home_goals + away_goals > 0 | goal_events > 0) %>%
    select(date, home_team, away_team, home_goals, away_goals, dl_match_id) %>%
    as_league_result_table()

  added <- played %>% anti_join(league_results, by = key)
  corrected <- played %>%
    inner_join(
      league_results %>% select(all_of(key), old_home = home_goals, old_away = away_goals),
      by = key
    ) %>%
    filter(old_home != home_goals | old_away != away_goals)

  # A file nobody is changing comes back identical rather than merely
  # equivalent, so a run that finds nothing new does not rewrite it.
  if (nrow(added) == 0 && nrow(corrected) == 0) {
    return(list(
      league_results = league_results, added = added, corrected = corrected,
      changed = FALSE
    ))
  }

  updated <- league_results %>%
    anti_join(played, by = key) %>%
    bind_rows(played) %>%
    arrange(date, home_team, away_team)

  list(league_results = updated, added = added, corrected = corrected, changed = TRUE)
}

as_league_result_table <- function(results) {
  results %>%
    mutate(
      date = as_date_column(date),
      home_team = as.character(home_team),
      away_team = as.character(away_team),
      home_goals = as.integer(home_goals),
      away_goals = as.integer(away_goals),
      dl_match_id = as_id(dl_match_id)
    )
}

write_league_result_file <- function(league_results, path) {
  readr::write_csv(
    league_results %>% mutate(date = format(date, "%d/%m/%Y")), path, na = ""
  )
  invisible(league_results)
}
