# Dream Leagues scraping ----
#
# The league page is plain server-rendered HTML with no API behind it, so one
# GET returns the whole season: every week, every fixture, and inside each
# fixture's accordion the goals with their scorer and minute, plus man of the
# match. Nothing here writes anything; parsing is separated from fetching so the
# parser can be tested against saved pages without touching the network.
#
# What the page does NOT carry is who turned up, which is why attendance stays a
# hand-entered file and the fee engine still depends on it.

DREAMLEAGUES_URL <- "https://dreamleagues.co.uk/leagues/shoreditch-weds-7-a-side"
OUR_TEAM <- "Borulanta"

# Identify honestly rather than borrowing a browser's string. The site's
# robots.txt allows the default user agent; this is one page a week.
SCRAPER_USER_AGENT <- paste(
  "borulanta-sync/1.0",
  "(+https://github.com/bendahmen/borulanta; weekly results sync for one team)"
)

#' Fetch the league page, optionally keeping a copy of exactly what was parsed.
#'
#' The snapshot is the difference between "the parser broke" and "the parser
#' broke and we no longer have the page it broke on", so a sync run keeps one.
fetch_league_page <- function(url = DREAMLEAGUES_URL, snapshot_path = NULL) {
  response <- httr::GET(url, httr::user_agent(SCRAPER_USER_AGENT), httr::timeout(30))
  httr::stop_for_status(response, task = paste("fetch", url))

  html <- httr::content(response, as = "text", encoding = "UTF-8")
  if (!is.null(snapshot_path)) {
    dir.create(dirname(snapshot_path), showWarnings = FALSE, recursive = TRUE)
    writeLines(html, snapshot_path, useBytes = TRUE)
  }
  html
}

#' Every fixture in the league, one row per match, with events nested.
#'
#' Returns the whole league rather than just our matches: the count of fixtures
#' parsed is the cheapest signal that the page still looks like the page we
#' think it is, and that check is worth more than the rows it discards.
parse_fixtures <- function(html) {
  page <- rvest::read_html(html)
  weeks <- rvest::html_elements(page, ".section-fixtures .accordion-leagues")

  purrr::map_dfr(weeks, parse_week)
}

parse_week <- function(week) {
  # "09 September 2026 - Week 1" — the week number is the site's own bookkeeping
  # and tells us nothing the date does not.
  header <- rvest::html_element(week, ".accordion-date") %>% rvest::html_text2()
  date <- as.Date(str_trim(str_remove(header, "\\s*-\\s*Week\\s*\\d+\\s*$")), format = "%d %B %Y")
  if (is.na(date)) {
    stop("could not read a fixture date from: ", header, call. = FALSE)
  }

  panels <- rvest::html_elements(week, ".panel.collapse-section")
  purrr::map_dfr(panels, parse_panel, date = date)
}

parse_panel <- function(panel, date) {
  title <- rvest::html_element(panel, ".accordion-title")
  text_of <- function(node, selector) {
    str_trim(rvest::html_text2(rvest::html_element(node, selector)))
  }

  # "0 : 0" on an unplayed fixture; the site pre-populates the whole season.
  score <- text_of(title, ".team-wrapper-blue")
  goals <- as.integer(str_match(score, "^(\\d+)\\s*:\\s*(\\d+)$")[, 2:3])
  if (anyNA(goals)) {
    stop("could not read a score from: ", score, call. = FALSE)
  }

  # The site's own fixture key. Kept for provenance and for reconciling a
  # postponement by hand, but deliberately not used to match rows: the league
  # restarts each season with a fresh set of ids, so an id is only unique
  # within a season while a date is unique full stop.
  fixture_id <- rvest::html_element(title, ".plus-info") %>%
    rvest::html_attr("href") %>%
    str_remove("^#info-")

  tibble(
    date = date,
    dl_match_id = fixture_id,
    home_team = text_of(title, ".team-1-wrapper"),
    away_team = text_of(title, ".team-2-wrapper"),
    home_goals = goals[[1]],
    away_goals = goals[[2]],
    events = list(parse_panel_events(panel))
  )
}

#' Goals and man of the match from one fixture's accordion.
#'
#' `side` is "home"/"away" because that is all the page knows; which of those is
#' us is decided later, once we know which side of the fixture we were on.
parse_panel_events <- function(panel) {
  empty <- tibble(
    side = character(),
    minute = integer(),
    event_type = character(),
    player = character()
  )

  rows <- rvest::html_elements(panel, ".accordion-content .row-info")
  if (length(rows) == 0) {
    return(empty)
  }

  parsed <- purrr::map_dfr(rows, parse_event_row)
  if (nrow(parsed) == 0) empty else parsed
}

parse_event_row <- function(row) {
  named_side <- function(selector) {
    value <- str_trim(rvest::html_text2(rvest::html_element(row, selector)))
    if (length(value) == 0 || is.na(value) || !nzchar(value)) NA_character_ else value
  }
  home <- named_side(".team-1-wrapper")
  away <- named_side(".team-2-wrapper")

  # An empty row is the site rendering a heading for a category with nothing in
  # it — every fixture has a CARDS row whether or not a card was shown.
  if (is.na(home) && is.na(away)) {
    return(NULL)
  }

  # Classify on the icon, which is what actually distinguishes the rows; the
  # label beside it is a fallback for the medal row, whose icon is dropped when
  # no man of the match was awarded.
  icons <- rvest::html_elements(row, ".icon-info img") %>% rvest::html_attr("src")
  label <- str_trim(rvest::html_text2(rvest::html_element(row, ".time-info p")))

  shows <- function(icon) any(str_detect(icons, icon), na.rm = TRUE)
  event_type <- if (shows("icon-ball")) {
    "goal"
  } else if (shows("icon-medal") || identical(label, "MOM")) {
    "mom"
  } else {
    return(NULL) # an event kind we do not model yet; ignore rather than guess
  }

  minute <- if (event_type == "goal") {
    suppressWarnings(as.integer(str_extract(label, "\\d+")))
  } else {
    NA_integer_
  }

  tibble(
    side = if (!is.na(home)) "home" else "away",
    minute = minute,
    event_type = event_type,
    player = coalesce(home, away)
  )
}

#' Narrow the league to our fixtures, with scores and events oriented to us.
#'
#' Every match is played on neutral ground, so home and away are the site's
#' bookkeeping rather than anything real; the only thing the distinction is
#' good for is knowing which end of the scoreline is ours.
our_fixtures <- function(fixtures, team = OUR_TEAM) {
  ours <- fixtures %>%
    filter(home_team == team | away_team == team)

  if (nrow(ours) == 0) {
    return(ours %>% transmute(
      date, dl_match_id,
      opponent = character(),
      goals_for = integer(),
      goals_against = integer(),
      events = list()
    ))
  }

  ours %>%
    mutate(
      we_are_home = home_team == team,
      opponent = if_else(we_are_home, away_team, home_team),
      goals_for = if_else(we_are_home, home_goals, away_goals),
      goals_against = if_else(we_are_home, away_goals, home_goals),
      events = purrr::map2(events, we_are_home, orient_events)
    ) %>%
    select(date, dl_match_id, opponent, goals_for, goals_against, events)
}

orient_events <- function(events, we_are_home) {
  our_side <- if (we_are_home) "home" else "away"
  events %>%
    mutate(team = if_else(side == our_side, "us", "them")) %>%
    select(team, minute, event_type, player)
}
