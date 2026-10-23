# Cards ----
#
# The bslib cards and panels the UI is assembled from. Anything eagerly built
# here is a card with no arguments to vary; anything that takes an argument or
# needs a unique input id is a function.


# The fee cards appear on both Home and Fees, so they are built rather than
# stored: Shiny needs a unique id per input, and the same card in two places
# would give two selectInputs the same one. The Fees tab keeps the original ids
# and the home page gets its own; the server keeps the two pickers on the same
# person, so switching on one tab carries to the other.
fee_panel_ui <- function(input_id, output_id) {
  card(
    class = "panel-card",
    card_body(
      div(class = "section-tag", "Fee Check"),
      div(
        class = "selection-shell",
        selectInput(input_id, "Choose your name", choices = NULL)
      ),
      div(class = "fee-label", "Current balance"),
      uiOutput(output_id),
      div(
        class = "fee-footnote",
        paste(
          "Every season you have played, charged under the fee rules in",
          "force at the time, less what you have paid."
        )
      )
    )
  )
}


payment_panel_ui <- function() {
  card(
    class = "payment-card",
    card_body(
      div(class = "section-tag", "Settle Up"),
      p(
        class = "payment-copy",
        "Balances move when I record the transfer, not when you send it."
      ),
      tags$a(
        href = "https://monzo.me/benjamindahmen8?h=Njfjz9",
        target = "_blank",
        class = "payment-button",
        "Pay via Monzo"
      ),
      div(
        class = "account-panel",
        div(class = "account-title", "Bank transfer details"),
        p(class = "account-detail", "Account Number: 94456363"),
        p(class = "account-detail", "Sort Code: 04-00-03"),
        p(class = "account-detail", "Name: Benjamin Dahmen")
      )
    )
  )
}


# Freshness ----
#
# The CSVs are bundled into the deployment, so the app is only ever as fresh as
# the last sync that was committed and deployed. Nothing on the page shows that:
# a stale countdown looks exactly like a live one, and reads more confidently
# than the results tabs it sits above. So the dates that bound the data are
# stated outright rather than left to be inferred from it.

#' Where the numbers came from and when, as one line under the strapline.
#'
#' Either half is dropped rather than rendered empty, so a checkout with no
#' league table on file says only what it knows.
#'
#' Both a full and a short wording are emitted and the stylesheet picks one.
#' The full line wraps to two on a phone, and the hero is the one part of this
#' layout that is held to a fixed height there; the scrape date alone is the
#' half worth keeping, because it is what the countdown beneath it depends on.
#' Whichever is hidden is display:none, so it leaves the accessibility tree too
#' and nothing is read out twice.
freshness_note <- function(matches, league_table) {
  last_result <- if (nrow(matches) > 0) {
    paste("Results to", format(max(matches$date), "%d %b %Y"))
  }
  last_read <- if (nrow(league_table) > 0) {
    format(max(league_table$scraped_on), "%d %b %Y")
  }

  if (is.null(last_result) && is.null(last_read)) {
    return(NULL)
  }

  full <- paste(
    c(last_result, if (!is.null(last_read)) paste("league page read", last_read)),
    collapse = " \u00b7 "
  )
  short <- if (!is.null(last_read)) paste("Page read", last_read) else last_result

  div(
    class = "hero-freshness",
    span(class = "freshness-full", full),
    span(class = "freshness-short", short)
  )
}


#' A warning on the countdown when the fixture list behind it has gone stale.
#'
#' Only when it has: a provenance line on every visit is noise, and this is the
#' one card that keeps counting down confidently while going wrong. The league
#' table's scrape date stands in for the fixture list's own, which it does not
#' have — both are snapshot files the same sync run replaces wholesale.
stale_fixture_note <- function(scraped_on, today) {
  if (length(scraped_on) == 0) {
    return(NULL)
  }

  last_read <- max(scraped_on)
  if (is.na(last_read) || as.integer(today - last_read) <= STALE_FIXTURES_DAYS) {
    return(NULL)
  }

  div(
    class = "home-stale",
    paste0(
      "The fixture list was last read on ", format(last_read, "%d %b"),
      ", so this may be out of date."
    )
  )
}


#' Data problems on the page, rather than in a log nobody opens.
#'
#' validate_app_data() warns at startup, which reaches the console or the
#' deployment log — exactly where it will not be seen. Quiet by design: these
#' are notes to whoever maintains the CSVs rather than errors a reader can act
#' on, and the panel is absent entirely when there is nothing wrong.
data_health_banner <- function(problems) {
  if (length(problems) == 0) {
    return(NULL)
  }

  div(
    class = "data-health",
    div(class = "data-health-title", "Data check"),
    tags$ul(class = "data-health-list", lapply(problems, tags$li))
  )
}


#' The last result, with as much of the detail as we happen to hold.
#'
#' Opponent, scorers and man of the match all came in with the sync, so nothing
#' played before it exists has any of them. Each is therefore dropped from the
#' card rather than rendered blank: a line reading "Scorers: —" on every match
#' in the archive is worse than no line.
last_match_card_ui <- function(summary, seasons) {
  if (is.null(summary)) {
    return(card(
      class = "home-card",
      card_body(
        div(class = "section-tag", "Last match"),
        div(class = "empty-state", "No matches recorded yet.")
      )
    ))
  }

  played <- summary$match
  season_label <- seasons$label[match(played$season_id, seasons$season_id)]

  detail_line <- function(label, value) {
    div(class = "home-detail", span(class = "home-detail-label", label), span(value))
  }

  card(
    class = "home-card",
    card_body(
      div(class = "section-tag", "Last match"),
      div(
        class = "home-scoreline",
        span(class = "home-score", played$result),
        result_badge(played$points)
      ),
      div(
        class = "home-card-meta",
        paste(
          c(
            format_match_date(played$date),
            if (!is.na(played$opponent)) paste("v", played$opponent),
            if (!is.na(season_label)) season_label
          ),
          collapse = " \u00b7 "
        )
      ),
      div(
        class = "home-details",
        detail_line("Squad", paste(summary$squad_size, "players")),
        if (nrow(summary$scorers) > 0) {
          detail_line("Scorers", listed_scorers(summary$scorers))
        },
        if (!is.na(summary$mom)) detail_line("Man of the match", summary$mom)
      )
    )
  )
}


#' "20:20 · Pitch 1", or nothing when the page gave neither.
kickoff_pitch_line <- function(fixture) {
  parts <- c(fixture$kickoff, fixture$pitch)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (length(parts) == 0) {
    return(NULL)
  }
  div(class = "home-card-meta", paste(parts, collapse = " \u00b7 "))
}


#' The next fixture, or an honest note about why there is not one.
#'
#' The two empty cases are different and worth separating: a fixture list that
#' has run out means the season is done or the sync is overdue, while no file at
#' all means the sync has not run since fixtures started being recorded.
next_match_card_ui <- function(fixture, have_fixtures, scraped_on = NULL,
                               today = Sys.Date()) {
  if (is.null(fixture)) {
    return(card(
      class = "home-card",
      card_body(
        div(class = "section-tag", "Next match"),
        div(
          class = "empty-state",
          if (have_fixtures) {
            "No fixtures left on the league page. Run the sync once the new season is up."
          } else {
            "No fixture list yet — run the sync to pull one in."
          }
        )
      )
    ))
  }

  days_away <- as.integer(fixture$date - today)
  when <- case_when(
    days_away == 0 ~ "Tonight",
    days_away == 1 ~ "Tomorrow",
    TRUE ~ paste("In", days_away, "days")
  )

  card(
    class = "home-card",
    card_body(
      div(class = "section-tag", "Next match"),
      div(class = if (days_away == 0) "home-when is-today" else "home-when", when),
      div(class = "home-opponent", coalesce(fixture$opponent, "Opponent to be confirmed")),
      div(class = "home-card-meta", format_match_date(fixture$date)),
      kickoff_pitch_line(fixture),
      stale_fixture_note(scraped_on, today)
    )
  )
}


#' The league standings, trimmed to what fits and with our row picked out.
#'
#' Goals for and against are dropped: this is context on a landing page, not
#' the Matches tab, and goal difference carries the same information in one
#' column. The scrape date is on the card because the table ages between syncs
#' and a stale table that cannot be dated is a stale table nobody questions.
league_table_ui <- function(league_table) {
  if (nrow(league_table) == 0) {
    return(card(
      class = "table-card league-table-card",
      card_body(
        div(class = "section-tag", "League table"),
        div(class = "empty-state", "No league table yet — run the sync to pull one in.")
      )
    ))
  }

  card(
    class = "table-card league-table-card",
    card_head(
      "League table", "Where we sit",
      paste(
        "From the league's own page, as it stood on",
        format(max(league_table$scraped_on), "%d %B %Y.")
      )
    ),
    card_body(dataTableOutput("league_table"))
  )
}


#' The heading every card here shares: a short category kicker above a
#' descriptive title. They are separate arguments because they say different
#' things — passing one value for both prints it twice, stacked.
#'
#' The subtitle is a string or any tag, because some cards have the server
#' write it through a uiOutput.
card_head <- function(tag, title, subtitle = NULL) {
  card_header(
    div(class = "section-tag", tag),
    h2(class = "table-title", title),
    if (is.character(subtitle)) p(class = "table-subtitle", subtitle) else subtitle
  )
}


#' A headed card with its contents in the body: the shape of nearly every card
#' below the home page.
table_card <- function(tag, title, subtitle = NULL, ...) {
  card(class = "table-card", card_head(tag, title, subtitle), card_body(...))
}


#' One figure in a row of them: a label, the number, and a line under it.
#'
#' `family` picks the class prefix, since the match and player pages style
#' theirs as `match-detail-*` and the fee cards as `fee-overview-*`.
stat_tile <- function(label, value, note = NULL, family = "match-detail") {
  div(
    class = paste0(family, "-stat"),
    div(class = paste0(family, "-label"), label),
    div(class = paste0(family, "-value"), value),
    if (!is.null(note)) div(class = paste0(family, "-note"), note)
  )
}


#' Placeholder shown when the selected season has no matches recorded yet.
empty_season_card <- function(message) {
  card(
    class = "table-card",
    card_body(div(class = "empty-state", message))
  )
}


season_form_card <- table_card(
  "Season form", "How the season is trending",
  "Each point averages the last five matches. The first four average what there is.",
  div(
    class = "selection-shell",
    selectInput(
      "form_metric",
      "Show",
      choices = c(
        "Points per match" = "points",
        "Goals scored and conceded" = "goals",
        "Goal difference" = "goal_difference",
        "Squad size" = "squad_size"
      )
    )
  ),
  plotOutput("season_form_plot", height = "380px")
)


match_summary_ui <- function(selected_match) {
  outcome <- case_when(
    selected_match$points == 3 ~ "Win",
    selected_match$points == 1 ~ "Draw",
    TRUE ~ "Loss"
  )

  div(
    class = "match-detail-summary",
    stat_tile(
      "Result", selected_match$result,
      paste(
        c(
          format(selected_match$date, "%d %B %Y"),
          na.omit(selected_match$opponent),
          outcome
        ),
        collapse = " — "
      )
    ),
    stat_tile("Points", selected_match$points, "Out of 3"),
    stat_tile("Squad", selected_match$squad_size, "On the sheet that night")
  )
}


#' What the leaderboard above it actually covers.
#'
#' Stated rather than assumed, because for anything played before the sync it
#' is not all-time and a table that does not say so invites being read as if it
#' were. Appearances are counted over the same matches as the goals, so the
#' rate divides like by like.
scorer_coverage_ui <- function(coverage) {
  if (coverage$observed == 0) {
    return(p(
      class = "table-subtitle",
      paste(
        "No match in the selected seasons has its goals on file.",
        "They start with the sync."
      )
    ))
  }

  p(
    class = "table-subtitle",
    if (coverage$observed == coverage$total) {
      paste0(
        "All ", coverage$total, " matches in the selected seasons have ",
        "their goals on file."
      )
    } else {
      paste0(
        coverage$observed, " of the ", coverage$total, " matches in the ",
        "selected seasons have their goals on file. The rest were played ",
        "before the sync existed and are left out, appearances included."
      )
    }
  )
}


#' Goals and man of the match, with the window they cover stated on the card.
scorer_card <- table_card(
  "Scorers", "Goals and man of the match", uiOutput("scorer_coverage"),
  dataTableOutput("scorers")
)


# Player page ----

#' A player's season at a glance: four numbers, then the record behind them.
player_headline_ui <- function(profile) {
  div(
    class = "match-detail-summary",
    stat_tile(
      "Appearances", profile$appearances,
      paste0("of ", profile$matches, " matches")
    ),
    stat_tile(
      "Turnout",
      if (is.na(profile$attendance_rate)) "\u2014" else paste0(round(profile$attendance_rate * 100), "%"),
      "share of matches played"
    ),
    stat_tile(
      "Points per match",
      if (is.na(profile$present$points_per_match)) "\u2014" else format(round(profile$present$points_per_match, 2), nsmall = 2),
      "when playing"
    ),
    # Goals only earn a tile once there is a match they could have been
    # scored in. Before that the sync has simply not run yet, and a zero
    # would read as a barren season rather than an empty file.
    if (profile$covered > 0) {
      stat_tile(
        "Goals", profile$goals,
        paste0(
          profile$mom, " man of the match, over ", profile$covered,
          " recorded"
        )
      )
    }
  )
}


differential_note <- function(profile) {
  if (is.na(profile$differential)) {
    return(p(
      class = "table-subtitle",
      paste(
        "No comparison to draw: there is no match in the selected",
        "seasons on the other side of it."
      )
    ))
  }

  direction <- if (profile$differential >= 0) "better" else "worse"
  p(
    class = "table-subtitle",
    paste0(
      "The side takes ", format(round(abs(profile$differential), 2), nsmall = 2),
      " points a match ", direction, " with ", profile$player,
      " than without. That is a description of how those nights went, not ",
      "of what they did: it holds nothing else about the match fixed. The ",
      "player effects tab does hold the rest of the lineup fixed."
    )
  )
}


player_page <- tagList(
  # The roster, and the way in to the page below it. This was the Attendance
  # tab: the same statistic for everybody at once that the card under it
  # gives one player at a time, which made two tabs out of one subject.
  table_card(
    "Attendance", "Who turns up",
    paste(
      "Share of matches played, and how those matches went.",
      "Pick a row for that player's page."
    ),
    dataTableOutput("attendance_list")
  ),
  table_card(
    "Player", "One player at a time", uiOutput("profile_player_note"),
    uiOutput("player_headline"),
    h3(class = "match-lineup-title", "Appearances"),
    plotOutput("appearance_timeline", height = "140px")
  ),
  table_card(
    "With and without", "How the side does either way", uiOutput("differential_note"),
    dataTableOutput("with_without")
  )
)


#' Our record against each side, or an honest note about why there is not one.
#'
#' The opponent arrived with the sync, so a window made only of older matches
#' has nobody to group by. That is a different thing from having played nobody
#' and is worth saying, rather than showing an empty table.
opponent_record_card <- table_card(
  "Head to head", "How we do against each side",
  paste(
    "Points per match rather than total points: we have met some",
    "sides twice as often as others."
  ),
  uiOutput("opponent_record")
)


#' What is left to play, and what the standings say about it.
#'
#' On the home page, with the next match and the standings it is made of: the
#' fixture list describes the current league season, which is not a fee season,
#' so scoping it by the picker would filter one thing by the boundaries of
#' another. It sat on Matches once and had to say so in its own subtitle. Here
#' nothing takes the picker, so there is nothing to say.
run_in_card <- table_card(
  "Run-in", "Who is left",
  "Every fixture still to come, with where that side sits in the league.",
  dataTableOutput("run_in")
)


#' Every result, and the one you pick out of it.
#'
#' One card rather than two: the table was a list of every match and the card
#' under it opened with a dropdown listing the same matches again, in the same
#' order, immediately below. The table is the picker now.
every_result_card <- table_card(
  "Matches", "Every result",
  paste(
    "Newest first, for the seasons you have ticked. Pick a row for",
    "the lineup that night."
  ),
  dataTableOutput("matches")
)

match_detail_card <- table_card(
  "Match detail", "Lineup and season context",
  "Who played, and how much of that season they turned up for.",
  uiOutput("match_summary"),
  h3(class = "match-lineup-title", "Lineup"),
  dataTableOutput("match_lineup")
)


fee_overview_summary <- function(overview) {
  balance_label <- case_when(
    overview$balance > 0 ~ "Still owed",
    overview$balance < 0 ~ "Credit",
    TRUE ~ "Settled"
  )

  div(
    class = "fee-overview-summary",
    stat_tile(
      "Match charges", format_fee_amount(overview$total_charges), family = "fee-overview"
    ),
    stat_tile(
      "Payments recorded", format_fee_amount(overview$total_payments),
      family = "fee-overview"
    ),
    stat_tile(balance_label, format_fee_amount(overview$balance), family = "fee-overview")
  )
}


#' Everyone's balance at once, for the person actually chasing the money.
#'
#' Outstanding and credit are shown apart rather than netted: the net is what
#' the pot is short, but a credit belongs to somebody who cannot be asked for
#' money they have already paid, so it is not an offset against what is owed.
balance_totals_ui <- function(totals) {
  div(
    class = "fee-overview-summary",
    stat_tile("Still owed", format_fee_amount(totals$outstanding), family = "fee-overview"),
    stat_tile("In credit", format_fee_amount(totals$credit), family = "fee-overview"),
    stat_tile("Settled up", totals$settled, family = "fee-overview")
  )
}


everyone_panel <- table_card(
  "Everyone", "Who is square and who is not",
  paste(
    "All seasons, same rules as your own balance. Rows in amber owe",
    "something; rows in green are owed."
  ),
  uiOutput("balance_totals"),
  dataTableOutput("all_balances")
)


fee_history_panel <- tagList(
  table_card(
    "Payment history", "How your balance is calculated",
    "All seasons, each match charged under the rules in force at the time.",
    uiOutput("fee_overview_summary")
  ),
  table_card(
    "Charges", "Match-by-match charges",
    p(
      class = "fee-history-note",
      "Shares are rounded to the penny here. The totals above are not."
    ),
    dataTableOutput("match_charges")
  ),
  table_card("Payments", "Payments made", NULL, dataTableOutput("payment_history"))
)


regression_explanation_ui <- function(regression_results, min_appearances, coverage,
                                      estimator = "ols") {
  included <- unique(regression_results$player)

  opponent_paragraph <- if (coverage$observed == 0) {
    p(
      paste(
        "No match in this window has its opponent on file, so nothing",
        "adjusts for who we played."
      )
    )
  } else {
    p(
      paste0(
        "The opponent is on file for ", coverage$observed, " of ",
        coverage$total, " matches here. Those matches also control for ",
        "opponent strength: the other side's goal difference per game ",
        "against everyone except us that season, centred so that 0 is an ",
        "average opponent. Matches with no opponent recorded take the ",
        "average and an indicator, which keeps their own level out of the ",
        "player coefficients."
      )
    )
  }

  estimator_paragraph <- if (estimator == "ridge") {
    tagList(
      p(
        paste0(
          "These are ridge estimates. The plain fit spends a column on ",
          "each of the ", length(included), " players over a few dozen ",
          "matches, so somebody with three appearances is measured off ",
          "three nights and lands at whichever extreme those nights ",
          "went. Ridge pulls every estimate toward the average player ",
          "by an amount inversely proportional to how much is known ",
          "about them, so the thin players move a long way and the ",
          "regulars barely move. A coefficient here is a deviation from ",
          "the average player rather than a share of the scoreline, and ",
          "there are no confidence intervals: ridge has no usable ",
          "standard error."
        )
      ),
      p(
        paste(
          "The estimates come out very close to zero, and that is the",
          "result rather than a display problem. The penalty is chosen",
          "by cross-validation, which is asking how much of a match",
          "the lineup predicts out of sample, and the answer here is",
          "almost none. The ordering still means something; the",
          "magnitudes are telling you not to lean on it."
        )
      )
    )
  } else {
    p(
      paste0(
        "Each column is a separate match-level regression of that outcome ",
        "on indicators for the ", length(included), " players with at ",
        "least ", min_appearances, " appearances in the selected seasons. ",
        "The coefficient says how the outcome moves when that player is on ",
        "the pitch, holding the rest of the lineup fixed. The number in ",
        "brackets is the OLS p-value, and with this many players a couple ",
        "below 0.05 is what chance alone produces."
      )
    )
  }

  card_body(
    estimator_paragraph,
    p(
      paste(
        "Anyone below the appearance threshold has no indicator of their",
        "own and sits in the residual. There is no time trend. These are",
        "descriptive: nothing here separates a good player from one who",
        "happens to play in good teams."
      )
    ),
    opponent_paragraph
  )
}


#' The estimator toggle, above every card it changes.
#'
#' Not inside the coefficient-plot card next to the outcome picker, because it
#' governs all three outputs on the tab. Radio buttons rather than a switch so
#' the labels can carry what the choice means.
estimator_control <- div(
  class = "selection-shell estimator-shell",
  radioButtons(
    "regression_estimator",
    "Estimates",
    choices = c(
      "Raw (OLS)" = "ols",
      "Shrunk toward the average player (ridge)" = "ridge"
    ),
    selected = "ols",
    inline = TRUE
  )
)


# Not a table_card(): the server renders the card_body itself.
regression_explanation <- card(
  class = "table-card",
  card_head("Method", "How to read the player effects"),
  uiOutput("regression_explanation")
)


#' The Player effects tab, from the estimator toggle down. Its empty state is
#' arranged in app.R with the other tabs'.
player_effects_page <- tagList(
  estimator_control,
  layout_columns(
    col_widths = c(6, 6),
    table_card(
      "Visualise", "Attack and defence",
      paste(
        "Top right is more goals scored and fewer conceded, holding the rest",
        "of the lineup fixed."
      ),
      plotOutput("attack_defence_plot", height = "680px")
    ),
    table_card(
      "Visualise", "Coefficient plot",
      "Dots are estimates and lines are 95% confidence intervals.",
      selectInput(
        "regression_outcome",
        "Outcome",
        choices = setNames(names(OUTCOME_LABELS), OUTCOME_LABELS)
      ),
      plotOutput("coefficient_plot", height = "560px")
    )
  ),
  table_card(
    "Player effects", "Player by player", uiOutput("regression_table_note"),
    dataTableOutput("player_regressions")
  ),
  regression_explanation
)


#' Season tick boxes, newest first, each captioned with when that season ran.
season_picker <- function(app_data) {
  spans <- season_span(app_data$seasons, app_data$matches) |>
    arrange(desc(start_date))

  checkboxGroupInput(
    "seasons",
    "Seasons",
    choiceNames = purrr::map2(
      spans$label,
      spans$span,
      ~ tagList(
        span(class = "season-name", .x),
        span(class = "season-span", .y)
      )
    ),
    choiceValues = spans$season_id,
    # Everything is in scope until you narrow it.
    selected = spans$season_id,
    inline = TRUE
  )
}
