library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(glue)

# R/fees.R defines parse_match_date(), which R/seasons.R needs at load time.
source("R/fees.R")
source("R/seasons.R")
source("R/analysis.R")
source("R/data.R")

# Read and validate every CSV once per process, not once per visitor.
app_data <- load_app_data()

app_theme <- bs_theme(
    version = 5,
    bg = "#f7f9f6",
    fg = "#16241f",
    primary = "#0850AB",   # navy blue (oklch(45% 0.16 258)) — was the lime green
    secondary = "#AB4400", # amber (oklch(52% 0.15 45))
    success = "#006D1E",   # green (oklch(46% 0.15 148))
    info = "#0850AB",
    warning = "#AB4400",
    danger = "#AB4400",
    base_font = font_collection("Barlow", "Avenir Next", "Trebuchet MS", "sans-serif"),
    heading_font = font_collection(
        "Barlow Condensed",
        "Futura",
        "Avenir Next Condensed",
        "Arial Narrow",
        "sans-serif"
    ),
    code_font = font_collection("SFMono-Regular", "Menlo", "monospace")
)

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

format_fee_amount <- function(amount) {
    sign <- if (amount < 0) "-" else ""
    paste0(sign, "£", formatC(abs(amount), format = "f", digits = 2))
}

# Home page cards ----

#' "Wed 19 Aug 2026" — the weekday earns its place on a game that is always
#' on a Wednesday, because a fixture that is not is worth noticing.
format_match_date <- function(date) format(date, "%a %d %b %Y")

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

# A match week without a sync. The game is weekly and the sync runs after it,
# so a fixture list older than this has missed at least one.
STALE_FIXTURES_DAYS <- 8L

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

result_badge <- function(points) {
    label <- case_when(points == 3 ~ "WIN", points == 1 ~ "DRAW", TRUE ~ "LOSS")
    class <- case_when(points == 3 ~ "win", points == 1 ~ "draw", TRUE ~ "loss")
    span(class = paste("result-badge", class), label)
}

#' A comma list that reads like a sentence, with a tally where there is one.
listed_scorers <- function(scorers) {
    paste(
        if_else(scorers$goals > 1, paste0(scorers$player, " (", scorers$goals, ")"), scorers$player),
        collapse = ", "
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
        card_header(
            div(class = "section-tag", "League table"),
            h2(class = "table-title", "Where we sit"),
            p(
                class = "table-subtitle",
                paste(
                    "From the league's own page, as it stood on",
                    format(max(league_table$scraped_on), "%d %B %Y.")
                )
            )
        ),
        card_body(dataTableOutput("league_table"))
    )
}

league_table_table <- function(league_table) {
    datatable(
        league_table %>%
            transmute(
                Pos = position,
                Team = team,
                P = played, W = won, D = drawn, L = lost,
                GD = goal_difference,
                Pts = points
            ),
        rownames = FALSE,
        class = "nowrap",
        options = list(
            dom = "t",
            pageLength = nrow(league_table),
            ordering = FALSE,
            autoWidth = TRUE,
            scrollX = TRUE,
            rowCallback = JS(
                "function(row, data) {",
                sprintf("  if (data[1] === %s) { $(row).addClass('is-us'); }", jsonlite::toJSON(OUR_TEAM, auto_unbox = TRUE)),
                "}"
            )
        )
    )
}

fee_overview_summary <- function(overview) {
    balance_label <- case_when(
        overview$balance > 0 ~ "Still owed",
        overview$balance < 0 ~ "Credit",
        TRUE ~ "Settled"
    )

    div(
        class = "fee-overview-summary",
        div(
            class = "fee-overview-stat",
            div(class = "fee-overview-label", "Match charges"),
            div(class = "fee-overview-value", format_fee_amount(overview$total_charges))
        ),
        div(
            class = "fee-overview-stat",
            div(class = "fee-overview-label", "Payments recorded"),
            div(class = "fee-overview-value", format_fee_amount(overview$total_payments))
        ),
        div(
            class = "fee-overview-stat",
            div(class = "fee-overview-label", balance_label),
            div(class = "fee-overview-value", format_fee_amount(overview$balance))
        )
    )
}

match_charge_table <- function(match_charges) {
    table_data <- match_charges %>%
        transmute(
            Date = format(date, "%d %b %Y"),
            Season = season,
            Result = result,
            `Played?` = if_else(played, "Yes", "No"),
            `Squad size` = `Squad size`,
            Charge = charge,
            Explanation = explanation
        )

    datatable(
        table_data,
        rownames = FALSE,
        class = "nowrap",
        options = list(
            dom = "tip",
            pageLength = 12,
            ordering = FALSE,
            autoWidth = TRUE,
            scrollX = TRUE
        )
    ) %>%
        formatCurrency(columns = "Charge", currency = "£", digits = 2)
}

payment_history_table <- function(payment_history) {
    table_data <- payment_history %>%
        transmute(
            Date = format(date, "%d %b %Y"),
            Payment = amount
        )

    datatable(
        table_data,
        rownames = FALSE,
        class = "nowrap",
        options = list(
            dom = "t",
            ordering = FALSE,
            autoWidth = TRUE,
            scrollX = TRUE,
            language = list(emptyTable = "No payments recorded yet.")
        )
    ) %>%
        formatCurrency(columns = "Payment", currency = "£", digits = 2)
}

fee_history_panel <- tagList(
    card(
        class = "table-card",
        card_header(
            div(class = "section-tag", "Payment history"),
            h2(class = "table-title", "How your balance is calculated"),
            p(
                class = "table-subtitle",
                "All seasons, each match charged under the rules in force at the time."
            )
        ),
        card_body(uiOutput("fee_overview_summary"))
    ),
    card(
        class = "table-card",
        card_header(
            div(class = "section-tag", "Charges"),
            h2(class = "table-title", "Match-by-match charges"),
            p(
                class = "fee-history-note",
                "Shares are rounded to the penny here. The totals above are not."
            )
        ),
        card_body(dataTableOutput("match_charges"))
    ),
    card(
        class = "table-card",
        card_header(
            div(class = "section-tag", "Payments"),
            h2(class = "table-title", "Payments made")
        ),
        card_body(dataTableOutput("payment_history"))
    )
)

#' A table in a card, headed the way every other card here is headed: a short
#' category kicker above a descriptive title. They are separate arguments
#' because they say different things — passing one value for both prints it
#' twice, stacked.
table_card_ui <- function(tag, title, subtitle, output_id) {
    card(
        class = "table-card",
        card_header(
            div(class = "section-tag", tag),
            h2(class = "table-title", title),
            p(class = "table-subtitle", subtitle)
        ),
        card_body(
            dataTableOutput(output_id)
        )
    )
}

regression_table <- function(regression_results) {
    # DT counts from zero, and only over the data columns because rownames are
    # off. Each visible outcome column is told to sort on its hidden partner.
    column_index <- function(name) match(name, names(regression_results)) - 1L
    # unname(), because a named columnDefs list is not valid DataTables options.
    visible <- unname(vapply(names(REGRESSION_TABLE_COLUMNS), column_index, integer(1)))
    hidden <- unname(vapply(unname(REGRESSION_TABLE_COLUMNS), column_index, integer(1)))

    datatable(
        regression_results,
        rownames = FALSE,
        class = "nowrap",
        options = list(
            dom = "tip",
            pageLength = 25,
            order = list(list(1, "desc")),
            autoWidth = TRUE,
            scrollX = TRUE,
            columnDefs = c(
                list(list(targets = hidden, visible = FALSE)),
                purrr::map2(
                    visible, hidden,
                    ~ list(targets = .x, orderData = .y, className = "dt-right")
                )
            )
        )
    )
}

attack_defence_plot <- function(regression_results) {
    plot_data <- regression_results %>%
        filter(outcome %in% c("goals_scored", "goals_conceded", "goal_difference")) %>%
        select(player, appearances, outcome, estimate) %>%
        pivot_wider(names_from = outcome, values_from = estimate) %>%
        mutate(defensive_effect = -goals_conceded)

    ggplot(
        plot_data,
        aes(x = goals_scored, y = defensive_effect, size = appearances, color = goal_difference)
    ) +
        geom_vline(xintercept = 0, color = "#8a938d", linewidth = 0.4) +
        geom_hline(yintercept = 0, color = "#8a938d", linewidth = 0.4) +
        geom_point(alpha = 0.8) +
        ggrepel::geom_label_repel(
            aes(label = player),
            seed = 20260820,
            size = 3.2,
            fontface = "bold",
            fill = "white",
            color = "#16241f",
            label.size = 0.2,
            box.padding = 0.45,
            point.padding = 0.3,
            min.segment.length = 0,
            max.overlaps = Inf,
            show.legend = FALSE
        ) +
        scale_color_gradient2(
            low = "#AB4400",
            mid = "#16241f",
            high = "#006D1E",
            midpoint = 0,
            name = "Goal difference"
        ) +
        scale_size_area(max_size = 14, name = "Appearances") +
        scale_x_continuous(expand = expansion(mult = 0.2)) +
        scale_y_continuous(expand = expansion(mult = 0.2)) +
        labs(
            x = "Goals scored: coefficient",
            y = "Goals conceded: coefficient, sign flipped"
        ) +
        coord_equal(clip = "off") +
        theme_minimal(base_size = 12) +
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom",
            plot.margin = margin(12, 32, 12, 32)
        )
}

coefficient_plot <- function(regression_results, selected_outcome) {
    outcome_labels <- c(
        points = "Points",
        goals_scored = "Goals scored",
        goals_conceded = "Goals conceded",
        goal_difference = "Goal difference"
    )

    plot_data <- regression_results %>%
        filter(outcome == selected_outcome) %>%
        arrange(estimate) %>%
        mutate(player = factor(player, levels = player))

    ggplot(plot_data, aes(x = player, y = estimate)) +
        geom_hline(yintercept = 0, color = "#8a938d", linewidth = 0.4) +
        geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0) +
        geom_point(color = "#0850AB", size = 2.6) +
        coord_flip() +
        labs(
            x = NULL,
            y = paste0(
                outcome_labels[[selected_outcome]],
                " coefficient with 95% confidence interval"
            )
        ) +
        theme_minimal(base_size = 12) +
        theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank()
        )
}

plot_card_ui <- function(title, subtitle, output_id, height) {
    card(
        class = "table-card",
        card_header(
            div(class = "section-tag", "Visualise"),
            h2(class = "table-title", title),
            p(class = "table-subtitle", subtitle)
        ),
        card_body(plotOutput(output_id, height = height))
    )
}

regression_explanation_ui <- function(regression_results, min_appearances, coverage) {
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

    card_body(
        p(
            paste0(
                "Each column is a separate match-level regression of that outcome ",
                "on indicators for the ", length(included), " players with at ",
                "least ", min_appearances, " appearances in the selected seasons. ",
                "The coefficient says how the outcome moves when that player is on ",
                "the pitch, holding the rest of the lineup fixed. The number in ",
                "brackets is the OLS p-value."
            )
        ),
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

regression_explanation <- card(
    class = "table-card",
    card_header(
        div(class = "section-tag", "Method"),
        h2(class = "table-title", "How to read the player effects")
    ),
    uiOutput("regression_explanation")
)

#' Season tick boxes, newest first, each captioned with when that season ran.
season_picker <- function(app_data) {
    spans <- season_span(app_data$seasons, app_data$matches) %>%
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

#' Placeholder shown when the selected season has no matches recorded yet.
empty_season_card <- function(message) {
    card(
        class = "table-card",
        card_body(div(class = "empty-state", message))
    )
}

match_plot_theme <- function() {
    theme_minimal(base_size = 12) +
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
}

season_form_plot <- function(season_form, selected_metric) {
    if (selected_metric == "points") {
        return(
            ggplot(season_form, aes(x = date, y = rolling_points)) +
                geom_line(color = "#0850AB", linewidth = 1) +
                geom_point(color = "#0850AB", size = 2.4) +
                scale_y_continuous(limits = c(0, 3)) +
                labs(y = "Points per match", x = NULL) +
                match_plot_theme()
        )
    }

    if (selected_metric == "goals") {
        plot_data <- season_form %>%
            select(date, rolling_goals_scored, rolling_goals_conceded) %>%
            pivot_longer(
                cols = -date,
                names_to = "metric",
                values_to = "value"
            ) %>%
            mutate(
                metric = recode(
                    metric,
                    rolling_goals_scored = "Goals scored",
                    rolling_goals_conceded = "Goals conceded"
                )
            )

        return(
            ggplot(plot_data, aes(x = date, y = value, color = metric)) +
                geom_line(linewidth = 1) +
                geom_point(size = 2.2) +
                scale_color_manual(
                    values = c(
                        "Goals scored" = "#0850AB",
                        "Goals conceded" = "#AB4400"
                    ),
                    name = NULL
                ) +
                labs(y = "Goals per match", x = NULL) +
                match_plot_theme()
        )
    }

    if (selected_metric == "goal_difference") {
        return(
            ggplot(season_form, aes(x = date, y = rolling_goal_difference)) +
                geom_hline(yintercept = 0, color = "#8a938d", linewidth = 0.4) +
                geom_line(color = "#006D1E", linewidth = 1) +
                geom_point(color = "#006D1E", size = 2.4) +
                labs(y = "Goal difference per match", x = NULL) +
                match_plot_theme()
        )
    }

    ggplot(season_form, aes(x = date, y = rolling_squad_size)) +
        geom_line(color = "#006D1E", linewidth = 1) +
        geom_point(color = "#006D1E", size = 2.4) +
        labs(y = "Players per match", x = NULL) +
        match_plot_theme()
}

season_form_card <- card(
    class = "table-card",
    card_header(
        div(class = "section-tag", "Season form"),
        h2(class = "table-title", "How the season is trending"),
        p(
            class = "table-subtitle",
            "Each point averages the last five matches. The first four average what there is."
        )
    ),
    card_body(
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
)

match_summary_ui <- function(selected_match) {
    outcome <- case_when(
        selected_match$points == 3 ~ "Win",
        selected_match$points == 1 ~ "Draw",
        TRUE ~ "Loss"
    )

    div(
        class = "match-detail-summary",
        div(
            class = "match-detail-stat",
            div(class = "match-detail-label", "Result"),
            div(class = "match-detail-value", selected_match$result),
            div(
                class = "match-detail-note",
                paste(
                    c(
                        format(selected_match$date, "%d %B %Y"),
                        na.omit(selected_match$opponent),
                        outcome
                    ),
                    collapse = " — "
                )
            )
        ),
        div(
            class = "match-detail-stat",
            div(class = "match-detail-label", "Points"),
            div(class = "match-detail-value", selected_match$points),
            div(class = "match-detail-note", "Out of 3")
        ),
        div(
            class = "match-detail-stat",
            div(class = "match-detail-label", "Squad"),
            div(class = "match-detail-value", selected_match$squad_size),
            div(class = "match-detail-note", "On the sheet that night")
        )
    )
}

match_lineup_table <- function(lineup) {
    datatable(
        lineup %>%
            transmute(
                Player = player,
                `Season appearances` = season_appearances,
                `Attendance rate` = attendance_rate
            ),
        rownames = FALSE,
        class = "nowrap",
        options = list(
            dom = "t",
            ordering = FALSE,
            autoWidth = TRUE,
            scrollX = TRUE
        )
    ) %>%
        formatPercentage(columns = "Attendance rate", digits = 0)
}

scorer_display_table <- function(scorers) {
    datatable(
        scorers %>%
            transmute(
                Player = player,
                Goals = goals,
                MOM = mom,
                Appearances = appearances,
                `Goals per game` = round(goals_per_appearance, 2)
            ),
        rownames = FALSE,
        class = "nowrap",
        options = list(
            dom = "tip",
            pageLength = 15,
            order = list(list(1, "desc")),
            autoWidth = TRUE,
            scrollX = TRUE,
            language = list(
                emptyTable = "No goals on file yet \u2014 they arrive with the sync."
            )
        )
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
scorer_card <- card(
    class = "table-card",
    card_header(
        div(class = "section-tag", "Scorers"),
        h2(class = "table-title", "Goals and man of the match"),
        uiOutput("scorer_coverage")
    ),
    card_body(dataTableOutput("scorers"))
)

match_detail_card <- card(
    class = "table-card",
    card_header(
        div(class = "section-tag", "Match detail"),
        h2(class = "table-title", "Lineup and season context"),
        p(
            class = "table-subtitle",
            "Who played, and how much of that season they turned up for."
        )
    ),
    card_body(
        div(
            class = "selection-shell",
            selectInput("selected_match", "Match", choices = NULL)
        ),
        uiOutput("match_summary"),
        h3(class = "match-lineup-title", "Lineup"),
        dataTableOutput("match_lineup")
    )
)

# User interface ----
ui <- page_fluid(
    theme = app_theme,
    title = "Borulanta",
    tags$head(
        tags$meta(
            name = "viewport",
            content = "width=device-width, initial-scale=1"
        ),
        tags$link(rel = "stylesheet", type = "text/css", href = "borulanta.css")
    ),
    div(
        class = "app-shell",
        div(
            class = "app-hero",
            div(
                class = "hero-crest",
                tags$img(src = "borulanta-crest.png", alt = "Borulanta crest")
            ),
            div(
                class = "hero-text",
                div(class = "hero-kicker", "Wednesday Football"),
                h1(class = "hero-title", "Borulanta"),
                p(
                    class = "hero-copy",
                    paste(
                        "Where we left off, who is next, and what you owe.",
                        "The season picker applies to matches, attendance and",
                        "player effects."
                    )
                ),
                # Kept out of .hero-copy, which the phone layout hides: a
                # countdown is read on a phone more than anywhere else, so how
                # old it is has to survive that.
                freshness_note(app_data$matches, app_data$league_table)
            ),
            div(
                class = "season-shell",
                season_picker(app_data)
            )
        ),
        data_health_banner(app_data$problems),
        div(
            class = "nav-pill-shell",
            navset_pill(
                id = "app_tabs",
                # First, so it is where the app opens. Like the Fees tab, and
                # for the same reason, it ignores the season picker: it answers
                # what just happened and what is next, and a question in the
                # present tense does not take a season.
                nav_panel(
                    "Home",
                    icon = icon("house"),
                    div(
                        class = "home-grid",
                        uiOutput("last_match_card"),
                        uiOutput("next_match_card")
                    ),
                    uiOutput("league_table_card"),
                    div(
                        class = "fees-grid",
                        fee_panel_ui("home_fee_player", "home_fees_owed"),
                        payment_panel_ui()
                    )
                ),
                nav_panel(
                    "Fees",
                    icon = icon("dollar-sign"),
                    div(
                        class = "fees-grid",
                        fee_panel_ui("fee_player", "fees_owed"),
                        payment_panel_ui()
                    ),
                    fee_history_panel
                ),
                nav_panel(
                    "Matches",
                    icon = icon("futbol"),
                    conditionalPanel(
                        "!output.season_has_matches",
                        empty_season_card("No matches in the selected seasons yet.")
                    ),
                    conditionalPanel(
                        "output.season_has_matches",
                        season_form_card,
                        table_card_ui(
                            "Matches",
                            "Every result",
                            "Newest first, for the seasons you have ticked.",
                            "matches"
                        ),
                        scorer_card,
                        match_detail_card
                    )
                ),
                nav_panel(
                    "Attendance",
                    icon = icon("users"),
                    conditionalPanel(
                        "!output.season_has_matches",
                        empty_season_card("No matches in the selected seasons yet.")
                    ),
                    conditionalPanel(
                        "output.season_has_matches",
                        table_card_ui(
                            "Attendance",
                            "Who turns up",
                            "Share of matches played, and how those matches went.",
                            "attendance_list"
                        )
                    )
                ),
                nav_panel(
                    "Player effects",
                    icon = icon("chart-line"),
                    conditionalPanel(
                        "!output.season_has_regression",
                        empty_season_card(
                            paste(
                                "Not enough matches in the selected seasons to",
                                "estimate player effects. Tick more seasons."
                            )
                        )
                    ),
                    conditionalPanel(
                    "output.season_has_regression",
                    layout_columns(
                        col_widths = c(6, 6),
                        plot_card_ui(
                            "Attack and defence",
                            "Top right is more goals scored and fewer conceded, holding the rest of the lineup fixed.",
                            "attack_defence_plot",
                            "680px"
                        ),
                        card(
                            class = "table-card",
                            card_header(
                                div(class = "section-tag", "Visualise"),
                                h2(class = "table-title", "Coefficient plot"),
                                p(
                                    class = "table-subtitle",
                                    "Dots are estimates and lines are 95% confidence intervals."
                                )
                            ),
                            card_body(
                                selectInput(
                                    "regression_outcome",
                                    "Outcome",
                                    choices = c(
                                        "Points" = "points",
                                        "Goals scored" = "goals_scored",
                                        "Goals conceded" = "goals_conceded",
                                        "Goal difference" = "goal_difference"
                                    )
                                ),
                                plotOutput("coefficient_plot", height = "560px")
                            )
                        )
                    ),
                    table_card_ui(
                        "Player effects",
                        "Player by player",
                        "Coefficient on each outcome, with its p-value in brackets.",
                        "player_regressions"
                    ),
                    regression_explanation
                    )
                )
            )
        )
    )
)

match_table <- function(matches) {
    matches <- matches %>%
        mutate(
            outcome = case_when(
                goals_for > goals_against ~ "<span class='result-badge win'>WIN</span>",
                goals_for == goals_against ~ "<span class='result-badge draw'>DRAW</span>",
                TRUE ~ "<span class='result-badge loss'>LOSS</span>"
            )
        ) %>%
        transmute(
            Date = format(date, "%d %b %Y"),
            Opponent = coalesce(opponent, "\u2014"),
            Result = result,
            Outcome = outcome
        )

    datatable(
        matches,
        rownames = FALSE,
        escape = FALSE, # required to render the badge span
        class = "nowrap",
        options = list(
            dom = "t",
            pageLength = nrow(matches),
            ordering = FALSE,
            autoWidth = TRUE,
            scrollX = TRUE
        )
    )
}

attendance_table <- function(attendance_list) {
    datatable(
        attendance_list,
        rownames = FALSE,
        class = "nowrap",
        options = list(
            dom = "tip",
            pageLength = 15,
            order = list(list(1, "desc")),
            autoWidth = TRUE,
            scrollX = TRUE,
            rowCallback = JS(
                "function(row, data, index) {",
                "  if (index < 3) { $(row).addClass('rank-top3'); }",
                "}"
            )
        )
    )
}

# The two Fee check cards, one per tab. Named once so the server can keep them
# in step without either tab knowing the other exists.
FEE_PLAYER_INPUTS <- c("fee_player", "home_fee_player")

# Server logic ----
server <- function(input, output, session) {
    # Everything downstream reads from this one filtered view of the data, so a
    # statistic is scoped to a season purely by what the picker is set to.
    # No tick boxes checked is a legitimate state, not an error: every filter
    # below simply yields nothing, and the tabs show their empty states.
    selected_seasons <- reactive(input$seasons)

    scoped <- reactive({
        list(
            matches = filter_season(app_data$matches, selected_seasons()),
            attendance = filter_season(app_data$attendance, selected_seasons()),
            events = filter_season(app_data$events, selected_seasons()),
            payments = filter_season(app_data$payments, selected_seasons()),
            charges = filter_season(app_data$charges, selected_seasons())
        )
    })

    has_matches <- reactive(nrow(scoped()$matches) > 0)

    # These stay callable on an empty season: they return zero-row results
    # rather than aborting, so has_regression() below is always answerable.
    season_form <- reactive({
        season_form_data(scoped()$attendance, scoped()$matches)
    })

    regression_results <- reactive({
        player_regression_results(
            scoped()$attendance, scoped()$matches, app_data$opponent_strength
        )
    })

    has_regression <- reactive(nrow(regression_results()) > 0)

    # A balance is a running total, not a per-season statistic: people settle up
    # when they settle up, not season by season, so splitting payments by date
    # would show a debt in one season and the mirror-image credit in the next.
    # The Fees tab therefore always covers all time, and the season picker
    # scopes the results and statistics tabs only.
    fee_players <- reactive({
        app_data$rosters %>%
            filter(active) %>%
            pull(player) %>%
            unique() %>%
            sort()
    })

    # The same card is on Home and on Fees, so there are two pickers for one
    # question. This is the answer, and each picker follows it: whichever one
    # you touch, the other catches up, so switching tabs never shows you
    # somebody else's balance.
    selected_fee_player <- reactiveVal(NULL)

    observeEvent(fee_players(), {
        players <- fee_players()
        current <- selected_fee_player()
        chosen <- if (isTruthy(current) && current %in% players) current else players[[1]]

        selected_fee_player(chosen)
        for (id in FEE_PLAYER_INPUTS) {
            updateSelectInput(session, id, choices = players, selected = chosen)
        }
    }, ignoreNULL = FALSE)

    # Guarded on equality in both directions: without it each picker would
    # answer the other's update with one of its own, indefinitely.
    for (id in FEE_PLAYER_INPUTS) {
        local({
            this <- id
            others <- setdiff(FEE_PLAYER_INPUTS, this)
            observeEvent(input[[this]], {
                if (identical(input[[this]], selected_fee_player())) {
                    return()
                }
                selected_fee_player(input[[this]])
                for (other in others) {
                    updateSelectInput(session, other, selected = input[[this]])
                }
            })
        })
    }

    fee_overview <- reactive({
        req(selected_fee_player())
        player_fee_overview(
            selected_fee_player(), app_data$charges, app_data$payments, app_data$seasons
        )
    })

    # Keyed on the fixture list rather than has_matches(), which stays TRUE
    # across two seasons that both have matches and so would not re-fire.
    observeEvent(season_form(), {
        if (!has_matches()) {
            updateSelectInput(session, "selected_match", choices = character(0))
            return()
        }

        match_choices <- season_form() %>%
            arrange(desc(date)) %>%
            transmute(
                value = as.character(date),
                label = paste(
                    format(date, "%d %b %Y"),
                    "\u2014",
                    if_else(is.na(opponent), result, paste(result, "v", opponent))
                )
            )

        updateSelectInput(
            session,
            "selected_match",
            choices = setNames(match_choices$value, match_choices$label),
            selected = match_choices$value[[1]]
        )
    })

    selected_match_details <- reactive({
        req(has_matches(), input$selected_match)
        match_detail_data(season_form(), scoped()$attendance, input$selected_match)
    })

    # Fees ----
    output$fee_overview_summary <- renderUI({
        fee_overview_summary(fee_overview())
    })

    output$match_charges <- renderDT({
        match_charge_table(fee_overview()$match_charges)
    })

    output$payment_history <- renderDT({
        payment_history_table(fee_overview()$payment_history)
    })

    fees_owed_ui <- reactive({
        balance <- fee_overview()$balance
        tags$div(
            class = paste("fee-amount", if (balance > 0) "is-owed" else ""),
            format_fee_amount(balance)
        )
    })

    output$fees_owed <- renderUI(fees_owed_ui())
    output$home_fees_owed <- renderUI(fees_owed_ui())

    # Home ----
    #
    # Rendered rather than built once at startup because both cards say
    # something about today — "Tonight", "In 5 days" — and a process that has
    # been up for a week would otherwise still be counting down from the day it
    # started.
    output$last_match_card <- renderUI({
        last_match_card_ui(
            last_match_summary(app_data$matches, app_data$attendance, app_data$events),
            app_data$seasons
        )
    })

    output$next_match_card <- renderUI({
        next_match_card_ui(
            next_fixture(app_data$fixtures, app_data$matches),
            have_fixtures = nrow(app_data$fixtures) > 0,
            scraped_on = app_data$league_table$scraped_on
        )
    })

    output$league_table_card <- renderUI(league_table_ui(app_data$league_table))

    output$league_table <- renderDT({
        req(nrow(app_data$league_table) > 0)
        league_table_table(app_data$league_table)
    })

    # Matches ----
    output$matches <- renderDT({
        match_table(scoped()$matches)
    })

    output$season_form_plot <- renderPlot({
        season_form_plot(season_form(), input$form_metric)
    })

    output$match_summary <- renderUI({
        match_summary_ui(selected_match_details()$match)
    })

    output$match_lineup <- renderDT({
        match_lineup_table(selected_match_details()$lineup)
    })

    output$scorers <- renderDT({
        req(has_matches())
        scorer_display_table(
            scorer_table(scoped()$events, scoped()$attendance, scoped()$matches)
        )
    })

    output$scorer_coverage <- renderUI({
        scorer_coverage_ui(event_coverage(scoped()$matches))
    })

    # Attendance ----
    output$attendance_list <- renderDT({
        req(has_matches())
        attendance_table(create_attendance_list(scoped()$attendance, scoped()$matches))
    })

    # Player effects ----
    output$player_regressions <- renderDT({
        req(has_regression())
        regression_table(player_regression_table(regression_results()))
    })

    output$attack_defence_plot <- renderPlot({
        req(has_regression())
        attack_defence_plot(regression_results())
    })

    output$coefficient_plot <- renderPlot({
        req(has_regression())
        coefficient_plot(regression_results(), input$regression_outcome)
    })

    output$regression_explanation <- renderUI({
        req(has_regression())
        regression_explanation_ui(
            regression_results(), MIN_REGRESSION_APPEARANCES,
            opponent_coverage(scoped()$matches, app_data$opponent_strength)
        )
    })

    # Empty states ----
    output$season_has_matches <- reactive(has_matches())
    output$season_has_regression <- reactive(has_regression())
    outputOptions(output, "season_has_matches", suspendWhenHidden = FALSE)
    outputOptions(output, "season_has_regression", suspendWhenHidden = FALSE)
}

# Run app ----
shinyApp(ui, server)
