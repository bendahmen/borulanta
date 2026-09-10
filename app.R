library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(glue)

# The data layer first: R/fees.R defines parse_match_date(), which R/seasons.R
# needs at load time. Then the presentation layer, which reads constants from
# it — R/tables.R wants REGRESSION_TABLE_COLUMNS out of R/analysis.R.
#
# R/cards.R builds some of its cards eagerly rather than as functions, so it is
# sourced after the helpers they call and before the ui below that holds them.
# None of it touches app_data, which is why the load can come after.
source("R/fees.R")
source("R/seasons.R")
source("R/analysis.R")
source("R/data.R")
source("R/format.R")
source("R/plots.R")
source("R/tables.R")
source("R/cards.R")

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
                        "The season picker applies to matches, players and",
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
                season_picker(app_data),
                # Dimmed and captioned, not hidden or disabled: the tick boxes
                # stay usable, so the scope can be set here and carried to a
                # tab that reads it. Under the pills rather than over them,
                # where the heading already is.
                conditionalPanel(
                    "!output.season_picker_applies",
                    div(class = "season-inert", "Not used on this tab")
                )
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
                    # The run-in belongs with these two and not on Matches: it
                    # describes the current league season, which is not a fee
                    # season, and it used to have to say so in its own subtitle
                    # to excuse ignoring the picker. Here nothing ignores the
                    # picker, so there is nothing to excuse.
                    run_in_card,
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
                    everyone_panel,
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
                        scorer_card,
                        opponent_record_card,
                        every_result_card,
                        match_detail_card
                    )
                ),
                # Attendance used to be a tab of its own holding one card. It
                # is the roster-wide version of the statistic the page below it
                # shows one player at a time, so it is the way in to that page
                # rather than a separate destination: pick a row, read the
                # player. One fewer tab, and the picker gets something to pick
                # from.
                nav_panel(
                    "Players",
                    icon = icon("users"),
                    conditionalPanel(
                        "!output.season_has_matches",
                        empty_season_card("No matches in the selected seasons yet.")
                    ),
                    conditionalPanel("output.season_has_matches", player_page)
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
                    estimator_control,
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
                    card(
                        class = "table-card",
                        card_header(
                            div(class = "section-tag", "Player effects"),
                            h2(class = "table-title", "Player by player"),
                            uiOutput("regression_table_note")
                        ),
                        card_body(dataTableOutput("player_regressions"))
                    ),
                    regression_explanation
                    )
                )
            )
        )
    )
)

# The two Fee check cards, one per tab. Named once so the server can keep them
# in step without either tab knowing the other exists.
FEE_PLAYER_INPUTS <- c("fee_player", "home_fee_player")

# The tabs the season picker actually scopes, by their nav_panel titles. Home
# and Fees are the two it does not: both are all of time on purpose — what just
# happened and what is next do not take a season, and a balance is a running
# total that would show a debt in one season and its mirror image in the next.
SEASON_SCOPED_TABS <- c("Matches", "Players", "Player effects")

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

    estimator <- reactive(input$regression_estimator %||% "ols")

    regression_results <- reactive({
        player_regression_results(
            scoped()$attendance, scoped()$matches, app_data$opponent_strength,
            estimator = estimator()
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

    # The match the detail card describes, picked by clicking a row of Every
    # result rather than from a dropdown that listed the same matches again.
    #
    # DT reports the index into the data it was given, not the position on
    # screen, so this indexes the same frame the table was built from. Falling
    # back to the first row rather than req()-ing a selection keeps the card
    # filled on arrival and through a season change, which clears the
    # selection: an empty card under a full table reads as a fault.
    selected_match_date <- reactive({
        req(has_matches())
        matches <- scoped()$matches
        row <- input$matches_rows_selected
        if (length(row) != 1 || row > nrow(matches)) row <- 1L
        as.character(matches$date[[row]])
    })

    selected_match_details <- reactive({
        match_detail_data(season_form(), scoped()$attendance, selected_match_date())
    })

    # Fees ----
    output$fee_overview_summary <- renderUI({
        fee_overview_summary(fee_overview())
    })

    # Like every other fee output, all time rather than the picked seasons: a
    # balance is a running total and scoping it by date would show a debt in one
    # season and its mirror image in the next.
    all_balances <- reactive({
        all_player_balances(app_data$charges, app_data$payments)
    })

    output$balance_totals <- renderUI(balance_totals_ui(balance_totals(all_balances())))

    output$all_balances <- renderDT(balance_table(all_balances()))

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

    output$opponent_record <- renderUI({
        record <- opponent_record(scoped()$matches)
        if (nrow(record) == 0) {
            return(div(
                class = "empty-state",
                paste(
                    "No match in the selected seasons has its opponent on file.",
                    "They start with the sync."
                )
            ))
        }
        dataTableOutput("opponent_record_table")
    })

    output$opponent_record_table <- renderDT({
        opponent_record_table(opponent_record(scoped()$matches))
    })

    output$run_in <- renderDT({
        run_in_table(run_in(app_data$fixtures, app_data$matches, app_data$league_table))
    })

    output$scorer_coverage <- renderUI({
        scorer_coverage_ui(event_coverage(scoped()$matches))
    })

    # Players ----
    #
    # The roster table is the picker, and the page under it is one row of that
    # table opened up. It offers whoever turned out in the ticked seasons,
    # which is deliberately not the fee picker's list: that one offers the
    # active roster and ignores seasons because a balance is a running total,
    # while this is a page about matches that happened. A player who has left
    # still has a page; a new signing who has not played yet does not.
    roster_list <- reactive({
        req(has_matches())
        create_attendance_list(scoped()$attendance, scoped()$matches)
    })

    # Seeded from the fee picker where that person turned out in the ticked
    # seasons, so arriving here usually lands on you. Isolated because it is an
    # opening position rather than a link: changing the fee player on the other
    # tab should not reach over and move the row you picked here. Where they
    # did not turn out — the two pickers are allowed to disagree about who
    # exists — it opens on whoever turns up most, which is the top of the table.
    output$attendance_list <- renderDT({
        roster <- roster_list()
        seed <- match(isolate(selected_fee_player()), roster$player)
        attendance_table(roster, selected_row = coalesce(seed, 1L))
    })

    # As with the match picker, DT reports the index into the data rather than
    # the position on screen, so re-sorting the table by any column leaves this
    # pointing at the same person. No selection falls back to the top of the
    # table rather than emptying the page below it.
    profile_player <- reactive({
        roster <- roster_list()
        req(nrow(roster) > 0)
        row <- input$attendance_list_rows_selected
        if (length(row) != 1 || row > nrow(roster)) row <- 1L
        roster$player[[row]]
    })

    profile <- reactive({
        player_profile(
            profile_player(), scoped()$attendance, scoped()$matches, scoped()$events
        )
    })

    output$profile_player_note <- renderUI({
        p(
            class = "table-subtitle",
            paste0(profile_player(), ". Pick another row above to change who this is about.")
        )
    })

    output$player_headline <- renderUI(player_headline_ui(profile()))
    output$with_without <- renderDT(with_without_table(profile()))
    output$differential_note <- renderUI(differential_note(profile()))
    output$appearance_timeline <- renderPlot(appearance_timeline_plot(profile()))

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

    output$regression_table_note <- renderUI({
        p(class = "table-subtitle", estimate_label(estimator()))
    })

    output$regression_explanation <- renderUI({
        req(has_regression())
        regression_explanation_ui(
            regression_results(), MIN_REGRESSION_APPEARANCES,
            opponent_coverage(scoped()$matches, app_data$opponent_strength),
            estimator()
        )
    })

    # Empty states ----
    output$season_has_matches <- reactive(has_matches())
    output$season_has_regression <- reactive(has_regression())
    outputOptions(output, "season_has_matches", suspendWhenHidden = FALSE)
    outputOptions(output, "season_has_regression", suspendWhenHidden = FALSE)

    # Where the season picker applies ----
    #
    # It sits in the hero on every tab and does nothing on two of them. The
    # strapline says so, and the phone layout hides the strapline — so on the
    # screen where the picker is hardest to ignore it is also unexplained, and
    # it is the tab the app opens on. Rather than move it, say so: the shell
    # goes quiet and captions itself on the tabs that read all of time anyway.
    # Named rather than written straight into output, so it can be read on its
    # own: an anonymous reactive assigned to output is reachable from the
    # conditionalPanel and from nowhere else.
    season_picker_applies <- reactive({
        (input$app_tabs %||% "Home") %in% SEASON_SCOPED_TABS
    })
    output$season_picker_applies <- season_picker_applies
    outputOptions(output, "season_picker_applies", suspendWhenHidden = FALSE)
}

# Run app ----
shinyApp(ui, server)
