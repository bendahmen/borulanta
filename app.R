library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(glue)

players <- read_csv("data/players.csv")

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

fee_panel <- card(
    class = "panel-card",
    card_body(
        div(class = "section-tag", "Fee Check"),
        div(
            class = "selection-shell",
            selectInput(
                "fee_player",
                "Choose your name",
                choices = as.list(players$player)
            )
        ),
        div(class = "fee-label", "Current balance"),
        uiOutput("fees_owed"),
        div(
            class = "fee-footnote",
            "Calculated from attendance, match fee rules, core-player status, and recorded payments."
        )
    )
)

payment_panel <- card(
    class = "payment-card",
    card_body(
        div(class = "section-tag", "Settle Up"),
        p(
            class = "payment-copy",
            "Use the payment link below, then the balance will drop once the transfer is recorded in the sheet."
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

format_fee_amount <- function(amount) {
    sign <- if (amount < 0) "-" else ""
    paste0(sign, "£", formatC(abs(amount), format = "f", digits = 2))
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
                "Match charges follow the squad-size and core-player rules in force on each date."
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
                "Individual shares are shown to the nearest penny; the summary keeps the exact split amounts."
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

table_card_ui <- function(title, subtitle, output_id) {
    card(
        class = "table-card",
        card_header(
            div(class = "section-tag", title),
            h2(class = "table-title", title),
            p(class = "table-subtitle", subtitle)
        ),
        card_body(
            dataTableOutput(output_id)
        )
    )
}

regression_table <- function(regression_results) {
    estimate_columns <- c(
        "Points (beta)",
        "Goals scored (beta)",
        "Goals conceded (beta)",
        "Goal difference (beta)"
    )
    p_value_columns <- c(
        "Points (p)",
        "Goals scored (p)",
        "Goals conceded (p)",
        "Goal difference (p)"
    )

    datatable(
        regression_results,
        rownames = FALSE,
        class = "nowrap",
        options = list(
            dom = "tip",
            pageLength = 25,
            order = list(list(1, "desc")),
            autoWidth = TRUE,
            scrollX = TRUE
        )
    ) %>%
        formatRound(columns = estimate_columns, digits = 2) %>%
        formatRound(columns = p_value_columns, digits = 3)
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
            x = "Goals-scored coefficient (higher is better)",
            y = "Defensive coefficient: minus goals conceded (higher is better)"
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

regression_explanation <- card(
    class = "table-card",
    card_header(
        div(class = "section-tag", "Method"),
        h2(class = "table-title", "How to read the player effects")
    ),
    card_body(
        p(
            paste(
                "Each result is a match-level regression on indicators for the 17 included",
                "players. Yelong, Langkun, and Benoit are excluded. The coefficient",
                "therefore describes a player's association with that outcome, conditional",
                "on the other included players."
            )
        ),
        p(
            paste(
                "Points are 3 for a win, 1 for a draw, and 0 for a loss; goals",
                "scored and conceded use the first and second number in the recorded",
                "score, and goal difference is scored minus conceded. The model does",
                "not include a time trend. The p-values are conventional OLS p-values.",
                "These are descriptive lineup-adjusted associations, not causal",
                "measures of individual quality."
            )
        )
    )
)

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
            "Each point is the trailing five-match average; early matches use all results so far."
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
                paste(format(selected_match$date, "%d %B %Y"), "—", outcome)
            )
        ),
        div(
            class = "match-detail-stat",
            div(class = "match-detail-label", "Points"),
            div(class = "match-detail-value", selected_match$points),
            div(class = "match-detail-note", "3 for a win, 1 for a draw")
        ),
        div(
            class = "match-detail-stat",
            div(class = "match-detail-label", "Squad"),
            div(class = "match-detail-value", selected_match$squad_size),
            div(class = "match-detail-note", "Players recorded as present")
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

match_detail_card <- card(
    class = "table-card",
    card_header(
        div(class = "section-tag", "Match detail"),
        h2(class = "table-title", "Lineup and season context"),
        p(
            class = "table-subtitle",
            "Choose a result to see the players who were there and their season attendance."
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
                div(class = "hero-kicker", "Wednesday Football"),
                h1(class = "hero-title", "Borulanta"),
                p(
                    class = "hero-copy",
                    "Track what you owe, review recent match results, and see who keeps the squad going each week."
                ),
                div(
                    class = "hero-meta",
                    icon("futbol"),
                    span("Fees, attendance, and results in one place")
                )
            )
        ),
        div(
            class = "nav-pill-shell",
            navset_pill(
                id = "app_tabs",
                nav_panel(
                    "Fees",
                    icon = icon("dollar-sign"),
                    div(
                        class = "fees-grid",
                        fee_panel,
                        payment_panel
                    ),
                    fee_history_panel
                ),
                nav_panel(
                    "Matches",
                    icon = icon("futbol"),
                    season_form_card,
                    table_card_ui(
                        "Matches",
                        "Recent results for the running season.",
                        "matches"
                    ),
                    match_detail_card
                ),
                nav_panel(
                    "Attendance",
                    icon = icon("users"),
                    table_card_ui(
                        "Attendance",
                        "Participation rate and on-pitch averages by player.",
                        "attendance_list"
                    )
                ),
                nav_panel(
                    "Player effects",
                    icon = icon("chart-line"),
                    layout_columns(
                        col_widths = c(6, 6),
                        plot_card_ui(
                            "Attack and defence",
                            "Top-right players are associated with more scoring and fewer goals conceded.",
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
                        "Lineup-adjusted associations with match outcomes.",
                        "player_regressions"
                    ),
                    regression_explanation
                )
            )
        )
    )
)

match_table <- function(matches) {
    matches <- matches %>%
        mutate(
            scored = as.integer(str_extract(result, "^\\d+")),
            conceded = as.integer(str_extract(result, "\\d+$")),
            outcome = case_when(
                scored > conceded ~ "<span class='result-badge win'>WIN</span>",
                scored == conceded ~ "<span class='result-badge draw'>DRAW</span>",
                TRUE ~ "<span class='result-badge loss'>LOSS</span>"
            )
        ) %>%
        select(date, result, outcome)

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

# Server logic ----
server <- function(input, output, session) {
    matches <- read_csv("data/matches.csv")
    attendance <- read_csv("data/attendance.csv")
    payments <- read_csv("data/payments.csv")

    source("calculate_fees.R")
    source("player_contributions.R")

    avg_points_by_player <- avg_by_player(attendance, matches, players)
    season_form <- season_form_data(attendance, matches)
    regression_results <- player_regression_results(
        attendance,
        matches,
        players
    )
    player_regressions <- player_regression_table(regression_results)

    attendance_list <- create_attendance_list(
        matches,
        attendance,
        avg_points_by_player
    )

    fee_overview <- reactive({
        req(input$fee_player)
        player_fee_overview(
            input$fee_player,
            matches,
            attendance,
            payments,
            players
        )
    })

    match_choices <- season_form %>%
        arrange(desc(date)) %>%
        transmute(
            value = as.character(date),
            label = paste(format(date, "%d %b %Y"), "—", result)
        )
    updateSelectInput(
        session,
        "selected_match",
        choices = setNames(match_choices$value, match_choices$label),
        selected = match_choices$value[[1]]
    )

    selected_match_details <- reactive({
        req(input$selected_match)
        match_detail_data(season_form, attendance, input$selected_match)
    })

    output$matches <- renderDT({
        match_table(matches)
    })

    output$season_form_plot <- renderPlot({
        season_form_plot(season_form, input$form_metric)
    })

    output$match_summary <- renderUI({
        match_summary_ui(selected_match_details()$match)
    })

    output$match_lineup <- renderDT({
        match_lineup_table(selected_match_details()$lineup)
    })

    output$attendance_list <- renderDT({
        attendance_table(attendance_list)
    })

    output$fee_overview_summary <- renderUI({
        fee_overview_summary(fee_overview())
    })

    output$match_charges <- renderDT({
        match_charge_table(fee_overview()$match_charges)
    })

    output$payment_history <- renderDT({
        payment_history_table(fee_overview()$payment_history)
    })

    output$player_regressions <- renderDT({
        regression_table(player_regressions)
    })

    output$attack_defence_plot <- renderPlot({
        attack_defence_plot(regression_results)
    })

    output$coefficient_plot <- renderPlot({
        coefficient_plot(regression_results, input$regression_outcome)
    })

    output$fees_owed <- renderUI({
        balance <- fee_overview()$balance
        tags$div(
            class = paste("fee-amount", if (balance > 0) "is-owed" else ""),
            format_fee_amount(balance)
        )
    })
}

# Run app ----
shinyApp(ui, server)
