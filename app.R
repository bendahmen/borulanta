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
        geom_text(aes(label = player), nudge_y = 0.15, size = 3, check_overlap = TRUE) +
        scale_color_gradient2(
            low = "#AB4400",
            mid = "#16241f",
            high = "#006D1E",
            midpoint = 0,
            name = "Goal difference"
        ) +
        scale_size_area(max_size = 14, name = "Appearances") +
        labs(
            x = "Goals-scored coefficient (higher is better)",
            y = "Defensive coefficient: minus goals conceded (higher is better)"
        ) +
        coord_equal() +
        theme_minimal(base_size = 12) +
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
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
                "Each result is a match-level regression on indicators for every player",
                "present. The coefficient therefore describes the player's association",
                "with that outcome, conditional on the rest of the lineup."
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
                    )
                ),
                nav_panel(
                    "Matches",
                    icon = icon("futbol"),
                    table_card_ui(
                        "Matches",
                        "Recent results for the running season.",
                        "matches"
                    )
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
                            "560px"
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
server <- function(input, output) {
    matches <- read_csv("data/matches.csv")
    attendance <- read_csv("data/attendance.csv")
    payments <- read_csv("data/payments.csv")

    source("calculate_fees.R")
    source("player_contributions.R")

    avg_points_by_player <- avg_by_player(attendance, matches, players)
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

    output$matches <- renderDT({
        match_table(matches)
    })

    output$attendance_list <- renderDT({
        attendance_table(attendance_list)
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
        balance <- calculate_fees(input$fee_player, matches, attendance, payments, players)
        tags$div(
            class = paste("fee-amount", if (balance > 0) "is-owed" else ""),
            glue("£{balance}")
        )
    })
}

# Run app ----
shinyApp(ui, server)
