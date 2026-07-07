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
