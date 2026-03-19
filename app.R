library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(glue)

players <- read_csv("data/players.csv")

app_theme <- bs_theme(
    version = 5,
    bg = "#0d1f1b",
    fg = "#f7f4ec",
    primary = "#a7f04f",
    secondary = "#ffd166",
    success = "#8bd450",
    info = "#7bdff2",
    warning = "#ffd166",
    danger = "#ff7b72",
    base_font = font_collection("Avenir Next", "Trebuchet MS", "sans-serif"),
    heading_font = font_collection(
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
    datatable(
        matches,
        rownames = FALSE,
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
            scrollX = TRUE
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
        tags$div(
            class = "fee-amount",
            glue(
                "£{calculate_fees(input$fee_player, matches, attendance, payments, players)}"
            )
        )
    })
}

# Run app ----
shinyApp(ui, server)
