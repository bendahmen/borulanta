library(shiny)
library(bslib)
library(maps)
library(mapproj)
library(tidyverse)
library(data.table)
library(DT)
library(glue)

players <- read_csv("data/players.csv")

# User interface ----
ui <- page_fluid(
    title = "Borulanta",
    navset_pill(
        nav_panel(
            "Fees", icon = icon("dollar-sign"),
            selectInput(
                "fee_player",
                "Who are you?",
                as.list(players$player)
            ),
            value_box(
                "You Owe",
                uiOutput("fees_owed"),
                showcase = icon("dollar-bill")
            ),
            card(
                div(
                    class = 'text-center',
                    tags$a(
                        href = "https://monzo.me/benjamindahmen8?h=Njfjz9",
                        target = "_blank",
                        tags$button("Pay me", class = "btn btn-default action-button")
                    ) 
                ),
                card_footer('Account Number: 94456363, Sort Code: 04-00-03, Name: Benjamin Dahmen')
            )
        ),
        nav_panel(
            "Matches", icon = icon("futbol"),
            dataTableOutput("matches")
        ),
        nav_panel(
            "Attendance", icon = icon("users"),
            dataTableOutput("attendance_list")
        )
    )
)


# Server logic ----
server <- function(input, output) {
    
    # Read data
    matches <- read_csv("data/matches.csv")
    attendance <- read_csv("data/attendance.csv")
    payments <- read_csv("data/payments.csv")
    
    source("calculate_fees.R")
    source("player_contributions.R")
    
    avg_points_by_player <- avg_by_player(attendance, matches, players)
    
    attendance_list <- create_attendance_list(matches, attendance, avg_points_by_player)
    output$matches <- 
        renderDT({datatable(matches)})
    output$attendance_list <- 
        renderDT({datatable(attendance_list)})
    output$fees_owed <- renderText({
        glue("£{calculate_fees(input$fee_player, matches, attendance, payments, players)}")
    })
}

# Run app ----
shinyApp(ui, server)
