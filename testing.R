library(shiny)
library(bslib)
library(maps)
library(mapproj)
library(tidyverse)
library(data.table)
library(DT)
library(glue)

source("calculate_fees.R")

player <- 'Oleg'
# Read data
matches <- read_csv("data/matches.csv")
players <- read_csv("data/players.csv")
attendance <- read_csv("data/attendance.csv")
payments <- read_csv("data/payments.csv")

total_fees <- 0
# How much does the player owe?
for (match in unique(matches$date)) {
  match_players <- attendance[attendance$date == match,]$player
  attended <- player %in% match_players
  # Is the player a guest?
  if (!players[players$player == player,]$regular) {
    # Did the player attend?
    if (attended) {
      total_fees <- total_fees + guest_fee
    } else {
      total_fees <- total_fees + 0
    }
  } else {
    n_guests <- sum(match_players %in% players[players$regular == FALSE,]$player)
    fee_remainder <- match_fee - (n_guests * guest_fee)
    # Were there enough players?
    if (length(match_players) >= min_players) {
      # Did the player attend?
      if (attended) {
        regular_fee <- fee_remainder /
          (length(match_players) - n_guests)
        total_fees <- total_fees + regular_fee
      } else {
        total_fees <- total_fees + 0
      }
    } else {
      regular_fee <- fee_remainder / length(players[players$regular == TRUE,]$player)
      total_fees <- total_fees + regular_fee
    }
  }
}
# Subtract any payments made
payments_made <- sum(as.numeric(payments[payments$player == player,]$amount))
total_fees <- total_fees - payments_made
return(total_fees)
