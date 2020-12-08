library(ggplot2)
library(readr)
library(dplyr)
library(knitr)

#-------------------------------------------------------------------------------------------------------------------------------------
# importing game data from 19-20 season
results_1920 <- read.csv("data/19_20_PL_results.csv", nrows = 380)
results_1920 <- results_1920 %>% select(Round.Number, Date, Home.Team, Away.Team, Home, Away)
#results_1920$ID <- seq.int(nrow(results_1920)) # giving each game an ID


# importing table format
table_1920 <- read.csv("data/19_20_PL_table.csv")
#-------------------------------------------------------------------------------------------------------------------------------------
# function that calculates league table up to right before the inputted match
league_table <- function(game_number) {
  games <- seq(1, game_number - 1)
  
  for (j in games) {
    table_1920[ table_1920$Team == as.character(results_1920[j,]$Away.Team),]$GP <- table_1920[ table_1920$Team == as.character(results_1920[j,]$Home.Team),]$GP + 1
    table_1920[ table_1920$Team == as.character(results_1920[j,]$Home.Team),]$GP <- table_1920[ table_1920$Team == as.character(results_1920[j,]$Home.Team),]$GP + 1
    
    table_1920[ table_1920$Team == as.character(results_1920[j,]$Home.Team),]$GF <- table_1920[ table_1920$Team == as.character(results_1920[j,]$Home.Team),]$GF + results_1920[j,]$Home
    table_1920[ table_1920$Team == as.character(results_1920[j,]$Home.Team),]$GA <- table_1920[ table_1920$Team == as.character(results_1920[j,]$Home.Team),]$GA + results_1920[j,]$Away
    
    table_1920[ table_1920$Team == as.character(results_1920[j,]$Away.Team),]$GF <- table_1920[ table_1920$Team == as.character(results_1920[j,]$Away.Team),]$GF + results_1920[j,]$Away
    table_1920[ table_1920$Team == as.character(results_1920[j,]$Away.Team),]$GA <- table_1920[ table_1920$Team == as.character(results_1920[j,]$Away.Team),]$GA + results_1920[j,]$Home
    
    if (results_1920[j,]$Home > results_1920[j,]$Away) { # home win
      table_1920[ table_1920$Team == as.character(results_1920[j,]$Home.Team),]$P <- table_1920[ table_1920$Team == as.character(results_1920[j,]$Home.Team),]$P + 3
    }
    
    if (results_1920[j,]$Home < results_1920[j,]$Away) { # away win
      table_1920[ table_1920$Team == as.character(results_1920[j,]$Away.Team),]$P <- table_1920[ table_1920$Team == as.character(results_1920[j,]$Away.Team),]$P + 3
    }
    
    if (results_1920[j,]$Home == results_1920[j,]$Away) { # tie
      table_1920[ table_1920$Team == as.character(results_1920[j,]$Home.Team),]$P <- table_1920[ table_1920$Team == as.character(results_1920[j,]$Home.Team),]$P + 1
      table_1920[ table_1920$Team == as.character(results_1920[j,]$Away.Team),]$P <- table_1920[ table_1920$Team == as.character(results_1920[j,]$Away.Team),]$P + 1
    }
  }
  
  table_1920$GP <- NULL
  table_1920 <- table_1920 %>% mutate(GD = GF - GA)
  table_1920 <- table_1920[order(-table_1920$P, -table_1920$GD),]
  rownames(table_1920) <- 1:nrow(table_1920)
  
  table_1920
}

starting_table <- league_table(368)

update_table <- function(team, result, goals_for, goals_against) {
  starting_table[ starting_table$Team == team,]$GF <- starting_table[ starting_table$Team == team,]$GF + goals_for
  starting_table[ starting_table$Team == team,]$GA <- starting_table[ starting_table$Team == team,]$GA + goals_against
  
  if (result == "win") {
    starting_table[ starting_table$Team == team,]$P <- starting_table[ starting_table$Team == team,]$P + 3
    
  }
  
  if (result == "tie") {
    starting_table[ starting_table$Team == team,]$P <- starting_table[ starting_table$Team == team,]$P + 1
  }
  
  starting_table <- starting_table %>% mutate(GD = GF - GA)
  starting_table <- starting_table[order(-starting_table$P, -starting_table$GD),]
  rownames(starting_table) <- 1:nrow(starting_table)
  
  starting_table
}

starting_table <- update_table("Aston Villa", "win", 1, 0)
starting_table <- update_table("Arsenal", "loss", 0, 1)


get_position <- function(team) {
  as.numeric(rownames(starting_table[ starting_table$Team == team,]))
}

get_position("Aston Villa")

