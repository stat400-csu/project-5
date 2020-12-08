library(ggplot2)
library(readr)
library(dplyr)
library(tidyverse)
library(knitr)

## Step One
#-------------------------------------------------------------------------------------------------------------------------------------------------
# Expected Goals

# importing game data for 19-20 season
results_1920 <- read.csv("data/19_20_PL_results.csv", nrows = 380)
results_1920 <- results_1920 %>% select(Round.Number, Home.Team, Away.Team, Home, Away)

# function that returns expected goals scored for a specific match (input two teams and a matchday)
expected_goals <- function(home, away, matchday) {
  rounds = seq((matchday - 1), (matchday - 19)) # matchdays to take data from
  
  home_data <- filter(results_1920, results_1920$Home.Team == home | results_1920$Away.Team == home)
  away_data <- filter(results_1920, results_1920$Home.Team == away | results_1920$Away.Team == away)
  
  home_scored <- 0
  home_conceded <- 0
  away_scored <- 0
  away_conceded <- 0
  
  for (i in rounds) {
    if (home_data[i,]$Home.Team == home) {
      home_scored = home_scored + home_data[i,]$Home
      home_conceded = home_conceded + home_data[i,]$Away
    }
    
    if (home_data[i,]$Away.Team == home) {
      home_scored = home_scored + home_data[i,]$Away
      home_conceded = home_conceded + home_data[i,]$Home
    }
    
    if (away_data[i,]$Away.Team == away) {
      away_scored = away_scored + away_data[i,]$Away
      away_conceded = away_conceded + away_data[i,]$Home
    }
    
    if (away_data[i,]$Home.Team == away) {
      away_scored = away_scored + away_data[i,]$Home
      away_conceded = away_conceded + away_data[i,]$Away
    }
  }
  
  home_expected <- (home_scored + away_conceded) / 2
  away_expected <- (away_scored + home_conceded) / 2
  
  c(home_expected/19, away_expected/19)
}

expected_goals("Aston Villa", "Arsenal", 37)

#-------------------------------------------------------------------------------------------------------------------------------------------------
#League Table Caculator


# importing game data from 19-20 season
results_1920 <- read.csv("data/19_20_PL_results.csv", nrows = 380)
results_1920 <- results_1920 %>% select(Round.Number, Date, Home.Team, Away.Team, Home, Away)
#results_1920$ID <- seq.int(nrow(results_1920)) # giving each game an ID


# importing table format
table_1920 <- read.csv("data/19_20_PL_table.csv")

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

# function that updates table
update_table <- function(table, team, result, goals_for, goals_against) {
  table[ table$Team == team,]$GF <- table[ table$Team == team,]$GF + goals_for
  table[ table$Team == team,]$GA <- table[ table$Team == team,]$GA + goals_against
  
  if (result == "win") {
    table[ table$Team == team,]$P <- table[ table$Team == team,]$P + 3
    
  }
  
  if (result == "tie") {
    table[ table$Team == team,]$P <- table[ table$Team == team,]$P + 1
  }
  
  table <- table %>% mutate(GD = GF - GA)
  table <- table[order(-table$P, -table$GD),]
  rownames(table) <- 1:nrow(table)
  
  table
}

# function that get's position of specific team in table
get_position <- function(table, team) {
  as.numeric(rownames(table[ table$Team == team,]))
}

#-------------------------------------------------------------------------------------------------------------------------------------------------
## Step 2: MC Function

mc_step2 <- function(match_num) {
  m <- 100 #Number of times to run it. 
  win_stayup <- 0
  win_relegated <- 0
  
  lose_stayup <- 0
  lose_relegated <- 0
  
  tie_stayup <- 0
  tie_relegated <- 0
  
  final_pos <- NULL
  
  for (j in seq(1, m)) {
    win <- 0
    loss <- 0
    tie <- 0
    starting_table <- league_table(368)
    result <- "result"
    
    for (i in seq(match_num, 380)) {
      home <- results_1920[i,]$Home.Team
      away <- results_1920[i,]$Away.Team
      round <- results_1920[i,]$Round.Number
      
      x <- expected_goals(home, away, round)
      
      h_g <- rpois(1, x[1])
      a_g <- rpois(1, x[2])
      
      if (i == match_num) {
        if (h_g > a_g) {
          result = "win"
        }
        if (h_g < a_g) {
          result = "lose"
        }
        if (h_g == a_g) {
          result = "tie"
        }
      }
      
      if (h_g > a_g) {
        starting_table <- update_table(starting_table, home, "win", h_g, a_g)
        starting_table <- update_table(starting_table, away, "loss", a_g, h_g)
      }
      
      if (h_g < a_g) {
        starting_table <- update_table(starting_table, home, "loss", h_g, a_g)
        starting_table <- update_table(starting_table, away, "win", a_g, h_g)
      }
      
      if (h_g == a_g) {
        starting_table <- update_table(starting_table, home, "tie", h_g, a_g)
        starting_table <- update_table(starting_table, away, "tie", a_g, h_g)
      }
      
    }
    
    final_pos <- get_position(starting_table, "Aston Villa")
    
    if (result == "win" & final_pos < 18) {
      win_stayup <- win_stayup + 1
    }
    
    if (result == "win" & final_pos >= 18) {
      win_relegated <- win_relegated + 1
    }
    
    if (result == "lose" & final_pos < 18) {
      lose_stayup <- lose_stayup + 1
    }
    
    if (result == "lose" & final_pos >= 18) {
      lose_relegated <- lose_relegated + 1
    }
    
    if (result == "tie" & final_pos < 18) {
      tie_stayup <- tie_stayup + 1
    }
    
    if (result == "tie" & final_pos >= 18) {
      tie_relegated <- tie_relegated + 1
    }
  }
  c(win_stayup, win_relegated, lose_stayup, lose_relegated, tie_stayup, tie_relegated)
}

result <- mc_step2(368)

result


#-------------------------------------------------------------------------------------------------------------------------------------------------
##Step 3
#B will do with tau function

#J will set up function to predict rest of season