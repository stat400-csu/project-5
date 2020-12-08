library(ggplot2)
library(readr)
library(dplyr)
library(tidyverse)

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

expected_goals("Arsenal", "Aston Villa", 37) # Aston Villa vs. Arsenal (week 37)

