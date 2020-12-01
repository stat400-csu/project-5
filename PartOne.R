
#Step One: Probabilities of Match Results

#Averages from the last 19 matches
#How do we handle the new teams and the teams that were relegated last season?
#How best to get the data from the CSV? We may need to change the way the data is
#i.e. instead of the match results, have the teams results. 
#Then we could just plug in the home and away teams results
avg_home_goal <- 0
avg_home_concede <- 0

avg_away_goal <- 0
avg_away_concede <- 0

#Probability of Each team winning
#Should this be a function that we call in Step 2?
prob_home_win <- (avg_home_goal + avg_away_concede)/2
prob_away_win <- (avg_away_goal + avg_home_concede)/2
prob_draw <- 0 #How to calculate this?

#Step Two: Monte Carlo Estimation of Final Ranks
#Actual match results up until this game are combined with estimated probabilities 
#for all the matches remaining until the end of the season to find out the probabilities 
#of final team rank. 
#Do we want to predict the end of the 2020/2021 season, or just standings on 12/8 (first presentation day)?


