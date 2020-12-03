library(ggplot2)

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

#----------------------------------------------------------------------------------------------------------------------------------------------------
# re-creating example on pg. 395 (man city vs. man united, round 36, 2011-2012)

# expected average goals
ev_mc <- 1.842
ev_mu <- 1.237

# poisson distributed variables (function that returns probability of scoring a certain amount of goals)
p_mc = function(goals) {
  dpois(goals, ev_mc)
}

p_mu = function(goals) {
  dpois(goals, ev_mu)
}

p_mc(1) # .292 (matches result from paper)
p_mu(0) # .290 (matches result from paper)

# probability of man city winning 1-0 (the two variables are independent of each other)
p_mc(1) * p_mu(0) # .085 (matches result from paper)

# plot of the two distributions (blue = man city, red = man united)
x_s <- seq(0, 10)
ggplot() + geom_line(aes(x_s, p_mc(x_s)), colour = "blue") + geom_line(aes(x_s, p_mu(x_s)), colour = "red") + xlab("goals") + ylab("density")

# so, most goals ever scored in a premier league game is 9, so we can check all end-game scenarios up to 9?                                                                                                                              

mc_win_prob <- 0
mu_win_prob <- 0
tie_prob <- 0
goals = seq(0, 9)

# double for loop calculating probabilities for all scenarios up to 9 goals each (i = mc, j = mu)
for (i in goals) {
  for (j in goals) {
    if (i == j) {
      tie_prob <- tie_prob + (p_mc(i)*p_mu(j))
    }
    if (i > j) {
      mc_win_prob <- mc_win_prob + (p_mc(i)*p_mu(j))
    }
    if (i < j) {
      mu_win_prob <- mu_win_prob + (p_mc(i)*p_mu(j))
    }
  }
}

# numbers are all similar to the ones on pg. 395 of the paper
data.frame("mc win" = round(mc_win_prob, 3), "draw" = round(tie_prob, 3), "mu win" = round(mu_win_prob, 3))
                                                                                                                                   
#-------------------------------------------------------------------------------------------------------------------------------------------------


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


