library(ggplot2)

#Step One: Probabilities of Match Results


#----------------------------------------------------------------------------------------------------------------------------------------------------
# re-creating example on pg. 395 (man city vs. man united, round 36, 2011-2012)
#by JK 12/02/2020

# expected average goals
#will have to figure out a way to load these automatically or just set up?
ev_mc <- 1.842 #ev_home
ev_mu <- 1.237 #ev_away

# poisson distributed variables (function that returns probability of scoring a certain amount of goals)
p_mc = function(goals) { #p_home = function(goals, ev_home) {
  dpois(goals, ev_mc) #dpois(goals, ev_home)
}

p_mu = function(goals) { #p_away = function(goals, ev_away) {
  dpois(goals, ev_mu) #dpois(goals, ev_away)
}

p_mc(1) # .292 (matches result from paper) p_home(goals, ev_home)
p_mu(0) # .290 (matches result from paper) p_away(goals, ev_away)

# probability of man city winning 1-0 (the two variables are independent of each other)
p_mc(1) * p_mu(0) # .085 (matches result from paper)

# plot of the two distributions (blue = man city, red = man united)
x_s <- seq(0, 10)
ggplot() + geom_line(aes(x_s, p_mc(x_s)), colour = "blue") + geom_line(aes(x_s, p_mu(x_s)), colour = "red") + xlab("goals") + ylab("density")

# so, most goals ever scored in a premier league game is 9, so we can check all end-game scenarios up to 9?                                                                                                                              

mc_win_prob <- 0 #home_win_prob
mu_win_prob <- 0 #away_win_prob
tie_prob <- 0
goals = seq(0, 9)

# double for loop calculating probabilities for all scenarios up to 9 goals each (i = mc, j = mu)
for (i in goals) {
  for (j in goals) {
    if (i == j) {
      tie_prob <- tie_prob + (p_mc(i)*p_mu(j)) #tie_prob <- tie_prob + (p_home(i)*p_away(j))
    }
    if (i > j) {
      mc_win_prob <- mc_win_prob + (p_mc(i)*p_mu(j)) #home_win_prob <- home_win_prob + (p_home(i)*p_away(j))
    }
    if (i < j) {
      mu_win_prob <- mu_win_prob + (p_mc(i)*p_mu(j)) #away_win_prob <- away_win_prob + (p_home(i)*p_away(j))
    }
  }
}

# numbers are all similar to the ones on pg. 395 of the paper
data.frame("mc win" = round(mc_win_prob, 3), "draw" = round(tie_prob, 3), "mu win" = round(mu_win_prob, 3))
#data.frame("Home win" = round(home_win_prob, 3), "draw" = round(tie_prob, 3), "Away win" = round(away_win_prob, 3))                                                                                                                                   

#-------------------------------------------------------------------------------------------------------------------------------------------------

# re-creating above but for Home/away games
#by OM 12/02/2020

# expected average goals
#will have to figure out a way to load these automatically or just set up?
ev_home <- 0
ev_away <- 0
goals_home <- 0
goals_away <- 0

# poisson distributed variables (function that returns probability of scoring a certain amount of goals)
p_home = function(goals_home, ev_home) {
  dpois(goals_home, ev_home)
}

p_away = function(goals_away, ev_away) {
  dpois(goals_away, ev_away)
}

#Confirming the results (PART 4 OF PAPER)
p_home(goals_home, ev_home) #(matches result from week's game)
p_away(goals_away, ev_away) #(matches result from week's game)


#Probability of true result
#p_home(SCORE)*p_away(SCORE)

# plot of the two distributions (blue = HOME, red = AWAY)
x_s <- seq(0, 10)
ggplot() + geom_line(aes(x_s, p_mc(x_s)), colour = "blue") + geom_line(aes(x_s, p_mu(x_s)), colour = "red") + xlab("goals") + ylab("density")

# so, most goals ever scored in a premier league game is 9, so we can check all end-game scenarios up to 9?                                                                                                                              

home_win_prob <- 0 
away_win_prob <- 0
tie_prob <- 0
goals = seq(0, 9)

# double for loop calculating probabilities for all scenarios up to 9 goals each (i = mc, j = mu)
for (i in goals) {
  for (j in goals) {
    if (i == j) {
      tie_prob <- tie_prob + (p_home(i)*p_away(j))
    }
    if (i > j) {
      home_win_prob <- home_win_prob + (p_home(i)*p_away(j))
    }
    if (i < j) {
      away_win_prob <- away_win_prob + (p_home(i)*p_away(j))
    }
  }
}

# numbers are all similar to the ones on pg. 395 of the paper
#data.frame("mc win" = round(mc_win_prob, 3), "draw" = round(tie_prob, 3), "mu win" = round(mu_win_prob, 3))
data.frame("Home win" = round(home_win_prob, 3), "draw" = round(tie_prob, 3), "Away win" = round(away_win_prob, 3)) 



#-------------------------------------------------------------------------------------------------------------------------------------------------
#by OM 12/02/2020
#Step Two

ev_home <- 0
ev_away <- 0
goals_home <- 0
goals_away <- 0

# poisson distributed variables (function that returns probability of scoring a certain amount of goals)
p_home = function(goals_home, ev_home) {
  dpois(goals_home, ev_home)
}

p_away = function(goals_away, ev_away) {
  dpois(goals_away, ev_away)
}

mc_function = function (goals_home, ev_home, goals_away, ev_away){
  home_win_prob <- 0 
  away_win_prob <- 0
  tie_prob <- 0
  goals = seq(0, 9)
  
  # double for loop calculating probabilities for all scenarios up to 9 goals each (i = mc, j = mu)
  for (i in goals) {
    for (j in goals) {
      if (i == j) {
        tie_prob <- tie_prob + (p_home(i)*p_away(j))
      }
      if (i > j) {
        home_win_prob <- home_win_prob + (p_home(i)*p_away(j))
      }
      if (i < j) {
        away_win_prob <- away_win_prob + (p_home(i)*p_away(j))
      }
    }
  }
  
  # numbers are all similar to the ones on pg. 395 of the paper
  #data.frame("mc win" = round(mc_win_prob, 3), "draw" = round(tie_prob, 3), "mu win" = round(mu_win_prob, 3))
  data.frame("Home win" = round(home_win_prob, 3), "draw" = round(tie_prob, 3), "Away win" = round(away_win_prob, 3)) 
}

mc_function( 1, 1.842, 0, 1.237)

#-------------------------------------------------------------------------------------------------------------------------------------------------
