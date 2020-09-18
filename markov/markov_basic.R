# Markov chains in baseball

# Runners  ---  1--  -2-  --3  12-  1-3  -23  123
# Outs
#  0        1    4    7   10   13   16   19   22 
#  1        2    5    8   11   14   17   20   23
#  2        3    6    9   12   15   18   21   24

# T is a 28x28 matrix with transition probabilities from one state to another
# T_(i,j) gives the probability of going from state i to state j
# The sum of all transition probabilities in a given row must equal 1

# https://tinyurl.com/y3t4om6o
# This is the most simple form of a Markov chain: 
# - every batter has the same transition matrix
# - can't score on an out (very naive assumption)
# - doesn't differentiate between inning-ending base-out states
# - assumes only six offensive outcomes possible:
# -- out (no advancing), BB, 1B, 2B, 3B, HR

library(wordspace)
library(tidyverse)
source("examples/common.R")

# Probability of a one-base error

p_error_1b <- 0.85
p_error_2b <- 1 - p_error_1b

# Probability of scoring from first on a double

p_2b_score_from_1b <- 0.38
p_2b_go_to_3b <- 1 - p_2b_score_from_1b

# Probability of going first to third on a single

p_1b_first_to_third <- 0.3
p_1b_first_to_second <- 1 - p_1b_first_to_third

# Probability of going second to third on a field out

p_fo_second_to_third <- 0.15
p_fo_second_stay <- 1 - p_fo_second_to_third

# Function to get results of a plate appearance
# This is essentially copied/pasted from pa_pred_simple.R

get_results <- function(bat, pitch, stad, home, temp, state, idx) {
  
  #initialize empty lists
  candidate_batters <- list()
  candidate_pitchers <- list()
  
  for (batter in bat)
  {
    candidate_batters <- append(candidate_batters, sing$GetPlayers(name=batter))
  }
  
  for (pitcher in pitch)
  {
    candidate_pitchers <- append(candidate_pitchers, sing$GetPlayers(name=pitcher))
  }
  
  venue <- sing$GetVenues(stadium.name = stad)[[1]]
  atmosphere <- Atmosphere$new(venue = venue, 
                               temperature = temp, 
                               home_team = sing$GetTeams(name = home)[[1]])
  state_input <- state
  state_input$bat_lineup_order <- idx
  
  matchups <- list()
  
  for (b in candidate_batters) 
  {
    for (p in candidate_pitchers)
    {
      matchups <- append(matchups, Matchup$new(batter = b, pitcher = p, 
                                               atmosphere = atmosphere, 
                                               state = state_input))
    }
  }
  
  results <- sing$GetPaSim(matchup = matchups)
  results = results[order(results$woba_exp, decreasing = TRUE), ]
  return(results)
  
}

# Function to create individual transition matrices

get_tmatrix <- function(batter, pitcher, stadium,
                        home, temp, away, idx) {
  
  # Uses info to get results of plate appearance
  # "Away" argument is T/F on whether team is on road
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away), idx)

  # Uses predicted results to get the transition matrix
  # A0: 8x8 transition matrix where there are no outs and outs don't increase
  # A1: 8x8 transition matrix where there is 1 out and outs don't increase
  # A2: 8x8 transition matrix where there are 2 outs and outs don't increase
  # B0: 8x8 transition matrix where there are no outs and outs increase by 1
  # B1: 8x8 transition matrix where there is 1 out and outs increase by 1
  # C: 8x8 transition matrix where number of outs increase by 2
  # D: 8x1 transition matrix containing inning-ending triple play probabilities
  # E: 8x1 transition matrix containing inning-ending double play probabilities
  # F2: 8x1 transition matrix containing inning-ending out probabilities
  
  # A0 <- matrix(0, 8, 8)
  # A1 <- matrix(0, 8, 8)
  # A2 <- matrix(0, 8, 8)
  # B0 <- matrix(0, 8, 8)
  # B1 <- matrix(0, 8, 8)
  # C <- matrix(0, 8, 8)
  # D <- matrix(0, 8, 1)
  # E <- matrix(0, 8, 1)
  # F2 <- matrix(0, 8, 1)
  
  tmatrix <- matrix(0, 25, 25)
  
  # Probability of --- to ---, starting with no outs and no increase

  tmatrix[1, 1] <- results$hr_exp

  # Probability of --- to 1--, starting with no outs and no increase
  
  tmatrix[1, 2] <- (results$ba_exp - results$double_exp - results$triple_exp - 
                      results$hr_exp) + 
    (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp +
    p_error_1b * results$e_exp
  
  # Probability of --- to -2-, starting with no outs and no increase
  
  tmatrix[1, 3] <- results$double_exp + p_error_2b * results$e_exp
  
  # Probability of --- to --3, starting with no outs and no increase
  
  tmatrix[1, 4] <- results$triple_exp
  
  # Probability of --- to ---, starting with no outs and one-out increase
  
  tmatrix[1, 9] <- 1 - results$obp_exp
  
  # Changing results to start at 1--, no out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, on_1b = TRUE), idx)
  
  # Probability of 1-- to ---, starting with no outs and no increase
  
  tmatrix[2, 1] <- results$hr_exp
  
  # Probability of 1-- to -2-, starting with no outs and no increase
  
  tmatrix[2, 3] <- p_2b_score_from_1b * results$double_exp
  
  # Probability of 1-- to --3, starting with no outs and no increase
  
  tmatrix[2, 4] <- results$triple_exp
  
  # Probability of 1-- to 12-, starting with no outs and no increase
  
  tmatrix[2, 5] <- p_1b_first_to_second * (results$ba_exp - results$double_exp - 
                                             results$triple_exp - results$hr_exp) + 
    (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp +
    p_error_1b * results$e_exp
  
  # Probability of 1-- to 1-3, starting with no outs and no increase
  
  tmatrix[2, 6] <- p_1b_first_to_third * (results$ba_exp - results$double_exp - 
                                            results$triple_exp - results$hr_exp)
  
  # Probability of 1-- to -23, starting with no outs and no increase
  
  tmatrix[2, 7] <- p_2b_go_to_3b * results$double_exp + p_error_2b * results$e_exp
  
  # Probability of 1-- to 1--, starting with no outs and one-out increase
  
  tmatrix[2, 10] <- results$fc_o_exp + results$fo_exp + results$so_exp
  
  # Probability of 1-- to -2-, starting with no outs and one-out increase
  
  tmatrix[2, 11] <- results$sh_exp
  
  # Probability of 1-- to ---, starting with no outs and two-out increase
  
  tmatrix[2, 17] <- results$dp_exp + results$gdp_exp + results$sf_dp_exp + 
    results$so_dp_exp
  
  # Changing results to start at -2-, no out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, on_2b = TRUE), idx)
  
  # Probability of -2- to ---, starting with no outs and no increase
  
  tmatrix[3, 1] <- results$hr_exp

  # Probability of -2- to 1--, starting with no outs and no increase
  
  tmatrix[3, 2] <- results$ba_exp - results$double_exp - 
    results$triple_exp - results$hr_exp
  
  # Probability of -2- to -2-, starting with no outs and no increase
  
  tmatrix[3, 3] <- results$double_exp + p_error_2b * results$e_exp
  
  # Probability of -2- to --3, starting with no outs and no increase
  
  tmatrix[3, 4] <- results$triple_exp
  
  # Probability of -2- to 12-, starting with no outs and no increase
  
  tmatrix[3, 5] <- (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp
  
  # Probability of -2- to 1-3, starting with no outs and no increase
  
  tmatrix[3, 6] <- p_error_1b * results$e_exp
  
  # Probability of -2- to -2-, starting with no outs and one-out increase
  
  tmatrix[3, 11] <- results$so_exp + p_fo_second_stay * results$fo_exp
  
  # Probability of -2- to --3, starting with no outs and one-out increase
  
  tmatrix[3, 12] <- p_fo_second_to_third * results$fo_exp + results$sh_exp
  
  # Probability of -2- to ---, starting with no outs and two-out increase
  
  tmatrix[3, 17] <- results$dp_exp + results$gdp_exp + results$sf_dp_exp + 
    results$so_dp_exp
  
  # Changing results to start at --3, no out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, on_3b = TRUE), idx)
  
  # Probability of --3 to ---, starting with no outs and no increase
  
  tmatrix[4, 1] <- results$hr_exp
  
  # Probability of --3 to 1--, starting with no outs and no increase
  
  tmatrix[4, 2] <- (results$ba_exp - results$double_exp - 
                      results$triple_exp - results$hr_exp) +
    p_error_1b * results$e_exp
  
  # Probability of --3 to -2-, starting with no outs and no increase
  
  tmatrix[4, 3] <- results$double_exp + p_error_2b * results$e_exp
  
  # Probability of --3 to --3, starting with no outs and no increase
  
  tmatrix[4, 4] <- results$triple_exp
  
  # Probability of --3 to 1-3, starting with no outs and no increase
  
  tmatrix[4, 6] <- (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp
  
  # Probability of --3 to ---, starting with no outs and one-out increase
  
  tmatrix[4, 9] <- results$sf_exp
  
  # Probability of --3 to --3, starting with no outs and one-out increase
  
  tmatrix[4, 12] <- results$so_exp
  
  # Probability of --3 to ---, starting with no outs and two-out increase
  
  tmatrix[4, 17] <- results$dp_exp + results$gdp_exp + results$sf_dp_exp + 
    results$so_dp_exp
  
  # Changing results to start at 12-, no out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, on_1b = TRUE, on_2b = TRUE), 
                         idx)
  
  # Probability of 12- to ---, starting with no outs and no increase
  
  tmatrix[5, 1] <- results$hr_exp
  
  # Probability of 12- to -2-, starting with no outs and no increase
  
  tmatrix[5, 3] <- p_2b_score_from_1b * results$double_exp
  
  # Probability of 12- to --3, starting with no outs and no increase
  
  tmatrix[5, 4] <- results$triple_exp
  
  # Probability of 12- to 12-, starting with no outs and no increase
  
  tmatrix[5, 5] <- p_1b_first_to_second * (results$ba_exp - results$double_exp - 
                                             results$triple_exp - results$hr_exp)
  
  # Probability of 12- to 1-3, starting with no outs and no increase
  
  tmatrix[5, 6] <- p_1b_first_to_third * (results$ba_exp - results$double_exp - 
                                      results$triple_exp - results$hr_exp)
  
  # Probability of 12- to -23, starting with no outs and no increase
  
  tmatrix[5, 7] <- p_2b_go_to_3b * results$double_exp + p_error_2b * results$e_exp
  
  # Probability of 12- to 123, starting with no outs and no increase
  
  tmatrix[5, 8] <- (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp +
    p_error_1b * results$e_exp
  
  # Probability of 12- to 12-, starting with no outs and one-out increase
  
  tmatrix[5, 13] <- results$f_out_exp + results$so_exp
  
  # Probability of 12- to 1-3, starting with no outs and one-out increase
  
  tmatrix[5, 14] <- results$fo_exp + results$fc_o_exp
  
  # Probability of 12- to -23, starting with no outs and one-out increase
  
  tmatrix[5, 15] <- results$sh_exp
  
  #############
  # DP PROBABILITY FOR 12- HERE
  #############
  
  # Probability of 12- to end of inning, starting with no outs
  
  tmatrix[5, 25] <- results$tp_exp
  
  # Changing results to start at 1-3, no out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, on_1b = TRUE, on_3b = TRUE), idx)
  
  # Probability of 1-3 to ---, starting with no outs and no increase
  
  tmatrix[6, 1] <- results$hr_exp

  # Probability of 1-3 to -2-, starting with no outs and no increase
  
  tmatrix[6, 3] <- p_2b_score_from_1b * results$double_exp
  
  # Probability of 1-3 to --3, starting with no outs and no increase
  
  tmatrix[6, 4] <- results$triple_exp
  
  # Probability of 1-3 to 12-, starting with no outs and no increase
  
  tmatrix[6, 5] <- p_1b_first_to_second * (results$ba_exp - results$double_exp - 
                                             results$triple_exp - results$hr_exp) + 
    p_error_1b * results$e_exp
  
  # Probability of 1-3 to 1-3, starting with no outs and no increase
  
  tmatrix[6, 6] <- p_1b_first_to_third * (results$ba_exp - results$double_exp - 
                                            results$triple_exp - results$hr_exp)
  
  # Probability of 1-3 to -23, starting with no outs and no increase
  
  tmatrix[6, 7] <- p_2b_go_to_3b * results$double_exp + p_error_2b * results$e_exp
  
  # Probability of 1-3 to 123, starting with no outs and no increase
  
  tmatrix[6, 8] <- (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp
  
  # Probability to 1-3 to 1--, starting with no outs and one-out increase
  
  tmatrix[6, 10] <- results$sf_exp
  
  # Probability of 1-3 to -2-, starting with no outs and one-out increase
  
  tmatrix[6, 11] <- results$sh_exp
  
  # Probability of 1-3 to 1-3, starting with no outs and one-out increase
  
  tmatrix[6, 14] <- results$so_exp
  
  #############
  # DP PROBABILITY FOR 1-3 HERE
  #############
  
  # Probability of 1-3 to end of inning, starting with no outs
  
  tmatrix[6, 25] <- results$tp_exp
  
  # Changing results to start at -23, no out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, on_2b = TRUE, on_3b = TRUE), idx)
  
  # Probability of -23 to ---, starting with no outs and no increase
  
  tmatrix[7, 1] <- results$hr_exp
  
  # Probability of -23 to 1--, starting with no outs and no increase
  
  tmatrix[7, 2] <- results$ba_exp - results$double_exp - 
    results$triple_exp - results$hr_exp
  
  # Probability of -23 to -2-, starting with no outs and no increase
  
  tmatrix[7, 3] <- results$double_exp + p_error_2b * results$e_exp
  
  # Probability of -23 to --3, starting with no outs and no increase
  
  tmatrix[7, 4] <- results$triple_exp
  
  # Probability of -23 to 1-3, starting with no outs and no increase
  
  tmatrix[7, 6] <- p_error_1b * results$e_exp
  
  # Probability of -23 to 123, starting with no outs and no increase
  
  tmatrix[7, 8] <- (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp
  
  # Probability of -23 to -2-, starting with no outs and one-out increase
  
  tmatrix[7, 11] <- results$sf_exp + p_fo_second_stay * results$f_out_exp
  
  # Probability of -23 to --3, starting with no outs and one-out increase
  
  tmatrix[7, 12] <- results$sh_exp + p_fo_second_to_third * results$f_out_exp
  
  # Probability of -23 to -23, starting with no outs and one-out increase
  
  tmatrix[7, 15] <- results$so_exp
  
  #############
  # DP PROBABILITY FOR -23 HERE
  #############
  
  # Probability of -23 to end of inning, starting with no outs
  
  tmatrix[7, 25] <- results$tp_exp
  
  # Changing results to start at 123, no out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, on_1b = TRUE, on_2b = TRUE, on_3b = TRUE), 
                         idx)
  
  # Probability of 123 to ---, starting with no outs and no increase
  
  tmatrix[8, 1] <- results$hr_exp

  # Probability of 123 to -2-, starting with no outs and no increase
  
  tmatrix[8, 3] <- p_2b_score_from_1b * results$double_exp
  
  # Probability of 123 to --3, starting with no outs and no increase
  
  tmatrix[8, 4] <- results$triple_exp
  
  # Probability of 123 to -23, starting with no outs and no increase
  
  tmatrix[8, 7] <- p_2b_go_to_3b * results$double_exp + p_error_2b * results$e_exp
  
  # Probability of 123 to 123, starting with no outs and no increase
  
  tmatrix[8, 8] <- (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp +
    p_error_1b * results$e_exp

  # Probability of 123 to 12-, starting with no outs and one-out increase
  
  tmatrix[8, 13] <- results$sf_exp + 0.5 * p_fo_second_stay * results$f_out_exp
  
  # Probability of 123 to 1-3, starting with no outs and one-out increase
  
  tmatrix[8, 14] <- p_fo_second_to_third * results$f_out_exp + results$fo_exp + 
    results$fc_o_exp
  
  # Probability of 123 to -23, starting with no outs and one-out increase
  
  tmatrix[8, 15] <- results$sh_exp
  
  # Probability of 123 to 123, starting with no outs and one-out increase
  
  tmatrix[8, 16] <- 0.5 * p_fo_second_stay * results$f_out_exp + results$so_exp
  
  #############
  # DP PROBABILITY FOR 123 HERE
  #############
  
  # Probability of 123 to end of inning, starting with no outs
  
  tmatrix[8, 25] <- results$tp_exp
  
  # Changing results to start at ---, one out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, outs = 1), idx)
  
  # Probability of --- to ---, starting with one out and no increase
  
  tmatrix[9, 9] <- results$hr_exp
  
  # Probability of --- to 1--, starting with one out and no increase
  
  tmatrix[9, 10] <- (results$ba_exp - results$double_exp - results$triple_exp - 
                       results$hr_exp) + 
    (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp +
    p_error_1b * results$e_exp
  
  # Probability of --- to -2-, starting with one out and no increase
  
  tmatrix[9, 11] <- results$double_exp + p_error_2b * results$e_exp
  
  # Probability of --- to --3, starting with one out and no increase
  
  tmatrix[9, 12] <- results$triple_exp
  
  # Probability of --- to ---, starting with one out and one-out increase
  
  tmatrix[9, 17] <- 1 - results$obp_exp
  
  # Changing results to start at 1--, one out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, outs = 1, on_1b = TRUE), idx)
  
  # Probability of 1-- to ---, starting with one out and no increase
  
  tmatrix[10, 9] <- results$hr_exp
  
  # Probability of 1-- to -2-, starting with one out and no increase
  
  tmatrix[10, 11] <- p_2b_score_from_1b * results$double_exp
  
  # Probability of 1-- to --3, starting with one out and no increase
  
  tmatrix[10, 12] <- results$triple_exp
  
  # Probability of 1-- to 12-, starting with one out and no increase
  
  tmatrix[10, 13] <- p_1b_first_to_second * (results$ba_exp - results$double_exp - 
                                               results$triple_exp - results$hr_exp) + 
    (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp +
    p_error_1b * results$e_exp
  
  # Probability of 1-- to 1-3, starting with one out and no increase
  
  tmatrix[10, 14] <- p_1b_first_to_third * (results$ba_exp - results$double_exp - 
                                              results$triple_exp - results$hr_exp)
  
  # Probability of 1-- to -23, starting with one out and no increase
  
  tmatrix[10, 15] <- p_2b_go_to_3b * results$double_exp + p_error_2b * results$e_exp
  
  # Probability of 1-- to 1--, starting with one out and one-out increase
  
  tmatrix[10, 18] <- results$fc_o_exp + results$fo_exp + results$so_exp
  
  # Probability of 1-- to -2-, starting with one out and one-out increase
  
  tmatrix[10, 19] <- results$sh_exp
  
  # Probability of 1-- to end of inning, starting with one out
  
  tmatrix[10, 25] <- results$dp_exp + results$gdp_exp + results$sf_dp_exp + 
    results$so_dp_exp
  
  # Changing results to start at -2-, one out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, outs = 1, on_2b = TRUE), idx)
  
  # Probability of -2- to ---, starting with one out and no increase
  
  tmatrix[11, 9] <- results$hr_exp
  
  # Probability of -2- to 1--, starting with one out and no increase
  
  tmatrix[11, 10] <- results$ba_exp - results$double_exp - 
    results$triple_exp - results$hr_exp
  
  # Probability of -2- to -2-, starting with one out and no increase
  
  tmatrix[11, 11] <- results$double_exp + p_error_2b * results$e_exp
  
  # Probability of -2- to --3, starting with one out and no increase
  
  tmatrix[11, 12] <- results$triple_exp
  
  # Probability of -2- to 12-, starting with one out and no increase
  
  tmatrix[11, 13] <- (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp
  
  # Probability of -2- to 1-3, starting with one out and no increase
  
  tmatrix[11, 14] <- p_error_1b * results$e_exp
  
  # Probability of -2- to -2-, starting with one out and one-out increase
  
  tmatrix[11, 19] <- results$so_exp + p_fo_second_stay * results$fo_exp
  
  # Probability of -2- to --3, starting with one out and one-out increase
  
  tmatrix[11, 20] <- p_fo_second_to_third * results$fo_exp + results$sh_exp
  
  # Probability of -2- to end of inning, starting with one out
  
  tmatrix[11, 25] <- results$dp_exp + results$gdp_exp + results$sf_dp_exp + 
    results$so_dp_exp
  
  # Changing results to start at --3, one out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, outs = 1, on_3b = TRUE), idx)
  
  # Probability of --3 to ---, starting with one out and no increase
  
  tmatrix[12, 9] <- results$hr_exp
  
  # Probability of --3 to 1--, starting with one out and no increase
  
  tmatrix[12, 10] <- (results$ba_exp - results$double_exp - 
                        results$triple_exp - results$hr_exp) +
    p_error_1b * results$e_exp
  
  # Probability of --3 to -2-, starting with one out and no increase
  
  tmatrix[12, 11] <- results$double_exp + p_error_2b * results$e_exp
  
  # Probability of --3 to --3, starting with one out and no increase
  
  tmatrix[12, 12] <- results$triple_exp
  
  # Probability of --3 to 1-3, starting with one out and no increase
  
  tmatrix[12, 14] <- (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp
  
  # Probability of --3 to ---, starting with one out and one-out increase
  
  tmatrix[12, 17] <- results$sf_exp
  
  # Probability of --3 to --3, starting with one out and one-out increase
  
  tmatrix[12, 20] <- results$so_exp
  
  # Probability of --3 to end of inning, starting with one out
  
  tmatrix[12, 25] <- results$dp_exp + results$gdp_exp + results$sf_dp_exp + 
    results$so_dp_exp
  
  # Changing results to start at 12-, one out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, outs = 1, on_1b = TRUE, on_2b = TRUE), idx)
  
  # Probability of 12- to ---, starting with one out and no increase
  
  tmatrix[13, 9] <- results$hr_exp
  
  # Probability of 12- to -2-, starting with one out and no increase
  
  tmatrix[13, 11] <- p_2b_score_from_1b * results$double_exp
  
  # Probability of 12- to --3, starting with one out and no increase
  
  tmatrix[13, 12] <- results$triple_exp
  
  # Probability of 12- to 12-, starting with one out and no increase
  
  tmatrix[13, 13] <- p_1b_first_to_second * (results$ba_exp - results$double_exp - 
                                               results$triple_exp - results$hr_exp)
  
  # Probability of 12- to 1-3, starting with one out and no increase
  
  tmatrix[13, 14] <- p_1b_first_to_third * (results$ba_exp - results$double_exp - 
                                              results$triple_exp - results$hr_exp)
  
  # Probability of 12- to -23, starting with one out and no increase
  
  tmatrix[13, 15] <- p_2b_go_to_3b * results$double_exp + p_error_2b * results$e_exp
  
  # Probability of 12- to 123, starting with one out and no increase
  
  tmatrix[13, 16] <- (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp +
    p_error_1b * results$e_exp
  
  # Probability of 12- to 12-, starting with one out and one-out increase
  
  tmatrix[13, 21] <- results$f_out_exp + results$so_exp
  
  # Probability of 12- to 1-3, starting with one out and one-out increase
  
  tmatrix[13, 22] <- results$fo_exp + results$fc_o_exp
  
  # Probability of 12- to -23, starting with one out and one-out increase
  
  tmatrix[13, 23] <- results$sh_exp
  
  # Probability of 12- to end of inning, starting with one out

  tmatrix[13, 25] <- results$dp_exp + results$gdp_exp + results$sf_dp_exp + 
    results$so_dp_exp
  
  # Changing results to start at 1-3, one out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, outs = 1, on_1b = TRUE, on_3b = TRUE), idx)
  
  # Probability of 1-3 to ---, starting with one out and no increase
  
  tmatrix[14, 9] <- results$hr_exp
  
  # Probability of 1-3 to -2-, starting with one out and no increase
  
  tmatrix[14, 11] <- p_2b_score_from_1b * results$double_exp
  
  # Probability of 1-3 to --3, starting with one out and no increase
  
  tmatrix[14, 12] <- results$triple_exp
  
  # Probability of 1-3 to 12-, starting with one out and no increase
  
  tmatrix[14, 13] <- p_1b_first_to_second * (results$ba_exp - results$double_exp - 
                                               results$triple_exp - results$hr_exp) + 
    p_error_1b * results$e_exp
  
  # Probability of 1-3 to 1-3, starting with one out and no increase
  
  tmatrix[14, 14] <- p_1b_first_to_third * (results$ba_exp - results$double_exp - 
                                              results$triple_exp - results$hr_exp)
  
  # Probability of 1-3 to -23, starting with one out and no increase
  
  tmatrix[14, 15] <- p_2b_go_to_3b * results$double_exp + p_error_2b * results$e_exp
  
  # Probability of 1-3 to 123, starting with one out and no increase
  
  tmatrix[14, 16] <- (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp
  
  # Probability to 1-3 to 1--, starting with one out and one-out increase
  
  tmatrix[14, 18] <- results$sf_exp
  
  # Probability of 1-3 to -2-, starting with one out and one-out increase
  
  tmatrix[14, 19] <- results$sh_exp
  
  # Probability of 1-3 to 1-3, starting with one out and one-out increase
  
  tmatrix[14, 22] <- results$so_exp
  
  # Probability of 1-3 to end of inning, starting with one out
  
  tmatrix[14, 25] <- results$dp_exp + results$gdp_exp + results$sf_dp_exp + 
    results$so_dp_exp
  
  # Changing results to start at -23, no out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, outs = 1, on_2b = TRUE, on_3b = TRUE), idx)
  
  # Probability of -23 to ---, starting with one out and no increase
  
  tmatrix[15, 9] <- results$hr_exp
  
  # Probability of -23 to 1--, starting with one out and no increase
  
  tmatrix[15, 10] <- results$ba_exp - results$double_exp - 
    results$triple_exp - results$hr_exp
  
  # Probability of -23 to -2-, starting with one out and no increase
  
  tmatrix[15, 11] <- results$double_exp + p_error_2b * results$e_exp
  
  # Probability of -23 to --3, starting with one out and no increase
  
  tmatrix[15, 12] <- results$triple_exp
  
  # Probability of -23 to 1-3, starting with one out and no increase
  
  tmatrix[15, 14] <- p_error_1b * results$e_exp
  
  # Probability of -23 to 123, starting with one out and no increase
  
  tmatrix[15, 16] <- (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp
  
  # Probability of -23 to -2-, starting with one out and one-out increase
  
  tmatrix[15, 19] <- results$sf_exp + p_fo_second_stay * results$f_out_exp
  
  # Probability of -23 to --3, starting with one out and one-out increase
  
  tmatrix[15, 20] <- results$sh_exp + p_fo_second_to_third * results$f_out_exp
  
  # Probability of -23 to -23, starting with one out and one-out increase
  
  tmatrix[15, 23] <- results$so_exp
  
  # Probability of -23 to end of inning, starting with one out
  
  tmatrix[15, 25] <-  results$dp_exp + results$gdp_exp + results$sf_dp_exp + 
    results$so_dp_exp
  
  # Changing results to start at 123, no out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, outs = 1, on_1b = TRUE, on_2b = TRUE, 
                                   on_3b = TRUE), idx)
  
  # Probability of 123 to ---, starting with one out and no increase
  
  tmatrix[16, 9] <- results$hr_exp
  
  # Probability of 123 to -2-, starting with one out and no increase
  
  tmatrix[16, 11] <- p_2b_score_from_1b * results$double_exp
  
  # Probability of 123 to --3, starting with one out and no increase
  
  tmatrix[16, 12] <- results$triple_exp
  
  # Probability of 123 to -23, starting with one out and no increase
  
  tmatrix[16, 15] <- p_2b_go_to_3b * results$double_exp + p_error_2b * results$e_exp
  
  # Probability of 123 to 123, starting with one out and no increase
  
  tmatrix[16, 16] <- (results$bb_exp + results$ibb_exp) + results$hbp_exp + 
    results$ci_exp + p_error_1b * results$e_exp
  
  # Probability of 123 to 12-, starting with one out and one-out increase
  
  tmatrix[16, 21] <- results$sf_exp + 0.5 * p_fo_second_stay * results$f_out_exp
  
  # Probability of 123 to 1-3, starting with one out and one-out increase
  
  tmatrix[16, 22] <- p_fo_second_to_third * results$f_out_exp + results$fo_exp + 
    results$fc_o_exp
  
  # Probability of 123 to -23, starting with one out and one-out increase
  
  tmatrix[16, 23] <- results$sh_exp
  
  # Probability of 123 to 123, starting with one out and one-out increase
  
  tmatrix[16, 24] <- 0.5 * p_fo_second_stay * results$f_out_exp + results$so_exp
  
  # Probability of 123 to end of inning, starting with one out
  
  tmatrix[16, 25] <- results$dp_exp + results$gdp_exp + results$sf_dp_exp + 
    results$so_dp_exp
  
  # Changing results to start at ---, two out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, outs = 2), idx)
  
  # Probability of --- to ---, starting with two outs and no increase
  
  tmatrix[17, 17] <- results$hr_exp
  
  # Probability of --- to 1--, starting with two out and no increase
  
  tmatrix[17, 18] <- (results$ba_exp - results$double_exp - results$triple_exp - 
                        results$hr_exp) + 
    (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp +
    p_error_1b * results$e_exp
  
  # Probability of --- to -2-, starting with two out and no increase
  
  tmatrix[17, 19] <- results$double_exp + p_error_2b * results$e_exp
  
  # Probability of --- to --3, starting with two out and no increase
  
  tmatrix[17, 20] <- results$triple_exp
  
  # Probability of --- to end of inning, starting with two out
  
  tmatrix[17, 25] <- 1 - results$obp_exp
  
  # Changing results to start at 1--, two out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, outs = 2, on_1b = TRUE), idx)
  
  # Probability of 1-- to ---, starting with two out and no increase
  
  tmatrix[18, 17] <- results$hr_exp
  
  # Probability of 1-- to -2-, starting with two out and no increase
  
  tmatrix[18, 19] <- p_2b_score_from_1b * results$double_exp
  
  # Probability of 1-- to --3, starting with two out and no increase
  
  tmatrix[18, 20] <- results$triple_exp
  
  # Probability of 1-- to 12-, starting with two out and no increase
  
  tmatrix[18, 21] <- p_1b_first_to_second * (results$ba_exp - results$double_exp - 
                                               results$triple_exp - results$hr_exp) + 
    (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp +
    p_error_1b * results$e_exp
  
  # Probability of 1-- to 1-3, starting with two out and no increase
  
  tmatrix[18, 22] <- p_1b_first_to_third * (results$ba_exp - results$double_exp - 
                                              results$triple_exp - results$hr_exp)
  
  # Probability of 1-- to -23, starting with two out and no increase
  
  tmatrix[18, 23] <- p_2b_go_to_3b * results$double_exp + p_error_2b * results$e_exp
  
  # Probability of 1-- to end of inning, starting with two out
  
  tmatrix[18, 25] <- 1 - results$obp_exp
  
  # Changing results to start at -2-, two out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, outs = 2, on_2b = TRUE), idx)
  
  # Probability of -2- to ---, starting with two out and no increase
  
  tmatrix[19, 17] <- results$hr_exp
  
  # Probability of -2- to 1--, starting with two out and no increase
  
  tmatrix[19, 18] <- results$ba_exp - results$double_exp - 
    results$triple_exp - results$hr_exp
  
  # Probability of -2- to -2-, starting with two out and no increase
  
  tmatrix[19, 19] <- results$double_exp + p_error_2b * results$e_exp
  
  # Probability of -2- to --3, starting with two out and no increase
  
  tmatrix[19, 20] <- results$triple_exp
  
  # Probability of -2- to 12-, starting with two out and no increase
  
  tmatrix[19, 21] <- (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp
  
  # Probability of -2- to 1-3, starting with two out and no increase
  
  tmatrix[19, 22] <- p_error_1b * results$e_exp
  
  # Probability of -2- to end of inning, starting with two out
  
  tmatrix[19, 25] <- 1 - results$obp_exp
  
  # Changing results to start at --3, two out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, outs = 2, on_3b = TRUE), idx)
  
  # Probability of --3 to ---, starting with two out and no increase
  
  tmatrix[20, 17] <- results$hr_exp
  
  # Probability of --3 to 1--, starting with two out and no increase
  
  tmatrix[20, 18] <- (results$ba_exp - results$double_exp - 
                        results$triple_exp - results$hr_exp) +
    p_error_1b * results$e_exp
  
  # Probability of --3 to -2-, starting with two out and no increase
  
  tmatrix[20, 19] <- results$double_exp + p_error_2b * results$e_exp
  
  # Probability of --3 to --3, starting with two out and no increase
  
  tmatrix[20, 20] <- results$triple_exp
  
  # Probability of --3 to 1-3, starting with two out and no increase
  
  tmatrix[20, 22] <- (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp
  
  # Probability of --3 to end of inning, starting with two out
  
  tmatrix[20, 25] <- 1 - results$obp_exp
  
  # Changing results to start at 12-, two out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, outs = 2, on_1b = TRUE, on_2b = TRUE), idx)
  
  # Probability of 12- to ---, starting with two out and no increase
  
  tmatrix[21, 17] <- results$hr_exp
  
  # Probability of 12- to -2-, starting with two out and no increase
  
  tmatrix[21, 19] <- p_2b_score_from_1b * results$double_exp
  
  # Probability of 12- to --3, starting with two out and no increase
  
  tmatrix[21, 20] <- results$triple_exp
  
  # Probability of 12- to 12-, starting with two out and no increase
  
  tmatrix[21, 21] <- p_1b_first_to_second * (results$ba_exp - results$double_exp - 
                                               results$triple_exp - results$hr_exp)
  
  # Probability of 12- to 1-3, starting with two out and no increase
  
  tmatrix[21, 22] <- p_1b_first_to_third * (results$ba_exp - results$double_exp - 
                                              results$triple_exp - results$hr_exp)
  
  # Probability of 12- to -23, starting with two out and no increase
  
  tmatrix[21, 23] <- p_2b_go_to_3b * results$double_exp + p_error_2b * results$e_exp
  
  # Probability of 12- to 123, starting with two out and no increase
  
  tmatrix[21, 24] <- (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp +
    p_error_1b * results$e_exp
  
  # Probability of 12- to end of inning, starting with two out
  
  tmatrix[21, 25] <- 1 - results$obp_exp
  
  # Changing results to start at 1-3, two out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, outs = 2, on_1b = TRUE, on_3b = TRUE), idx)
  
  # Probability of 1-3 to ---, starting with two out and no increase
  
  tmatrix[22, 17] <- results$hr_exp
  
  # Probability of 1-3 to -2-, starting with two out and no increase
  
  tmatrix[22, 19] <- p_2b_score_from_1b * results$double_exp
  
  # Probability of 1-3 to --3, starting with two out and no increase
  
  tmatrix[22, 20] <- results$triple_exp
  
  # Probability of 1-3 to 12-, starting with two out and no increase
  
  tmatrix[22, 21] <- p_1b_first_to_second * (results$ba_exp - results$double_exp - 
                                               results$triple_exp - results$hr_exp) + 
    p_error_1b * results$e_exp
  
  # Probability of 1-3 to 1-3, starting with two out and no increase
  
  tmatrix[22, 22] <- p_1b_first_to_third * (results$ba_exp - results$double_exp - 
                                              results$triple_exp - results$hr_exp)
  
  # Probability of 1-3 to -23, starting with two out and no increase
  
  tmatrix[22, 23] <- p_2b_go_to_3b * results$double_exp + p_error_2b * results$e_exp
  
  # Probability of 1-3 to 123, starting with two out and no increase
  
  tmatrix[22, 24] <- (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp
  
  # Probability of 1-3 to end of inning, starting with two out
  
  tmatrix[22, 25] <- 1 - results$obp_exp
  
  # Changing results to start at -23, no out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, outs = 2, on_2b = TRUE, on_3b = TRUE), idx)
  
  # Probability of -23 to ---, starting with two out and no increase
  
  tmatrix[23, 17] <- results$hr_exp
  
  # Probability of -23 to 1--, starting with two out and no increase
  
  tmatrix[23, 18] <- results$ba_exp - results$double_exp - 
    results$triple_exp - results$hr_exp
  
  # Probability of -23 to -2-, starting with two out and no increase
  
  tmatrix[23, 19] <- results$double_exp + p_error_2b * results$e_exp
  
  # Probability of -23 to --3, starting with two out and no increase
  
  tmatrix[23, 20] <- results$triple_exp
  
  # Probability of -23 to 1-3, starting with two out and no increase
  
  tmatrix[23, 22] <- p_error_1b * results$e_exp
  
  # Probability of -23 to 123, starting with two out and no increase
  
  tmatrix[23, 24] <- (results$bb_exp + results$ibb_exp) + results$hbp_exp + results$ci_exp
  
  # Probability of --- to end of inning, starting with two out
  
  tmatrix[23, 25] <- 1 - results$obp_exp
  
  # Changing results to start at 123, two out state
  
  results <- get_results(batter, pitcher, stadium, home, temp, 
                         State$new(top = away, outs = 2, on_1b = TRUE, on_2b = TRUE, 
                                   on_3b = TRUE), idx)
  
  # Probability of 123 to ---, starting with two out and no increase
  
  tmatrix[24, 17] <- results$hr_exp
  
  # Probability of 123 to -2-, starting with two out and no increase
  
  tmatrix[24, 19] <- p_2b_score_from_1b * results$double_exp
  
  # Probability of 123 to --3, starting with two out and no increase
  
  tmatrix[24, 20] <- results$triple_exp
  
  # Probability of 123 to -23, starting with two out and no increase
  
  tmatrix[24, 23] <- p_2b_go_to_3b * results$double_exp + p_error_2b * results$e_exp
  
  # Probability of 123 to 123, starting with two out and no increase
  
  tmatrix[24, 24] <- (results$bb_exp + results$ibb_exp) + results$hbp_exp + 
    results$ci_exp + p_error_1b * results$e_exp
  
  # Probability of 123 to end of inning, starting with two out
  
  tmatrix[24, 25] <- 1 - results$obp_exp
  
  # Filling in the last element of the matrix
  
  tmatrix[25, 25] <- 1
  
  ########################
  # TO DO: Add field out probabilities for states with weird sums
  # Add in double-play probabilities for no outs and following states:
  # 12-, 1-3, -23, 123
  # Complete full transition matrix
  #######################
  
  # Normalizing the matrix for proper calculation
  
  for (i in 1:25) {
    rowsum <- sum(tmatrix[i,])
    tmatrix[i,] <- tmatrix[i,] / rowsum
  }
  
  # Adjusting the matrix components if there are outs
  # Commenting out for now
  
  # if (state$outs == 1) {
  #   test_matrix[1:8,] <- 0
  # }
  # 
  # else if (state$outs == 2) {
  #   test_matrix[1:16,] <- 0
  # }
  
  return(tmatrix)
  
}

# The error in the probability
# Exits when difference between the sum of all run-scoring probabilities and 1 is < EPSILON

EPSILON <- 1 / 100000

# Uses Markov chains to get run-scoring probability distributions for a half-inning

markov_half_inning <- function(idx, info, state = State$new(top = FALSE)) {
  
  # Assigns values from function
  # Also creates a list of the 24 possible batting states
  
  batters <- info[[1]]
  pitcher <- info[[2]]
  stadium <- info[[3]]
  home <- info[[4]]
  temp <- info[[5]]
  away <- state$top

  # Uses this info to create the transition matrix for each batter
  
  for (batter in 1:9) {
    matrix <- get_tmatrix(batters[batter], pitcher, stadium, home, temp, away, batter)
    assign(paste0("tmatrix_", batter), matrix)
  }
  
  # List of the transition matrices for the nine batters
  
  tmatrix_list <- list(tmatrix_1, tmatrix_2, tmatrix_3,
                       tmatrix_4, tmatrix_5, tmatrix_6,
                       tmatrix_7, tmatrix_8, tmatrix_9)
  
  # Initializing 21x25 scorekeeping matrix
  # 21 rows because we assume teams can score from 0 to 20 runs in a game
  # 25 columns because there are 25 unique states (including end of inning)
  
  scores <- matrix(0, 21, 25)
  
  # This corresponds to the starting state
  # 1 -> 8 correspond to no outs, 9 -> 16 are one out, 17 -> 24 two outs
  # In order within each: ---, 1--, -2-, --3, 12-, 1-3, -23, 123
  # ex. 2 corresponds to no outs, runner on first
  # ex. 13 corresponds to one out, runners on first and second
  # ex. 24 corresponds to two outs, bases loaded
  # The math to utilize this for the starting state is below
  
  state_input <- 1 + 8*state$outs + state$on_1b + 2*state$on_2b + 3*state$on_3b +
    ((state$on_1b & state$on_2b) | (state$on_1b & state$on_3b) | (state$on_2b & state$on_3b))
  
  scores[1, state_input] <- 1
  
  # Algorithm from:
  # https://pdfs.semanticscholar.org/563d/11f4baec14278357149a9726677453ba79a2.pdf
  
  while (abs(sum(scores[,25]) - 1) > EPSILON) {
    
    # Resets batting order index in case we go back to the top of the lineup 
    
    idx <- idx %% length(tmatrix_list)
    
    if (idx == 0) {
      idx <- 9
    }
    
    # Creates scoring probability matrices
    # Does this manually unfortunately, definitely a better way
    
    t_matrix <- tmatrix_list[[idx]]
    
    # This constructs run-scoring transition probabilities 
    # Does so for each possible number of runs on a given play
    # ex. Non-zero p_1 entries indicate a non_zero prob. of scoring 1 run on that transition
    
    p_1 <- matrix(0, 25, 25)
    p_1[1,1] <- t_matrix[1,1]
    p_1[2:4, 2:4] <- t_matrix[2:4, 2:4]
    p_1[5:7, 5:7] <- t_matrix[5:7, 5:7]
    p_1[8,8] <- t_matrix[8,8]
    p_1[9,9] <- t_matrix[9,9]
    p_1[10:12, 10:12] <- t_matrix[10:12, 10:12]
    p_1[13:15, 13:15] <- t_matrix[13:15, 13:15]
    p_1[16,16] <- t_matrix[16,16]
    p_1[17,17] <- t_matrix[17,17]
    p_1[18:20, 18:20] <- t_matrix[18:20, 18:20]
    p_1[21:23, 21:23] <- t_matrix[21:23, 21:23]
    p_1[24,24] <- t_matrix[24,24]
    
    p_2 <- matrix(0, 25, 25)
    p_2[2:4,1] <- t_matrix[2:4,1]
    p_2[5:7,2:4] <- t_matrix[5:7,2:4]
    p_2[8,5:7] <- t_matrix[8,5:7]
    p_2[10:12,9] <- t_matrix[10:12,9]
    p_2[13:15,10:12] <- t_matrix[13:15,10:12]
    p_2[16,13:15] <- t_matrix[16,13:15]
    p_2[18:20,17] <- t_matrix[18:20,17]
    p_2[21:23,18:20] <- t_matrix[21:23,18:20]
    p_2[24,21:23] <- t_matrix[24,21:23]
    
    p_3 <- matrix(0, 25, 25)
    p_3[5:7, 1] <- t_matrix[5:7, 1]
    p_3[8, 2:4] <- t_matrix[8, 2:4]
    p_3[13:15, 9] <- t_matrix[13:15, 9]
    p_3[16, 10:12] <- t_matrix[16, 10:12]
    p_3[21:23, 17] <- t_matrix[21:23, 17]
    p_3[24, 18:20] <- t_matrix[24, 18:20]
    
    p_4 <- matrix(0, 25, 25)
    p_4[8, 1] <- t_matrix[8, 1]
    p_4[16, 9] <- t_matrix[16, 9]
    p_4[24, 17] <- t_matrix[24, 17]
    
    # Creates matrix w/ probs. of scoring 0 runs from the others given
    
    p_0 <- t_matrix - p_1 - p_2 - p_3 - p_4
    
    # Creates empty 21x25 matrix
    
    temp <- matrix(0, 21, 25)
    
    # Adds expected runs to matrix, using same algorithm
    
    for (j in 1:21) {
      
      if (j == 1) {
        row <- scores[j,] %*% p_0
      }
      
      else if (j == 2) {
        row <- scores[j,] %*% p_0 + scores[j-1,] %*% p_1
      }
      
      else if (j == 3) {
        row <- scores[j,] %*% p_0 + scores[j-1,] %*% p_1 + scores[j-2,] %*% p_2
      }
      
      else if (j == 4) {
        row <- scores[j,] %*% p_0 + scores[j-1,] %*% p_1 + scores[j-2,] %*% p_2 +
          scores[j-3,] %*% p_3
      }
      
      else {
        row <- scores[j,] %*% p_0 + scores[j-1,] %*% p_1 + scores[j-2,] %*% p_2 +
          scores[j-3,] %*% p_3 + scores[j-4,] %*% p_4
      }
      
      temp[j,] <- row
      
    }
    
    scores <- temp
    
    idx <- idx + 1
    
  }
  
  # Turns output into a tibble to make output look presentable
  
  run_prob <- scores[,25]
  runs <- tibble(run_prob) %>% 
    mutate("Expected Runs Scored" = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
                                      11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
           "Probability" = run_prob) %>% 
    select("Expected Runs Scored", "Probability")

  return(runs)
  
}

# Function for full game Markov chain run distribution

markov_full_game <- function() {
  
  info_list <- get_info()
  run_distr <- list()
  idx <- 1
  
  for (inning in 1:9) {
    distr <- markov_half_inning(idx, info_list)
    run_distr[[inning]] <- distr
  }
  
  return(run_distr)
  
}

# Creating function for dynamic user input
# Can be called from within RStudio or from the command line

main <- function() {
  
  # Creates default situation for prediction
  
  batters <- c("Mookie Betts", "Max Muncy", "Justin Turner", "Cody Bellinger", 
               "Corey Seager", "AJ Pollock", "Joc Pederson", "Austin Barnes", 
               "Gavin Lux")
  
  pitcher <- "Chris Paddack"
  
  stadium <- "Dodger Stadium"
  
  home <- "Dodgers"
  
  temp <- 75
  
  state <- State$new(top = FALSE)
  
  info <- list(batters, pitcher, stadium, home, temp)
  
  # Has two different options: default matchup or custom matchup
  
  if (interactive()) {
    
    # Asks for user input on matchup type
    
    cat("Run default matchup of Dodgers batting vs. Chris Paddack in the 
        bottom of the first inning? (TRUE or FALSE) ")
    default <- as.logical(readLines(con = stdin(), 1))
    
    # Checks to make sure a logical value was actually imputed
    
    while (is.na(default)) {
      
      cat("Run default matchup of Dodgers batting vs. Chris Paddack in the 
          bottom of the first inning? (TRUE or FALSE) ")
      default <- as.logical(readLines(con = stdin(), 1))
      
    }
    
    # Runs default matchup, starting at the top of the order
    
    if (default) {

      markov_half_inning(1, info)
      
    }
    
    else {
      
      # Asks whether user wants to input a new lineup
      
      cat("New lineup? (TRUE or FALSE) ")
      new_lineup <- as.logical(readLines(con = stdin(), 1))
      
      # Checks to make sure a logical value was actually imputed
      
      while (is.na(new_lineup)) {
        
        cat("New lineup? (TRUE or FALSE) ")
        new_lineup <- as.logical(readLines(con = stdin(), 1))
        
      }
      
      # Gets inputs for new lineup if user wants
      
      if (new_lineup) {
        
        cat("Leadoff hitter: ")
        bat1 <- readLines(con = stdin(), 1)
        
        # Checks to make sure a real player was imputed
        
        while (is.null(sing$GetPlayers(name = bat1))) {
          
          cat("Leadoff hitter: ")
          bat1 <- readLines(con = stdin(), 1)
          
        }
        
        cat("Second hitter: ")
        bat2 <- readLines(con = stdin(), 1)
        
        # Same check
        
        while (is.null(sing$GetPlayers(name = bat2))) {
          
          cat("Second hitter: ")
          bat2 <- readLines(con = stdin(), 1)
          
        }
        
        cat("Third hitter: ")
        bat3 <- readLines(con = stdin(), 1)
        
        while (is.null(sing$GetPlayers(name = bat3))) {
          
          cat("Third hitter: ")
          bat3 <- readLines(con = stdin(), 1)
          
        }
        
        cat("Cleanup hitter: ")
        bat4 <- readLines(con = stdin(), 1)
        
        while (is.null(sing$GetPlayers(name = bat4))) {
          
          cat("Cleanup hitter: ")
          bat4 <- readLines(con = stdin(), 1)
          
        }
        
        cat("Fifth hitter: ")
        bat5 <- readLines(con = stdin(), 1)
        
        while (is.null(sing$GetPlayers(name = bat5))) {
          
          cat("Fifth hitter: ")
          bat5 <- readLines(con = stdin(), 1)
          
        }
        
        cat("Sixth hitter: ")
        bat6 <- readLines(con = stdin(), 1)
        
        while (is.null(sing$GetPlayers(name = bat6))) {
          
          cat("Sixth hitter: ")
          bat6 <- readLines(con = stdin(), 1)
          
        }
        
        cat("Seventh hitter: ")
        bat7 <- readLines(con = stdin(), 1)
        
        while (is.null(sing$GetPlayers(name = bat7))) {
          
          cat("Seventh hitter: ")
          bat7 <- readLines(con = stdin(), 1)
          
        }
        
        cat("Eighth hitter: ")
        bat8 <- readLines(con = stdin(), 1)
        
        while (is.null(sing$GetPlayers(name = bat8))) {
          
          cat("Eighth hitter: ")
          bat8 <- readLines(con = stdin(), 1)
          
        }
        
        cat("Ninth hitter: ")
        bat9 <- readLines(con = stdin(), 1)
        
        while (is.null(sing$GetPlayers(name = bat9))) {
          
          cat("Ninth hitter: ")
          bat9 <- readLines(con = stdin(), 1)
          
        }
        
        # Assigns to already-created variable with lineup
        
        batters <- c(bat1, bat2, bat3, bat4, bat5, bat6, bat7, bat8, bat9)
        
      }
      
      # Asks if user wants a new pitcher
      
      cat("New pitcher? (TRUE or FALSE) " )
      new_pitcher <- as.logical(readLines(con = stdin(), 1))
      
      # Checks to make sure a logical value was actually imputed
      
      while (is.na(new_pitcher)) {
        
        cat("New pitcher? (TRUE or FALSE) ")
        new_pitcher <- as.logical(readLines(con = stdin(), 1))
        
      }
      
      # Gets a new pitcher if the user said so
      
      if (new_pitcher) {
        
        cat("Pitcher: ")
        pitcher <- readLines(con = stdin(), 1)
        
        # Does the same check as above to make sure the input is correct
        
        while (is.null(sing$GetPlayers(name = pitcher))) {
          
          cat("Pitcher: ")
          pitcher <- readLines(con = stdin(), 1)
          
        }
        
      }
      
      # Asks if user wants a new environment
      
      cat("New environment? (TRUE or FALSE) ")
      new_envmt <- as.logical(readLines(con = stdin(), 1))
      
      # Same logical check as above
      
      while (is.na(new_envmt)) {
        
        cat("New environment? (TRUE or FALSE) ")
        new_envmt <- as.logical(readLines(con = stdin(), 1))
        
      }
      
      # Creates a new environment (stadium, home team, temperature) if requested
      
      if (new_envmt) {
        
        cat("Stadium: ")
        stadium <- readLines(con = stdin(), 1)
        
        # Null check for stadium input
        
        while (is.null(sing$GetVenues(stadium.name = stadium)[[1]])) {
          
          cat("Stadium: ")
          stadium <- readLines(con = stdin(), 1)
          
        }
        
        cat("Home Team: ")
        home <- readLines(con = stdin(), 1)
        
        # Null check for stadium input
        
        while (is.null(sing$GetTeams(name = home)[[1]])) {
          
          cat("Stadium: ")
          stadium <- readLines(con = stdin(), 1)
          
        }
        
        cat("Temperature: ")
        temp <- as.numeric(readLines(con = stdin(), 1))
        
        # NA check for temperature input
        
        while (is.na(temp)) {
          
          cat("Temperature: ")
          temp <- as.numeric(readLines(con = stdin(), 1))
          
        }
        
      }
      
      # Asks if user wants a new state
      
      cat("New state? (TRUE or FALSE) ")
      new_state <- as.logical(readLines(con = stdin(), 1))
      
      # Repeated logical check
      
      while (is.na(new_state)) {
        
        cat("New state? (TRUE or FALSE) ")
        new_state <- as.logical(readLines(con = stdin(), 1))
        
      }
      
      # Gets inputs for new state
      
      if (new_state) {
        
        # Asks if the user wants to change the inning
        
        cat("New inning? (TRUE or FALSE) ")
        new_inning <- as.logical(readLines(con = stdin(), 1))
        
        while (is.na(new_inning)) {
          
          cat("New inning? (TRUE or FALSE) ")
          new_inning <- as.logical(readLines(con = stdin(), 1))
          
        }
        
        if (new_inning) {
          
          cat("Inning: ")
          state$inning <- as.integer(readLines(con = stdin(), 1))
          
          while (is.na(state$inning) | state$inning < 1) {
            
            cat("Inning: ")
            state$inning <- as.numeric(readLines(con = stdin(), 1))
            
          }
          
        }
        
        # Asks if the user wants to change the half of the inning
        
        cat("Top of the inning? (TRUE or FALSE) ")
        state$top <- as.logical(readLines(con = stdin(), 1))
        
        while (is.na(state$top)) {
          
          cat("Top of the inning? (TRUE or FALSE) ")
          state$top <- as.logical(readLines(con = stdin(), 1))
          
        }
        
        # Asks if the user wants to change the number of outs
        
        cat("Change outs? (TRUE or FALSE) ")
        new_outs <- as.logical(readLines(con = stdin(), 1))
        
        while (is.na(new_outs)) {
          
          cat("Change outs? (TRUE or FALSE) ")
          new_outs <- as.logical(readLines(con = stdin(), 1))
          
        }
        
        if (new_outs) {
          
          cat("Outs: ")
          state$outs <- as.integer(readLines(con = stdin(), 1))
          
          while (is.na(state$outs) | (state$outs < 0 | state$outs > 2)) {
            
            cat("Inning: ")
            state$outs <- as.integer(readLines(con = stdin(), 1))
            
          }
          
        }
        
        # Asks if the user wants to change the baserunner state
        
        cat("New baserunner state? (TRUE or FALSE) ")
        new_br <- as.logical(readLines(con = stdin(), 1))
        
        while (is.na(new_br)) {
          
          cat("New baserunner state? (TRUE or FALSE) ")
          new_br <- as.logical(readLines(con = stdin(), 1))
          
        }
        
        # Changes baserunner state
        
        if (new_br) {
          
          # Goes through each base to ask whether there's a runner
          
          cat("Runner on first? (TRUE or FALSE) ")
          state$on_1b <- as.logical(readLines(con = stdin(), 1))
          
          while (is.na(state$on_1b)) {
            
            cat("Runner on first? (TRUE or FALSE) ")
            state$on_1b <- as.logical(readLines(con = stdin(), 1))
            
          }
          
          cat("Runner on second? (TRUE or FALSE) ")
          state$on_2b <- as.logical(readLines(con = stdin(), 1))
          
          while (is.na(state$on_2b)) {
            
            cat("Runner on second? (TRUE or FALSE) ")
            state$on_2b <- as.logical(readLines(con = stdin(), 1))
            
          }
          
          cat("Runner on third? (TRUE or FALSE) ")
          state$on_3b <- as.logical(readLines(con = stdin(), 1))
          
          while (is.na(state$on_3b)) {
            
            cat("Runner on third? (TRUE or FALSE) ")
            state$on_3b <- as.logical(readLines(con = stdin(), 1))
            
          }
          
        }
        
      }
      
      # Asks where to start in the batting order
      
      cat("Where to start in the order? ")
      order_start <- as.integer(readLines(con = stdin(), 1))
      
      # More checks
      
      while (is.na(order_start) | (order_start < 1 | order_start > 9)) {
        
        cat("Where to start in the order? ")
        order_start <- as.integer(readLines(con = stdin(), 1))
        
      }
      
      info <- list(batters, pitcher, stadium, home, temp)
      
      markov_half_inning(order_start, info, state)
      
    }
    
  }
  
  # When not run in the R console
  
  else {
    
    # Asks for user input on matchup type
    
    cat("Run default matchup of Dodgers batting vs. Chris Paddack in the 
        bottom of the first inning? (TRUE or FALSE) ")
    default <- as.logical(readLines(file("stdin"), 1))
    
    # Checks to make sure a logical value was actually imputed
    
    while (is.na(default)) {
      
      cat("Run default matchup of Dodgers batting vs. Chris Paddack in the 
          bottom of the first inning? (TRUE or FALSE) ")
      default <- as.logical(readLines(file("stdin"), 1))
      
    }
    
    # Runs default matchup, starting at the top of the order
    
    if (default) {
      
      markov_half_inning(1, info)
      
    }
    
    else {
      
      # Asks whether user wants to input a new lineup
      
      cat("New lineup? (TRUE or FALSE) ")
      new_lineup <- as.logical(readLines(file("stdin"), 1))
      
      # Checks to make sure a logical value was actually imputed
      
      while (is.na(new_lineup)) {
        
        cat("New lineup? (TRUE or FALSE) ")
        new_lineup <- as.logical(readLines(file("stdin"), 1))
        
      }
      
      # Gets inputs for new lineup if user wants
      
      if (new_lineup) {
        
        cat("Leadoff hitter: ")
        bat1 <- readLines(file("stdin"), 1)
        
        # Checks to make sure a real player was imputed
        
        while (is.null(sing$GetPlayers(name = bat1))) {
          
          cat("Leadoff hitter: ")
          bat1 <- readLines(file("stdin"), 1)
          
        }
        
        cat("Second hitter: ")
        bat2 <- readLines(file("stdin"), 1)
        
        # Same check
        
        while (is.null(sing$GetPlayers(name = bat2))) {
          
          cat("Second hitter: ")
          bat2 <- readLines(file("stdin"), 1)
          
        }
        
        cat("Third hitter: ")
        bat3 <- readLines(file("stdin"), 1)
        
        while (is.null(sing$GetPlayers(name = bat3))) {
          
          cat("Third hitter: ")
          bat3 <- readLines(file("stdin"), 1)
          
        }
        
        cat("Cleanup hitter: ")
        bat4 <- readLines(file("stdin"), 1)
        
        while (is.null(sing$GetPlayers(name = bat4))) {
          
          cat("Cleanup hitter: ")
          bat4 <- readLines(file("stdin"), 1)
          
        }
        
        cat("Fifth hitter: ")
        bat5 <- readLines(file("stdin"), 1)
        
        while (is.null(sing$GetPlayers(name = bat5))) {
          
          cat("Fifth hitter: ")
          bat5 <- readLines(file("stdin"), 1)
          
        }
        
        cat("Sixth hitter: ")
        bat6 <- readLines(file("stdin"), 1)
        
        while (is.null(sing$GetPlayers(name = bat6))) {
          
          cat("Sixth hitter: ")
          bat6 <- readLines(file("stdin"), 1)
          
        }
        
        cat("Seventh hitter: ")
        bat7 <- readLines(file("stdin"), 1)
        
        while (is.null(sing$GetPlayers(name = bat7))) {
          
          cat("Seventh hitter: ")
          bat7 <- readLines(file("stdin"), 1)
          
        }
        
        cat("Eighth hitter: ")
        bat8 <- readLines(file("stdin"), 1)
        
        while (is.null(sing$GetPlayers(name = bat8))) {
          
          cat("Eighth hitter: ")
          bat8 <- readLines(file("stdin"), 1)
          
        }
        
        cat("Ninth hitter: ")
        bat9 <- readLines(file("stdin"), 1)
        
        while (is.null(sing$GetPlayers(name = bat9))) {
          
          cat("Ninth hitter: ")
          bat9 <- readLines(file("stdin"), 1)
          
        }
        
        # Assigns to already-created variable with lineup
        
        batters <- c(bat1, bat2, bat3, bat4, bat5, bat6, bat7, bat8, bat9)
        
      }
      
      # Asks if user wants a new pitcher
      
      cat("New pitcher? (TRUE or FALSE) " )
      new_pitcher <- as.logical(readLines(file("stdin"), 1))
      
      # Checks to make sure a logical value was actually imputed
      
      while (is.na(new_pitcher)) {
        
        cat("New pitcher? (TRUE or FALSE) ")
        new_pitcher <- as.logical(readLines(file("stdin"), 1))
        
      }
      
      # Gets a new pitcher if the user said so
      
      if (new_pitcher) {
        
        cat("Pitcher: ")
        pitcher <- readLines(file("stdin"), 1)
        
        # Does the same check as above to make sure the input is correct
        
        while (is.null(sing$GetPlayers(name = pitcher))) {
          
          cat("Pitcher: ")
          pitcher <- readLines(file("stdin"), 1)
          
        }
        
      }
      
      # Asks if user wants a new environment
      
      cat("New environment? (TRUE or FALSE) ")
      new_envmt <- as.logical(readLines(file("stdin"), 1))
      
      # Same logical check as above
      
      while (is.na(new_envmt)) {
        
        cat("New environment? (TRUE or FALSE) ")
        new_envmt <- as.logical(readLines(file("stdin"), 1))
        
      }
      
      # Creates a new environment (stadium, home team, temperature) if requested
      
      if (new_envmt) {
        
        cat("Stadium: ")
        stadium <- readLines(file("stdin"), 1)
        
        # Null check for stadium input
        
        while (is.null(sing$GetVenues(stadium.name = stadium)[[1]])) {
          
          cat("Stadium: ")
          stadium <- readLines(file("stdin"), 1)
          
        }
        
        cat("Home Team: ")
        home <- readLines(file("stdin"), 1)
        
        # Same check for home input
        
        while (is.null(sing$GetTeams(name = home)[[1]])) {
          
          cat("Home Team: ")
          home <- readLines(file("stdin"), 1)
          
        }
        
        cat("Temperature: ")
        temp <- as.numeric(readLines(file("stdin"), 1))
        
        # NA check for temperature input
        
        while (is.na(temp)) {
          
          cat("Temperature: ")
          temp <- as.numeric(readLines(file("stdin"), 1))
          
        }
        
      }
      
      # Asks if user wants a new state
      
      cat("New state? (TRUE or FALSE) ")
      new_state <- as.logical(readLines(file("stdin"), 1))
      
      # Repeated logical check
      
      while (is.na(new_state)) {
        
        cat("New state? (TRUE or FALSE) ")
        new_state <- as.logical(readLines(file("stdin"), 1))
        
      }
      
      # Gets inputs for new state
      
      if (new_state) {
        
        # Asks if the user wants to change the inning
        
        cat("New inning? (TRUE or FALSE) ")
        new_inning <- as.logical(readLines(file("stdin"), 1))
        
        while (is.na(new_inning)) {
          
          cat("New inning? (TRUE or FALSE) ")
          new_inning <- as.logical(readLines(file("stdin"), 1))
          
        }
        
        if (new_inning) {
          
          cat("Inning: ")
          state$inning <- as.integer(readLines(file("stdin"), 1))
          
          while (is.na(state$inning) | state$inning < 1) {
            
            cat("Inning: ")
            state$inning <- as.numeric(readLines(file("stdin"), 1))
            
          }
          
        }
        
        # Asks if the user wants to change the half of the inning
        
        cat("Top of the inning? (TRUE or FALSE) ")
        state$top <- as.logical(readLines(file("stdin"), 1))
        
        while (is.na(state$top)) {
          
          cat("Top of the inning? (TRUE or FALSE) ")
          state$top <- as.logical(readLines(file("stdin"), 1))
          
        }
        
        # Asks if the user wants to change the number of outs
        
        cat("Change outs? (TRUE or FALSE) ")
        new_outs <- as.logical(readLines(file("stdin"), 1))
        
        while (is.na(new_outs)) {
          
          cat("Change outs? (TRUE or FALSE) ")
          new_outs <- as.logical(readLines(file("stdin"), 1))
          
        }
        
        if (new_outs) {
          
          cat("Outs: ")
          state$outs <- as.integer(readLines(file("stdin"), 1))
          
          while (is.na(state$outs) | (state$outs < 0 | state$outs > 2)) {
            
            cat("Outs: ")
            state$outs <- as.integer(readLines(file("stdin"), 1))
            
          }
          
        }
        
        # Asks if the user wants to change the baserunner state
        
        cat("New baserunner state? (TRUE or FALSE) ")
        new_br <- as.logical(readLines(file("stdin"), 1))
        
        while (is.na(new_br)) {
          
          cat("New baserunner state? (TRUE or FALSE) ")
          new_br <- as.logical(readLines(file("stdin"), 1))
          
        }
        
        # Changes baserunner state
        
        if (new_br) {
          
          # Goes through each base to ask whether there's a runner
          
          cat("Runner on first? (TRUE or FALSE) ")
          state$on_1b <- as.logical(readLines(file("stdin"), 1))
          
          while (is.na(state$on_1b)) {
            
            cat("Runner on first? (TRUE or FALSE) ")
            state$on_1b <- as.logical(readLines(file("stdin"), 1))
            
          }
          
          cat("Runner on second? (TRUE or FALSE) ")
          state$on_2b <- as.logical(readLines(file("stdin"), 1))
          
          while (is.na(state$on_2b)) {
            
            cat("Runner on second? (TRUE or FALSE) ")
            state$on_2b <- as.logical(readLines(file("stdin"), 1))
            
          }
          
          cat("Runner on third? (TRUE or FALSE) ")
          state$on_3b <- as.logical(readLines(file("stdin"), 1))
          
          while (is.na(state$on_3b)) {
            
            cat("Runner on third? (TRUE or FALSE) ")
            state$on_3b <- as.logical(readLines(file("stdin"), 1))
            
          }
          
        }
        
      }
      
      # Asks where to start in the batting order
      
      cat("Where to start in the order? ")
      order_start <- as.integer(readLines(file("stdin"), 1))
      
      # More checks
      
      while (is.na(order_start) | (order_start < 1 | order_start > 9)) {
        
        cat("Where to start in the order? ")
        order_start <- as.integer(readLines(file("stdin"), 1))
        
      }
      
      info <- list(batters, pitcher, stadium, home, temp)
      
      markov_half_inning(order_start, info, state)
      
      }
     
    }
    
  }
  
main()

