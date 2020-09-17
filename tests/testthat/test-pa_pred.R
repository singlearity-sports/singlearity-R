context("Singlearity")

# Moves back to main directory before running test

setwd('../..')
source('examples/pa_pred_simple.R')

# Head-to-head tests
# Ensuring that outcome likelihoods are feasible

test_that("test-pa_pred_simple", {
  
  # Using simple start-of-game matchup
  # Mookie Betts vs. Mike Clevinger, leading off the bottom of the 1st
  # At LAD, 70 degrees

  results <- pa_pred_simple()
  
  # Testing to make sure different events have near-zero probability
  # This function tests that a value is less than a given one
  
  # double plays, overall
  
  expect_lt(results$dp_exp, 1e-04)
  
  # fielder's choice
  
  expect_lt(results$fc_exp, 1e-04)
  
  # fielder's choice out
  
  expect_lt(results$fc_o_exp, 1e-04)
  
  # force out
  
  expect_lt(results$fo, 1e-04)
  
  # double plays, ground balls
  
  expect_lt(results$gdp_exp, 1e-04)
  
  # sacrifice flies, overall
  
  expect_lt(results$sf_exp, 1e-04)
  
  # sacrifice fly double plays
  
  expect_lt(results$sf_dp_exp, 1e-04)
  
  # sacrifice hits
  
  expect_lt(results$sh_exp, 1e-04)
  
  # strikeout double plays
  
  expect_lt(results$so_dp_exp, 1e-04)
  
  # triple plays
  
  expect_lt(results$tp_exp, 1e-04)
  
})


test_that("test-pa_pred_bases_loaded", {
  
  # Changing the example to look at a more complex example
  # Nelson Cruz vs. Shane Bieber, in the top of the 1st
  # At CLE, 70 degrees. Bieber has already thrown 20 pitches
  
  batters <- c('Nelson Cruz')
  pitchers <- c('Shane Bieber')
  stadium <- 'Progressive Field'
  home <- 'Indians'
  state <- State$new(on_1b = TRUE, on_2b = TRUE, on_3b = TRUE, 
                     pitch_number = 20, inning = 1, outs = 0, 
                     top = TRUE, bat_score = 0, fld_score = 0)
  
  results <- pa_pred_simple(stadium, home, 70, batters, pitchers, state)
  
  # Now we test to look at the probabilities of some other notable events
  # These are meant to see whether common events have realistic probabilities
  
  # We expect walk rate to be between 0.1% and 75%
  
  expect_gt(results$bb_exp, 1e-03)
  expect_lt(results$bb_exp, 3/4)
  
  # We expect strikeout rate to be in the same vicinity
  
  expect_gt(results$so_exp, 1e-03)
  expect_lt(results$so_exp, 3/4)
  
  # We expect HR rate to be significantly lower: .01% to 33%
  
  expect_gt(results$hr_exp, 1e-04)
  expect_lt(results$hr_exp, 1/3)
  
  # We also expect there to be essentially no chance of an intentional walk
  
  expect_lt(results$ibb_exp, 1e-04)
  
  # A triple play is possible, so we test that probability is not equal to zero
  
  expect_false(isTRUE(all.equal(results$tp_exp, 0)))
  
})


test_that("test-pa_pred_pitcher_comparison", {
  
  # Now ensuring that SP do better in similar situation w/ fewer pitches
  # Bases empty, two out, Gerrit Cole vs. Xander Bogaerts
  # Comparing start of game T1 vs. T7 (100 pitches)
  
  batters <- c('Xander Bogaerts')
  pitchers <- c('Gerrit Cole')
  stadium <- 'Yankee Stadium'
  home <- 'Yankees'
  state_t1 <- State$new(on_1b = F, on_2b = F, on_3b = F, 
                        pitch_number = 0, inning = 1, outs = 0, 
                        top = TRUE, bat_score = 0, fld_score = 0)
  
  results_t1 <- pa_pred_simple(stadium, home, 70, batters, pitchers, state_t1)
  
  state_t7 <- State$new(on_1b = F, on_2b = F, on_3b = F, 
                        pitch_number = 100, inning = 7, outs = 0, 
                        top = TRUE, bat_score = 0, fld_score = 0)
  
  results_t7 <- pa_pred_simple(stadium, home, 70, batters, pitchers, state_t7)
  
  # We want to make sure that there's higher offensive production after a long outing
  
  expect_gt(results_t7$ba_exp, results_t1$ba_exp)
  expect_gt(results_t7$obp_exp, results_t1$obp_exp)
  expect_gt(results_t7$slg_exp, results_t1$slg_exp)
  expect_gt(results_t7$woba_exp, results_t1$woba_exp)
  
})

