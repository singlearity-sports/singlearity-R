context("Singlearity")

# Moves back to main directory before running test

setwd('../..')

# Head-to-head tests
# Ensuring that outcome likelihoods are feasible

test_that("test-pa_pred_simple", {
  
  # Using simple start-of-game matchup
  # Mookie Betts vs. Chris Paddack, leading off the bottom of the 1st
  
  source('examples/pa_pred_simple.R')
  
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
