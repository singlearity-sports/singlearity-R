context("Singlearity")

# Moves back to main directory before running test

setwd('../..')

# Head-to-head tests
# Ensuring that outcome likelihoods are feasible

test_that("hth_results", {
  
  # Using simple start-of-game matchup from pa_pred_simple.R
  # Mookie Betts vs. Chris Paddack, leading off the bottom of the 1st
  
  source('examples/pa_pred_simple.R')
  
  # Testing to make sure different events have zero probability
  # This function tests for near-equality, not absolute equality
  
  # double plays, overall
  
  expect_equal(results$dp_exp, 0)
  
  # fielder's choice
  
  expect_equal(results$fc_exp, 0)
  
  # fielder's choice out
  
  expect_equal(results$fc_o_exp, 0)
  
  # force out
  
  expect_equal(results$fo, 0)
  
  # double plays, ground balls
  
  expect_equal(results$gdp_exp, 0)
  
  # sacrifice flies, overall
  
  expect_equal(results$sf_exp, 0)
  
  # sacrifice fly double plays
  
  expect_equal(results$sf_dp_exp, 0)
  
  # sacrifice hits
  
  expect_equal(results$sh_exp, 0)
  
  # strikeout double plays
  
  expect_equal(results$so_dp_exp, 0)
  
  # triple plays
  
  expect_equal(results$tp_exp, 0)
  
})

# Game state tests
# Testing that impossible outcomes (DPs, SFs, etc.) are represented correctly