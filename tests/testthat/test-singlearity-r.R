context("Singlearity")

# Head-to-head tests
# Ensuring that outcome likelihoods are feasible

test_that("hth_results", {
  
  # Using simple start-of-game matchup from pa_pred_simple.R
  # Mookie Betts vs. Chris Paddack, leading off the bottom of the 1st
  
  source('../../examples/pa_pred_simple.R')
  
  # Testing to make sure different events have zero probability:
  
  # double plays
  
  expect_equal(results$dp_exp, 0)
  
  # fielder's choice
  
  expect_equal(results$fc_exp, 0)
  
})

# Game state tests
# Testing that impossible outcomes (DPs, SFs, etc.) are represented correctly