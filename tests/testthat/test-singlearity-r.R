context("Singlearity")

# Head-to-head tests
# Ensuring that outcome likelihoods are feasible

test_that("hth_results", {
  
  # Using simple start-of-game matchup from pa_pred_simple.R
  # Mookie Betts vs. Chris Paddack, leading off the bottom of the 1st
  
  source('../../examples/pa_pred_simple.R')
  
})

# Game state tests
# Testing that impossible outcomes (DPs, SFs, etc.) are represented correctly