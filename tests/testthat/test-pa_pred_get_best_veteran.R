context("Singlearity")

# Moves back to main directory before running test

setwd('../..')

# Platoon tests
# Mostly simple to check that the implementation worked

test_that("test-pa_pred_get_best_veteran", {
  
  # Using players in example given 
  # Trying to find best LHH to play @ NYY vs. Gerrit Cole or Luis Severino
  
  source('examples/pa_pred_get_best_veteran.R')
  
  # Making sure that the proper number of matchups were evaluated
  
  expect_equal(nrow(results), length(candidate_batters) * length(candidate_pitchers))
  
  # Also double-checking that it did indeed return a proper wOBA ranking
  
  expect_gt(results[1,]$woba_exp, results[2,]$woba_exp)
  
})
