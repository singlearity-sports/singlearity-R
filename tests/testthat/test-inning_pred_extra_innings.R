context("Singlearity")
library(tidyverse)

# Moves back to main directory before running test

setwd('../..')
source('examples/inning_pred_extra_innings.R')

test_that("test-pa_pred_extras", {
  
  # Attempting to determine the best NYY reliever to face the Rays in extras
  # Using the default test settings
  # Running through the simulation and assigning to a variable

  sim_results <- inning_pred_extra_innings(1000)
  
  # Doing some quick sanity checks to make sure that our numbers make sense
  
  # That the data is sorted correctly
  
  expect_gt(pull(slice(sim_results, 1), save_pct), 
            pull(slice(sim_results, 2), save_pct))
  
})
