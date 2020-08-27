context("Singlearity")

# Moves back to main directory before running test

setwd('../..')

# COMMENT HERE

test_that("test-pa_pred_bases_loaded", {
  
  # Two outs, bases loaded, 3-3 B9, Aaron Judge batting vs. TBR
  
  source('examples/pa_pred_bases_loaded.R')
  
  # Doing some more common-sense tests
  # Some are the same as before, adding in some ones like IBB
  # We're looking at the pitcher with the lowest predicted OBP allowed
  # Edit index of "results" to look at other pitchers 
  
  # intentional walks
  
  expect_lt(results[1,]$ibb_exp, 1e-04)
  
  # double plays, overall
  
  expect_lt(results[1,]$dp_exp, 1e-04)
  
  # double plays, ground balls
  
  expect_lt(results[1,]$gdp_exp, 1e-04)
  
  # sacrifice flies, overall
  
  expect_lt(results[1,]$sf_exp, 1e-04)
  
  # sacrifice fly double plays
  
  expect_lt(results[1,]$sf_dp_exp, 1e-04)
  
  # sacrifice hits
  
  expect_lt(results[1,]$sh_exp, 1e-04)
  
  # strikeout double plays
  
  expect_lt(results[1,]$so_dp_exp, 1e-04)
  
  # triple plays
  
  expect_lt(results[1,]$tp_exp, 1e-04)
  
})
