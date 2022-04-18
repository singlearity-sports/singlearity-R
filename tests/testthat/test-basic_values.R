# Head-to-head tests
# Ensuring that outcome likelihoods are feasible
library(singlearity)

sing <- GetSinglearityClient()

test_that("basic_values", {
  # Using simple start-of-game matchup
  # Mookie Betts vs. Mike Clevinger, leading off the bottom of the 1st
  # At LAD, 70 degrees

  batters_list <- c('Mookie Betts', 'Nelson Cruz', 'Brandon Crawford')
  pitchers_list <- c('Mike Clevinger', 'Sonny Gray')
  state <- State$new(on_1b = TRUE, on_2b = TRUE, on_3b = TRUE,
                     pitch_number = 20, inning = 1, outs = 0,
                     top = TRUE, bat_score = 0, fld_score = 0)
  atmosphere = Atmosphere$new(venue = sing$GetVenues(stadium.name = 'Progressive Field')[[1]], temperature = 70, home_team = sing$GetTeams(name = 'Cleveland')[[1]])

  matchups <- list()
  candidate_batters <- list()
  candidate_pitchers <- list()
  for (batter in batters_list)
  {
    candidate_batters <- append(candidate_batters, sing$GetPlayers(name=trimws(batter)))
  }
  
  for (pitcher in pitchers_list)
  {
    candidate_pitchers <- append(candidate_pitchers, sing$GetPlayers(name=trimws(pitcher)))
  }

  for (b in candidate_batters)
  {
    for (p in candidate_pitchers)
    {
      matchups <- append(matchups, Matchup$new(batter = b, pitcher = p, atmosphere = atmosphere, state = state))
    }
  }
  
  results <- sing$GetPaSim(matchup = matchups) 
  
  # Now we test to look at the probabilities of some other notable events
  # These are meant to see whether common events have realistic probabilities
  
  # We expect walk rate to be between 0.1% and 75%
  
  expect_gt(results[1,]$bb_exp, 1e-03)
  expect_lt(results[1,]$bb_exp, 3/4)
  
  # We expect strikeout rate to be in the same vicinity
  
  expect_gt(results[1,]$so_exp, 1e-03)
  expect_lt(results[1,]$so_exp, 3/4)
  
  # We expect HR rate to be significantly lower: 1% to 15%
  
  expect_gt(results[1,]$hr_exp, .01)
  expect_lt(results[1,]$hr_exp, 1/3)
  

  # We also expect there to be essentially no chance of an intentional walk
  
  expect_lt(results[1,]$ibb_exp, .01)
  
  # A triple play is possible, so we test that probability is not equal to zero
  
  expect_gt(results[1,]$tp_exp, 1e-7)
  expect_lt(results[1,]$tp_exp, .01)


})
