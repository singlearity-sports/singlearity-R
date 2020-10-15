library(singlearity)

sing <- GetSinglearityClient()


test_that("test-pitch_count", {
  
  # Now ensuring that SP do better in similar situation w/ fewer pitches
  # Bases empty, two out, Gerrit Cole vs. Xander Bogaerts
  # Comparing start of game T1 vs. T7 (100 pitches)
  
  batter <- sing$GetPlayers(name='Xander Bogaerts')[[1]]
  pitcher <- sing$GetPlayers(name='Gerrit Cole')[[1]]
  atmosphere = Atmosphere$new(venue = sing$GetVenues(stadium.name = 'Yankee Stadium')[[1]], temperature = 70, home_team = sing$GetTeams(name = 'Yankees')[[1]])

  state_t1 <- State$new(pitch_number = 0, inning = 1)
  state_t7 <- State$new(pitch_number = 100, inning = 7)

  matchups <- c(Matchup$new(batter = batter, pitcher = pitcher, atmosphere = atmosphere, state = state_t1))
  results_t1 <- sing$GetPaSim(matchup = matchups) 

  matchups <- c(Matchup$new(batter = batter, pitcher = pitcher, atmosphere = atmosphere, state = state_t7))
  results_t7 <- sing$GetPaSim(matchup = matchups) 
 
  # We want to make sure that there's higher offensive production after a long outing
  
  expect_gt(results_t7$ba_exp, results_t1$ba_exp)
  expect_gt(results_t7$obp_exp, results_t1$obp_exp)
  expect_gt(results_t7$slg_exp, results_t1$slg_exp)
  expect_gt(results_t7$woba_exp, results_t1$woba_exp)
  
})
