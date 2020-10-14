# Head-to-head tests
# Ensuring that outcome likelihoods are feasible
library(singlearity)

sing <- GetSinglearityClient()

validate_two_outs <- function(results) {

  print('validating two outs')
  # Testing to make sure different events have near-zero probability
  # This function tests that a value is less than a given one

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
}

validate_empty_bases <- function(results) {

  print('validating empty bases')
  # fielder's choice

  expect_lt(results[1,]$fc_exp, 1e-03)

  # fielder's choice out

  expect_lt(results[1,]$fc_o_exp, 1e-03)

  # force out

  expect_lt(results[1,]$fo, 1e-04)

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

}


test_that("two_outs_simple", {
  # Using simple start-of-game matchup
  # Mookie Betts vs. Mike Clevinger, leading off the bottom of the 1st
  # At LAD, 70 degrees

  batters_list <- c('Mookie Betts')
  pitchers_list <- c('Mike Clevinger')
  state = State$new(outs=2, on_1b=TRUE, on_2b=TRUE, on_3b=TRUE)
  atmosphere = Atmosphere$new(venue = sing$GetVenues(stadium.name = 'Dodger Stadium')[[1]], temperature = 70, home_team = sing$GetTeams(name = 'Dodgers')[[1]])

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
  validate_two_outs(results)

})


test_that("two_outs_comprehensive", {
  candidate_batters <- sing$GetPlayers(team.name='Yankees', on.40=TRUE)[1:10]
  candidate_pitchers <- sing$GetPlayers(team.name='Red Sox', on.40=TRUE)[1:10]
  atmosphere = Atmosphere$new(venue = sing$GetVenues(stadium.name = 'Dodger Stadium')[[1]], temperature = 70, home_team = sing$GetTeams(name = 'Dodgers')[[1]])

  #two out test.  Do all baserunners
  matchups <- list()
  for (state_count in 0:7) 
  {  
    on_1b = (bitwAnd(state_count,1) != 0)
    on_2b = (bitwAnd(state_count,2) != 0) 
    on_3b = (bitwAnd(state_count,4) != 0) 
    state = State$new(outs=2, on_1b=on_1b, on_2b=on_2b, on_3b=on_3b)
    for (b in candidate_batters)
    {
     for (p in candidate_pitchers)
     {
      matchups <- append(matchups, Matchup$new(batter = b, pitcher = p, atmosphere = atmosphere, state = state))
     }
    }
  }
  results <- sing$GetPaSim(matchup = matchups) 
  #print(results)
  validate_two_outs(results)
})

test_that("bases_empty", {
  candidate_batters <- sing$GetPlayers(team.name='Yankees', on.40=TRUE)[1:10]
  candidate_pitchers <- sing$GetPlayers(team.name='Red Sox', on.40=TRUE)[1:10]
  atmosphere = Atmosphere$new(venue = sing$GetVenues(stadium.name = 'Dodger Stadium')[[1]], temperature = 70, home_team = sing$GetTeams(name = 'Dodgers')[[1]])

  matchups <- list()
  #bases empty test.  do all outs
  matchups <- list()
  for (outs in 0:2) 
  {
    state = State$new(outs=outs)
    for (b in candidate_batters)
    {
      for (p in candidate_pitchers)
      {
       matchups <- append(matchups, Matchup$new(batter = b, pitcher = p, atmosphere = atmosphere, state = state))
       }
    }
  }
  results <- sing$GetPaSim(matchup = matchups) 
  validate_empty_bases(results)
})


