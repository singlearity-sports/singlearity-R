# Test the different types of models like ab_woba, ab_outcome, ab_woba_no_state, and ab_log5
library(singlearity)

sing <- GetSinglearityClient()

test_that("alternate_models", {
  # Using simple start-of-game matchup
  # Mookie Betts vs. Mike Clevinger, leading off the bottom of the 1st
  # At LAD, 70 degrees

  batters_list <- c('Mookie Betts', 'Mike Trout')
  pitchers_list <- c('Clayton Kershaw', 'Gerrit Cole')
  state <- State$new()
  atmosphere = Atmosphere$new(venue = sing$GetVenues(stadium.name = 'Progressive Field')[[1]], temperature = 70, home_team = sing$GetTeams(name = 'Indians')[[1]])

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
  
  results_ab_outcome = sing$GetPaSim(matchup = matchups,  model.name = 'ab_outcome')
  for (model_type in c('ab_woba', 'ab_woba_no_state', 'ab_log5')) {
    results <- sing$GetPaSim(matchup = matchups, model.name = model_type) 
    for (i in 1:dim(results)[1]) {
        #validate that all models return results that are roughly the same
        expect_lt(abs(results[i,]$woba_exp - results_ab_outcome[i,]$woba_exp), 0.1)
    }
  }
})
