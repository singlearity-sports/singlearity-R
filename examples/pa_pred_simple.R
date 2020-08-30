# hard-coded file path
# commented out when running tests because API work contained within helper-common.R

# source('~/Desktop/Everything/Singlearity/examples/common.R')
# source(file='common.R')

# turning this into a function

pa_pred_simple <- function(batters, pitchers, stadium, home, 
                           temp = 70, state = State$new()) {
  
  #initialize empty lists
  candidate_batters <- list()
  candidate_pitchers <- list()
  
  for (batter in batters)
  {
    candidate_batters <- append(candidate_batters, sing$GetPlayers(name=batter))
  }
  
  for (pitcher in pitchers)
  {
    candidate_pitchers <- append(candidate_pitchers, sing$GetPlayers(name=pitcher))
  }
  
  venue <- sing$GetVenues(stadium.name = stadium)[[1]]
  atmosphere <- Atmosphere$new(venue = venue, temperature = temp, home_team = sing$GetTeams(name = home)[[1]])
  
  matchups <- list()
  
  for (b in candidate_batters) 
  {
    for (p in candidate_pitchers)
    {
      matchups <- append(matchups, Matchup$new(batter = b, pitcher = p, atmosphere = atmosphere, state = state))
    }
  }
  
  results <- sing$GetPaSim(matchup = matchups)
  results = results[order(results$woba_exp, decreasing = TRUE), ]
  return(results)
  
}
