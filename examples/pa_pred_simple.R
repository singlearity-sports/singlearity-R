# hard-coded file path
# commented out when running tests because API work contained within helper-common.R

# source('~/Desktop/Everything/Singlearity/examples/common.R')
# source(file='common.R')

pa_pred_simple <- function(batters, pitchers, stadium, temp,
                           state = State$new()) {
  
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
  
  venue <- sing$GetVenues(stadium.name = 'Dodger Stadium')[[1]]
  atmosphere <- Atmosphere$new(venue = venue, temperature = 70, home_team = sing$GetTeams(name = 'Dodgers')[[1]])
  
  matchups <- list()
  for (b in candidate_batters) 
  {
    for (p in candidate_pitchers)
    {
      matchups <- append(matchups, Matchup$new(batter = b, pitcher = p, atmosphere = atmosphere, state = State$new()))
    }
  }
  
  results <- sing$GetPaSim(matchup = matchups)
  results = results[order(results$woba_exp, decreasing = TRUE), ]
  print(results)
  
}

#list of batters
batter_list = c('Mookie Betts')

#list of pitchers
pitcher_list = c('Chris Paddack')

#initialize empty lists
candidate_batters <- list()
candidate_pitchers <- list()

for (batter in batter_list)
{
  candidate_batters <- append(candidate_batters, sing$GetPlayers(name=batter))
}

for (pitcher in pitcher_list)
{
  candidate_pitchers <- append(candidate_pitchers, sing$GetPlayers(name=pitcher))
}

venue <- sing$GetVenues(stadium.name = 'Dodger Stadium')[[1]]
atmosphere <- Atmosphere$new(venue = venue, temperature = 70, home_team = sing$GetTeams(name = 'Dodgers')[[1]])

matchups <- list()
for (b in candidate_batters) 
{
  for (p in candidate_pitchers)
  {
    matchups <- append(matchups, Matchup$new(batter = b, pitcher = p, atmosphere = atmosphere, state = State$new()))
  }
}

results <- sing$GetPaSim(matchup = matchups)
results = results[order(results$woba_exp, decreasing = TRUE), ]
print(results)


