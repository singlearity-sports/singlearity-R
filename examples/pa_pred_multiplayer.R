##########################################
# Make predictions for groups of batters vs groups of pitchers
##########################################

source(file = 'common.R')

#list of batters
batter_list = c('Mookie Betts', 'Justin Turner', 'Max Muncy', 'Cody Bellinger')

#list of pitchers
pitcher_list = c('Chris Paddack', 'Emilio Pagan')

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