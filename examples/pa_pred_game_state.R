##########################################
# Demonstrate making plate appearance predictions based on a game state
##########################################

# source('~/Desktop/Everything/Singlearity/examples/common.R')
source(file = 'common.R')

#list of batters
batter_list = c('Juan Soto', 'Tim Anderson', 'Derek Dietrich', 'Khris Davis')

#list of pitchers
pitcher_list = c('Trevor Bauer', 'Max Scherzer')


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


venue <- sing$GetVenues(stadium.name = 'Great American Ball Park')[[1]]
state <- State$new(on_1b = FALSE, on_2b = TRUE, on_3b = FALSE, pitch_number = 50, inning = 4, bat_score = 3, fld_score = 5, top = FALSE)
atmosphere <- Atmosphere$new(venue = venue, temperature = 70, home_team = sing$GetTeams(name = 'Reds')[[1]])


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
print(results)