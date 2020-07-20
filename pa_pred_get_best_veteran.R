###############################################################
# Let's try to sign a veteran RH 1B or DH to platoon against the Yankees lefty pitchers 
###############################################################

source(file = 'common.R')

pitcher_list = c('J.A. Happ', 'James Paxton', 'Jordan Montgomery')

candidate_batters <- list()
candidate_pitchers <- list()

candidate_batters <- sing$GetPlayers(position = "1B", bat.side = "R", 
                                     age.min = 32, active = TRUE)

candidate_batters <- append(candidate_batters, sing$GetPlayers(position = "DH", bat.side = "R", 
                                                               age.min = 32, active = TRUE))

for (pitcher in pitcher_list)
{
  candidate_pitchers <- append(candidate_pitchers, sing$GetPlayers(name=pitcher))
}


venue <- sing$GetVenues(stadium.name = 'Yankee Stadium')[[1]]
atmosphere <- Atmosphere$new(venue = venue, temperature = 70, home_team = sing$GetTeams(name = 'Yankees')[[1]])


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