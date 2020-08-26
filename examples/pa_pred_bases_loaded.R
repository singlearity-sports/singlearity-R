###############################################################
# Find the pitcher least likely to let in the winning run in a tie game 
# against Aaron Judge in the 9th with two outs and the bases loaded
###############################################################

# hard-coded file path
source('~/Desktop/Everything/Singlearity/examples/common.R')
# source(file = 'common.R')

batter = sing$GetPlayers(name = 'Aaron Judge')[[1]]
pitching_team = "Rays"
candidate_pitchers = sing$GetPlayers(team.name=pitching_team, position = "P", active = TRUE)

venue = sing$GetVenues(stadium.name = 'Yankee Stadium')[[1]]
atmosphere = Atmosphere$new(venue = venue, temperature = 70, home_team = sing$GetTeams(name = 'Yankees')[[1]])
state <- State$new(on_1b = TRUE, on_2b = TRUE, on_3b = TRUE, pitch_number = 0, inning = 9, outs = 2, top = FALSE, bat_score = 3, fld_score = 3)



matchups <- list()
for (p in candidate_pitchers)
{
  matchups <- append(matchups, Matchup$new(batter = batter, pitcher = p, atmosphere = atmosphere, state = state))
}

results <- sing$GetPaSim(matchup = matchups)
results = results[order(results$obp_exp, decreasing = FALSE), ]
print(results)
