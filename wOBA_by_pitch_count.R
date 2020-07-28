source(file='common.R')

# load required packages
library(ggplot2)

#list of batters
batter_list = c('George Springer', 'Jose Altuve', 'Michael Brantley', 'Alex Bregman', 'Yuli Gurriel', 'Carlos Correa', 'Yordan Alvarez', 'Martin Maldonado', 'Josh Reddick')

#list of pitchers
pitcher_list = c('Max Scherzer')


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


venue <- sing$GetVenues(stadium.name = 'Minute Maid Park')[[1]]
atmosphere <- Atmosphere$new(venue = venue, temperature = 70, home_team = sing$GetTeams(name = 'Astros')[[1]])

j = 0
matchups <- list()
for (i in seq(0,105, by = 15)) {
  j = j + 1
  for (b in candidate_batters) 
  {
    for (p in candidate_pitchers)
    {
      matchups <- append(matchups, Matchup$new(batter = b, pitcher = p, atmosphere = atmosphere, state = State$new(pitch_number = i, inning = j, top = FALSE)))
    }
  }
}


results <- sing$GetPaSim(matchup = matchups)
results = results[order(results$woba_exp, decreasing = TRUE), ]
print(results)

ggplot(results, aes(p_pitch_no, woba_exp, color = batter_name)) + geom_smooth() + labs(title = "wOBA by pitch count")
ggplot(results, aes(p_pitch_no, so_exp, color = batter_name)) + geom_smooth() + labs(title = "K% by pitch count")