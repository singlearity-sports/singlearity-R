# This code generates heat maps of expected wOBA and K% values for every possible
# matchup from a list of batters and pitchers. In this case, it creates graphs that
# could be used by the Giants to select a relief pitcher, or the Dodgers to select
# a pinch hitter.

source(file='common.R')

# load required packages
library(ggplot2)

batting_team = "Dodgers"


candidate_batters = sing$GetPlayers("Max Muncy")
candidate_batters <- append(candidate_batters, sing$GetPlayers("Mookie Betts"))
candidate_batters <- append(candidate_batters, sing$GetPlayers("Cody Bellinger"))
candidate_batters <- append(candidate_batters, sing$GetPlayers("Justin Turner"))
candidate_batters <- append(candidate_batters, sing$GetPlayers("Corey Seager"))
candidate_batters <- append(candidate_batters, sing$GetPlayers("Enrique Hernandez"))
candidate_batters <- append(candidate_batters, sing$GetPlayers("Joc Pederson"))
candidate_batters <- append(candidate_batters, sing$GetPlayers("Pollock"))
candidate_batters <- append(candidate_batters, sing$GetPlayers("Austin Barnes"))
candidate_batters <- append(candidate_batters, sing$GetPlayers("Terrance Gore"))
candidate_batters <- append(candidate_batters, sing$GetPlayers("Chris Taylor"))
candidate_batters <- append(candidate_batters, sing$GetPlayers("Will Smith", team.name = batting_team))
candidate_batters <- append(candidate_batters, sing$GetPlayers("Matt Beaty"))


pitching_team = "Giants"
candidate_pitchers = sing$GetPlayers(team.name=pitching_team, position = "P", active = TRUE)
candidate_pitchers <- candidate_pitchers[5:17]


venue <- sing$GetVenues(stadium.name = 'Dodger Stadium')[[1]]
state <- State$new(pitch_number = 0, top = FALSE)
atmosphere <- Atmosphere$new(venue = venue, temperature = 70, home_team = sing$GetTeams(name = 'Dodgers')[[1]])


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


# Generate wOBA heatmap
ggplot(results, aes(pitcher_name, batter_name, fill = woba_exp)) + geom_tile() + scale_fill_distiller(palette = "Spectral") + 
  geom_text(aes(label = round(woba_exp, 3))) + theme(legend.position = "none") + labs(title = "wOBA by Batter and Pitcher")

# Generate strikeout heatmap
ggplot(results, aes(pitcher_name, batter_name, fill = so_exp)) + geom_tile() + scale_fill_distiller(palette = "Spectral") + 
  geom_text(aes(label = round(so_exp, 3)*100)) + labs(title = "K% by Batter and Pitcher")
