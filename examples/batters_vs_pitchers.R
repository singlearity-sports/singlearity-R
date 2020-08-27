# This code generates heat maps of expected wOBA and K% values for every possible
# matchup from a list of batters and pitchers. In this case, it creates graphs that
# could be used by the Padres to select a relief pitcher, or the Dodgers to select
# a pinch hitter.

# source('~/Desktop/Everything/Singlearity/examples/common.R')
source(file='common.R')

# load required packages
library(ggplot2)

batting_team = "Dodgers"
pitching_team = "Padres"


candidate_batters = sing$GetPlayers(team.name=batting_team, active = TRUE, on.40 = TRUE)
candidate_pitchers = sing$GetPlayers(team.name=pitching_team, position = "P", active = TRUE, on.40 = TRUE)

venue <- sing$GetVenues(stadium.name = 'Dodger Stadium')[[1]]
state <- State$new(pitch_number = 0, top = FALSE)
atmosphere <- Atmosphere$new(venue = venue, temperature = 70, home_team = sing$GetTeams(name = 'Dodgers')[[1]])


matchups <- list()
for (b in candidate_batters) 
{
  if (b$position == 'P')  #ignore pitchers batting
      next
  for (p in candidate_pitchers)
  {
    matchups <- append(matchups, Matchup$new(batter = b, pitcher = p, atmosphere = atmosphere, state = state))
  }
}

results <- sing$GetPaSim(matchup = matchups)

# Generate wOBA heatmap
results = results[order(results$woba_exp, decreasing = TRUE), ]
ggplot(results, aes(pitcher_name, batter_name, fill = woba_exp)) + geom_tile() + scale_fill_distiller(palette = "Spectral")  +
  geom_text(aes(label = round(woba_exp, 3)), size=2) +theme(legend.position = "none", axis.text=element_text(size=5)) + labs(title="Predicted wOBA by Batter vs. Pitcher", subtitle="(Assumes 1st inning.  Pitcher pitch_count = 0.  No outs.  Bases empty.)") + theme(axis.text.x  = element_text(angle=90), plot.title = element_text(size=10, face="bold"), plot.subtitle= element_text(size=8))

# Generate strikeout heatmap
results = results[order(results$so_exp, decreasing = TRUE), ]
ggplot(results, aes(pitcher_name, batter_name, fill = so_exp)) + geom_tile() + scale_fill_distiller(palette = "Spectral")  +
  geom_text(aes(label = round(so_exp, 3)), size=2) +theme(legend.position = "none", axis.text=element_text(size=5)) + labs(title="Predicted K% by Batter vs. Pitcher", subtitle="(Assumes 1st inning.  Pitcher pitch_count = 0.  No outs.  Bases empty.)") + theme(axis.text.x  = element_text(angle=90), plot.title = element_text(size=10, face="bold"), plot.subtitle= element_text(size=8))


print(results)
