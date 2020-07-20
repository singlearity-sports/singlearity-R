library(singlearity)
key = Sys.getenv('SINGLEARITY_API_KEY')
if (nchar(key) == 0)
{
  stop("You need to set an API KEY in your environment.  Modify (or create) a .Renviron file with a line containing your API KEY, for instance:
       SINGLEARITY_API_KEY=myveryspecialkey")
}

sing = APIsApi$new()
sing$apiClient$apiKeys['SINGLEARITY_API_KEY'] = key
sing$apiClient$basePath='https://beta3.singlearity.com'


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

