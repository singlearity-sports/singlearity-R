##########################################
# Make predictions for groups of batters vs groups of pitchers
##########################################

# comment out file path when running test files
source(file = 'common.R')

# turning this into a function, with a default matchup

pa_pred_multiplayer <- function(num_batters = 3,
                                num_pitchers = 3,
                                batters = c('Mookie Betts', 
                                            'Max Muncy', 
                                            'Cody Bellinger'),
                                pitchers = c('Mike Clevinger',
                                             'Chris Paddack',
                                             'Dinelson Lamet'),
                                stadium = "Dodger Stadium",
                                home = "Dodgers",
                                temp = 70,
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

# Creates function to accept command-line arguments and run prediction function
# To run in the command line: 
# R -f pa_pred_simple.R --args [ARGUMENTS HERE]

main <- function() {
  
  args <- commandArgs(trailingOnly = TRUE)
  
  # Runs the function as default when there are no arguments
  
  if (length(args) == 0) {
    return(pa_pred_multiplayer())
  }
  
  # Checks to make sure we're not passing in too many arguments
  # We know that the first two arguments are the number of batters and pitchers
  # And w/ stadium, home team, temperature, and state, there are 9 other possible args.
  
  if (length(args) > 9 + args[1] + args[2]) {
    return("Invalid number of arguments.")
  }

  # Changes the stadium
  
  stad <- args[1]
  
  if (length(args) == 1) {
    return(pa_pred_simple(stad))
  }
  
  # Changes the stadium and home team
  
  home <- args[2]
  
  if (length(args) == 2) {
    return(pa_pred_simple(stad, home))
  }
  
  # Changes stadium, home team, and temperature
  
  temperature <- as.numeric(args[3])
  
  if (length(args) == 3) {
    return(pa_pred_simple(stad, home, temperature))
  }
  
  # CHanges stadium, home team, temperature, and batter
  
  batter <- args[4]
  
  if (length(args) == 4) {
    return(pa_pred_simple(stad, home, temperature, batter))
  }
  
  # Changes stadium, home team, temperature, and batter/pitcher matchup
  
  pitcher <- args[5]
  
  if (length(args) == 5) {
    return(pa_pred_simple(stad, home, temperature, batter, pitcher))
  }
  
  # Changes everything: stadium, home team, temp., matchup, and game state
  
  new_state <- State$new(inning = as.numeric(args[6]), 
                         to = as.logical(args[7]), 
                         on_1b = as.logical(args[8]), 
                         on_2b = as.logical(args[9]),
                         on_3b = as.logical(args[10]), 
                         outs = as.numeric(args[11]), 
                         bat_score = as.numeric(args[12]), 
                         fld_score = as.numeric(args[13]), 
                         pitch_number = as.numeric(args[14]))
  
  return(pa_pred_simple(stad, home, temperature, batter, pitcher, new_state))
  
}

main()


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