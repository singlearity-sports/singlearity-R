# comment out file path when running test files

source(file='common.R')

# turning this into a function, with a default matchup

pa_pred_simple <- function(stadium = "Dodger Stadium", 
                           home = "Dodgers",
                           temp = 70,
                           batters = "Mookie Betts", 
                           pitchers = "Mike Clevinger", 
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
  
  # Checks to make sure we're not passing in too many arguments
  
  if (length(args) > 14) {
    return("Invalid number of arguments.")
  }
  
  # Runs the function as default when there are no arguments
  
  if (length(args) == 0) {
    return(pa_pred_simple())
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
