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

main <- function() {
  
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) > 15) {
    return("Invalid number of arguments.")
  }
  
  if (length(args) == 0) {
    return(pa_pred_simple())
  }
  
  stad <- args[1]
  
  if (length(args) == 1) {
    return(pa_pred_simple(stad))
  }
  
  home <- args[2]
  
  if (length(args) == 2) {
    return(pa_pred_simple(stad, home))
  }
  
  temperature <- args[3]
  
  if (length(args) == 3) {
    return(pa_pred_simple(stad, home, temperature))
  }
  
  batter <- args[4]
  
  if (length(args) == 4) {
    return(pa_pred_simple(stad, home, temperature, batter))
  }
  
  pitcher <- args[5]
  
  if (length(args) == 5) {
    return(pa_pred_simple(stad, home, temperature, batter, pitcher))
  }
  
  state <- State$new(inning = args[6], 
                     to = args[7], 
                     on_1b = args[8], 
                     on_2b = args[9],
                     on_3b = args[10], 
                     outs = args[11], 
                     bat_score = args[12], 
                     fld_score = args[13],
                     bat_lineup_order = args[14], 
                     pitch_number = args[15])
  
}

main()
