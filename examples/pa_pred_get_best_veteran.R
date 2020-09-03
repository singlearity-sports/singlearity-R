###############################################################
# Let's try to sign a veteran LHH 1B or DH to platoon against the Yankees righty pitchers 
###############################################################

# comment out file path when running test files

source(file = 'common.R')

# makes this into a function

pa_pred_get_best_veteran <- function(num_pitchers = 2,
                                     num_pos = 2,
                                     num_side = 1,
                                     batter_pos = c("1B, DH"),
                                     batter_side = c("L"),
                                     age_min = 32,
                                     age_max = 45,
                                     pitchers = c("Gerrit Cole",
                                                  "Luis Severino"),
                                     stadium = "Yankee Stadium",
                                     home = "Yankees",
                                     temp = 70) {
  
  candidate_batters <- list()
  candidate_pitchers <- list()
  
  # loops through player side and position to add to list of batters
  
  for (pos in batter_pos) {
    for (side in batter_side) {
      candidate_batters <- append(candidate_batters, sing$GetPlayers(position = pos,
                                                                     bat.side = side,
                                                                     age.min = age_min,
                                                                     age.max = age_max,
                                                                     active = TRUE))
    }
  }
  
  # adds pitchers to list
  
  for (pitcher in pitchers)
  {
    candidate_pitchers <- append(candidate_pitchers, sing$GetPlayers(name=pitcher))
  }
  
  
  venue <- sing$GetVenues(stadium.name = stadium)[[1]]
  atmosphere <- Atmosphere$new(venue = venue, 
                               temperature = temp, 
                               home_team = sing$GetTeams(name = home)[[1]])
  
  
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
  return(results)
  
}

# Creates function to accept command-line arguments and run prediction function
# To run in the command line: 
# R -f pa_pred_get_best_veteran.R --args [ARGUMENTS HERE]

main <- function() {
  
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0) {
    return(pa_pred_get_best_veteran())
  }

  num_p <- as.numeric(args[1])
  num_pos <- as.numeric(args[2])
  num_sides <- as.numeric(args[3])
  
  # Checks to make sure we're not passing in too many arguments
  # We know that the first three arguments are number of pitchers, positions, and bat sides
  # And w/ stadium, home team, temperature, and state, there are 12 other possible args.
  # So if this doesn't add up, something's gone wrong
  
  if (length(args) > 14 + num_bat + num_p | length(args) == 1 | length(args) == 2) {
    return("Invalid number of arguments.")
  }
  
  # Changes the players by looping through arguments
  # These will be passed into the function
  
  batters_vec <- c()
  
  for (i in 1:num_bat) {
    batters_vec <- c(batters_vec, args[2 + i])
  }
  
  pitchers_vec <- c()
  
  for (j in 1:num_p) {
    pitchers_vec <- c(pitchers_vec, args[2 + num_bat + j])
  }
  
  # Runs function if only the players have changed
  
  if (length(args) == 2 + num_bat + num_p) {
    return(pa_pred_multiplayer(num_bat, num_p, batters_vec, pitchers_vec))
  }
  
  # Changes the stadium
  
  stad <- args[2 + num_bat + num_p + 1]
  
  if (length(args) == 2 + num_bat + num_p + 1) {
    return(pa_pred_multiplayer(num_bat, num_p, batters_vec, pitchers_vec, stad))
  }
  
  # Changes stadium and home team
  
  home <- args[2 + num_bat + num_p + 2]
  
  if (length(args) == 2 + num_bat + num_p + 2) {
    return(pa_pred_multiplayer(num_bat, num_p, batters_vec, pitchers_vec, stad, home))
  }
  
  # Changes stadium, home team, and temperature
  
  temperature <- as.numeric(args[2 + num_bat + num_p + 3])
  
  if (length(args) == 2 + num_bat + num_p + 3) {
    return(pa_pred_multiplayer(num_bat, num_p, batters_vec, pitchers_vec, 
                               stad, home, temperature))
  }
  
  # Changes everything: stadium, home team, temp., and game state
  
  new_state <- State$new(inning = as.numeric(args[2 + num_bat + num_p + 4]), 
                         to = as.logical(args[2 + num_bat + num_p + 5]), 
                         on_1b = as.logical(args[2 + num_bat + num_p + 6]), 
                         on_2b = as.logical(args[2 + num_bat + num_p + 7]),
                         on_3b = as.logical(args[2 + num_bat + num_p + 8]), 
                         outs = as.numeric(args[2 + num_bat + num_p + 9]), 
                         bat_score = as.numeric(args[2 + num_bat + num_p + 10]), 
                         fld_score = as.numeric(args[2 + num_bat + num_p + 11]), 
                         pitch_number = as.numeric(args[2 + num_bat + num_p + 12]))
  
  return(pa_pred_multiplayer(num_bat, num_p, batters_vec, pitchers_vec, 
                             stad, home, temperature, new_state))
  
}

main()
