###############################################################
# Let's try to sign a veteran LHH 1B or DH to platoon against the Yankees righty pitchers 
###############################################################

# comment out file path when running test files

source(file = 'common.R')

# makes this into a function

pa_pred_get_best_veteran <- function(num_pitchers = 2,
                                     num_pos = 2,
                                     num_side = 1,
                                     batter_pos = c("1B", "DH"),
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
  
  # Adds possible matchups to list
  
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
  # And w/ stadium, home team, temperature, and age min/max, there are 5 other possible args.
  # So if this doesn't add up, something's gone wrong
  
  if (length(args) > 8 + num_p + num_pos + num_sides | length(args) == 1 |
      length(args) == 2 | length(args) == 3) {
    return("Invalid number of arguments.")
  }
  
  # Changes the players by looping through arguments
  # These will be passed into the function
  
  batters_pos <- c()
  
  for (i in 1:num_pos) {
    batters_pos <- c(batters_pos, args[3 + i])
  }

  batters_sides <- c()
  
  for (j in 1:num_sides) {
    batters_sides <- c(batters_sides, args[3 + num_pos + j])
  }

  # Runs function if just the player positions and batting sides have changed
  
  if (length(args) == 3 + num_pos + num_sides) {
    return(pa_pred_get_best_veteran(num_p, num_pos, num_sides, 
                                    batters_pos, batters_sides))
  }
  
  # Runs function w/ changed age min/max
  
  min_age <- as.numeric(args[3 + num_pos + num_sides + 1])
  max_age <- as.numeric(args[3 + num_pos + num_sides + 2])
  
  if (length(args) == 3 + num_pos + num_sides + 2) {
    return(pa_pred_get_best_veteran(num_p, num_pos, num_sides,
                                    batters_pos, batters_sides, min_age, max_age))
  }
  
  # Runs function w/ new pitchers
  
  pitcher_vector <- c()
  
  for (i in 1:num_p) {
    pitcher_vector <- c(pitcher_vector, args[3 + num_pos + num_sides + 2 + i])
  }

  if (length(args) == 3 + num_pos + num_sides + 2 + num_p) {
    return(pa_pred_get_best_veteran(num_p, num_pos, num_sides,
                                    batters_pos, batters_sides, 
                                    min_age, max_age, pitcher_vector))
  }
  
  # Changes the stadium
  
  stad <- args[3 + num_pos + num_sides + 2 + num_p + 1]
  
  if (length(args) == 3 + num_bat + num_sides + 2 + num_p + 1) {
    return(pa_pred_get_best_veteran(num_p, num_pos, num_sides,
                                    batters_pos, batters_sides, 
                                    min_age, max_age, pitcher_vector, stad))
  } 
  
  # Changes stadium and home team
  
  home <- args[3 + num_pos + num_sides + 2 + num_p + 2]
  
  if (length(args) == 3 + num_bat + num_sides + 2 + num_p + 2) {
    return(pa_pred_get_best_veteran(num_p, num_pos, num_sides,
                                    batters_pos, batters_sides, 
                                    min_age, max_age, pitcher_vector, stad, home))
  }
  
  # Changes stadium, home team, and temperature
  
  temperature <- as.numeric(args[3 + num_pos + num_sides + 2 + num_p + 3])
  
  return(pa_pred_get_best_veteran(num_p, num_pos, num_sides,
                                  batters_pos, batters_sides, 
                                  min_age, max_age, pitcher_vector, 
                                  stad, home, temperature))
  
}

main()
