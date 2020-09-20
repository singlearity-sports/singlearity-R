# comment out file path when running test files

source(file='common.R')

# turning this into a function, with a default matchup

pa_pred_simple <- function(batters = sing$GetPlayers(name="Mookie Betts"),
                           pitchers = sing$GetPlayers(name="Mike Cleveringer"),
                           state =  State$new(), 
                           atmosphere = Atmosphere$new(venue = sing$GetVenues(stadium.name = 'Dodger Stadium')[[1]], temperature = 70, home_team = sing$GetTeams(name = 'Dodgers')[[1]])
                          ) {
  
  #initialize empty lists
  candidate_batters <- list()
  candidate_pitchers <- list()
  matchups <- list()
  
  for (b in batters) 
  {
    for (p in pitchers)
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

if (sys.nframe() == 0) { #main function if run as stand-alone
  batters_list = c('Mookie Betts', 'Cody Bellinger')
  pitchers_list = c('Mike Clevinger', 'Chris Paddack')
  candidate_batters <- list()
  candidate_pitchers <- list()
  for (batter in batters_list)
  {
    candidate_batters <- append(candidate_batters, sing$GetPlayers(name=trimws(batter)))
  }
  
  for (pitcher in pitchers_list)
  {
    candidate_pitchers <- append(candidate_pitchers, sing$GetPlayers(name=trimws(pitcher)))
  }
  
  
  state <- State$new(inning = 4, 
                         top = FALSE,   #set top = FALSE for bottom of inning
                         on_1b = TRUE, 
                         on_2b = FALSE,
                         on_3b = TRUE, 
                         outs = 1, 
                         bat_score = 4,
                         fld_score = 3,
                         pitch_number = 55)
  venue <- sing$GetVenues(stadium.name = 'Dodger Stadium')[[1]]
  atmosphere <- Atmosphere$new(venue = venue, temperature = 70, home_team = sing$GetTeams(name = 'Dodgers')[[1]])
   
 
  results = pa_pred_simple(batters = candidate_batters,
                              pitchers = candidate_pitchers,
                              state = state,
                              atmosphere = atmosphere)
  print(results)
  
}


