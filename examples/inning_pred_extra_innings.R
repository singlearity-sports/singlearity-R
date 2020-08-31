# hard-coded file path
# commented out when running tests because API work contained within helper-common.R

# source('~/Desktop/Everything/Singlearity/examples/common.R')
# source(file='common.R')

# Turning this into a function

library(tidyverse)

# The default is bringing in a new reliever to start extra innings

inning_pred_extra_innings <- function(home1, home2, home3, home4, home5,
                                      home6, home7, home8, home9, home_sp,
                                      away1, away2, away3, away4, away5,
                                      away6, away7, away8, away9, away_sp,
                                      pitcher_list, stadium, team_home, 
                                      bat_score_start, fld_score_start, bat_start, 
                                      num_sims = 1000, temp = 70, inning_start = 10, 
                                      top = FALSE, first = FALSE, second = TRUE, 
                                      third = FALSE, outs_start = 0, pitch_start = 0) {
    
    # Creates a function to create lineups
    # Position players passed in are in vector form
    # The first element is their name, the second element is their position
    # Pitchers passed in are a single string with their names
    
    home_lineup_pos = c(
        LineupPos$new(player = sing$GetPlayers(name = home1[1])[[1]], 
                      position = home1[2]),
        LineupPos$new(player = sing$GetPlayers(name = home2[1])[[1]], 
                      position = home2[2]),
        LineupPos$new(player = sing$GetPlayers(name = home3[1])[[1]], 
                      position = home3[2]),
        LineupPos$new(player = sing$GetPlayers(name = home4[1])[[1]], 
                      position = home4[2]),
        LineupPos$new(player = sing$GetPlayers(name = home5[1])[[1]], 
                      position = home5[2]),
        LineupPos$new(player = sing$GetPlayers(name = home6[1])[[1]], 
                      position = home6[2]),
        LineupPos$new(player = sing$GetPlayers(name = home7[1])[[1]], 
                      position = home7[2]),
        LineupPos$new(player = sing$GetPlayers(name = home8[1])[[1]], 
                      position = home8[2]),
        LineupPos$new(player = sing$GetPlayers(name = home9[1])[[1]], 
                      position = home9[2]),
        LineupPos$new(player = sing$GetPlayers(name = home_sp)[[1]], 
                      position = 'P')
    )

    visit_lineup_pos = c(
        LineupPos$new(player = sing$GetPlayers(name = away1[1])[[1]], 
                      position = away1[2]),
        LineupPos$new(player = sing$GetPlayers(name = away2[1])[[1]], 
                      position = away2[2]),
        LineupPos$new(player = sing$GetPlayers(name = away3[1])[[1]], 
                      position = away3[2]),
        LineupPos$new(player = sing$GetPlayers(name = away4[1])[[1]], 
                      position = away4[2]),
        LineupPos$new(player = sing$GetPlayers(name = away5[1])[[1]], 
                      position = away5[2]),
        LineupPos$new(player = sing$GetPlayers(name = away6[1])[[1]], 
                      position = away6[2]),
        LineupPos$new(player = sing$GetPlayers(name = away7[1])[[1]], 
                      position = away7[2]),
        LineupPos$new(player = sing$GetPlayers(name = away8[1])[[1]], 
                      position = away8[2]),
        LineupPos$new(player = sing$GetPlayers(name = away9[1])[[1]], 
                      position = away9[2]),
        LineupPos$new(player = sing$GetPlayers(name = away_sp)[[1]], 
                      position = 'P')
    )
    
    # Creates venue, atmosphere, and state from passed-in values as well
    # A bit messy but nothing too complex
    
    venue <- sing$GetVenues(stadium.name = stadium)[[1]]
    atmosphere <- Atmosphere$new(venue = venue, temperature = temp, 
                                 home_team = sing$GetTeams(name = team_home)[[1]])
    
    state <- State$new(inning = inning_start, to = top, 
                       on_1b = first, on_2b = second, on_3b = third, 
                       outs = outs_start, bat_score = bat_score_start, 
                       fld_score = fld_score_start, bat_lineup_order = bat_start, 
                       pitch_number = pitch_start)
    
    # Inner function to simulate through inning with selected pitcher
    
    find_best_reliever <- function(pitchers, sims) {
        
        # Creates tibble of pitchers and the results of their outings
        
        pitcher_results <- tibble(player = pitchers,
                                  wins = 0, losses = 0, ties = 0)
        
        # Loops through each of the pitchers to assess performance
        
        for (pitcher in pitchers) {

            # Splits it up by home and away
            
            if (top == FALSE) {
                visit_lineup_pos[[10]] <- 
                    LineupPos$new(player = sing$GetPlayers(name = pitcher)[[1]], 
                                  position = 'P')
                visit_lineup <- Lineup$new(visit_lineup_pos)
                home_lineup <- Lineup$new(home_lineup_pos)
                game <- Game$new(visit_lineup = visit_lineup, 
                                 home_lineup = home_lineup, 
                                 atmosphere = atmosphere)
                game_sim_results <- 
                    sing$GetGameSim(BodyGetGameSimGameSimPost$new(game = game,
                                                                  start_state = state), 
                                    num.sims = sims)
                
                # Goes through each of the simulated results to examine performance
                
                for (result in game_sim_results) {
                    if (result$away_score > result$home_score) {
                        pitcher_results <- pitcher_results %>% 
                            mutate(wins = ifelse(player == pitcher,
                                                 wins + 1,
                                                 wins))
                    } else if (result$away_score < result$home_score) {
                        pitcher_results <- pitcher_results %>% 
                            mutate(losses = ifelse(player == pitcher,
                                                   losses + 1,
                                                   losses))
                    } else if (result$away_score == result$home_score){
                        pitcher_results <- pitcher_results %>% 
                            mutate(ties = ifelse(player == pitcher,
                                                 ties + 1,
                                                 ties))
                    }
                }
                
            } else {
                
                home_lineup_pos[[10]] <- 
                    LineupPos$new(player = sing$GetPlayers(name = pitcher)[[1]], 
                                  position = 'P')
                visit_lineup <- Lineup$new(visit_lineup_pos)
                home_lineup <- Lineup$new(home_lineup_pos)
                game <- Game$new(visit_lineup = visit_lineup, 
                                 home_lineup = home_lineup, 
                                 atmosphere = atmosphere)
                game_sim_results <- 
                    sing$GetGameSim(BodyGetGameSimGameSimPost$new(game = game,
                                                                  start_state = state), 
                                    num.sims = sims)
                
                # Similar loop as before, now just seeing if the home team wins
                
                for (result in game_sim_results) {
                    
                    if (result$home_score > result$away_score) {
                        pitcher_results <- pitcher_results %>% 
                            mutate(wins = ifelse(player == pitcher,
                                                 wins + 1,
                                                 wins))
                    } else if (result$home_score < result$away_score) {
                        pitcher_results <- pitcher_results %>% 
                            mutate(losses = ifelse(player == pitcher,
                                                   losses + 1,
                                                   losses))
                    } else if (result$away_score == result$home_score){
                        pitcher_results <- pitcher_results %>% 
                            mutate(ties = ifelse(player == pitcher,
                                                 ties + 1,
                                                 ties))
                    }
                }
                
            }
        }
        
        return(pitcher_results)
        
    }
    
    # Gets results of simulations, converts to percentages, and sorts
    
    pitcher_results <- find_best_reliever(pitcher_list, num_sims) %>% 
        mutate(win_pct = wins / num_sims,
               loss_pct = losses / num_sims,
               tie_pct = ties / num_sims) %>% 
        select(player, win_pct, loss_pct, tie_pct) %>% 
        arrange(desc(win_pct))
    
    return(pitcher_results)
    
}

