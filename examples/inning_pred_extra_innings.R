# hard-coded file path
# commented out when running tests because API work contained within helper-common.R

source('examples/common.R')
# source(file='common.R')

# Turning this into a function

library(tidyverse)

# The default is bringing in a new reliever to start extra innings

inning_pred_extra_innings <- function(num_sims, 
                                      home_lineup = as_tibble(rbind(
                                          c('Austin Meadows', 'DH'),
                                          c('Brandon Lowe', 'LF'),
                                          c('Yandy Diaz', '3B'),
                                          c('Ji-Man Choi', '1B'),
                                          c('Willy Adames', 'SS'),
                                          c('Joey Wendle', '2B'),
                                          c('Manuel Margot', 'RF'),
                                          c('Kevin Kiermaier', 'CF'),
                                          c('Michael Perez', 'C'),
                                          c('Tyler Glasnow', 'P'))) %>% 
                                          rename(name = V1, pos = V2) %>% 
                                          mutate(lineup = 1:10) %>% 
                                          select(lineup, name, pos),
                                      visit_lineup = as_tibble(rbind(
                                          c('LeMahieu', '2B'),
                                          c('Aaron Judge', 'RF'),
                                          c('Gleyber Torres', 'SS'),
                                          c('Giancarlo Stanton', 'DH'),
                                          c('Aaron Hicks', 'CF'),
                                          c('Luke Voit', '1B'),
                                          c('Gary Sanchez', 'C'),
                                          c('Urshela', '3B'),
                                          c('Miguel Andujar', 'LF'),
                                          c('Gerrit Cole', 'P'))) %>% 
                                          rename(name = V1, pos = V2) %>% 
                                          mutate(lineup = 1:10) %>% 
                                          select(lineup, name, pos), 
                                      pitcher_list = c('Aroldis Chapman',
                                                       'Chad Green',
                                                       'Adam Ottavino',
                                                       'Luis Cessa',
                                                       'Brooks Kriske'), 
                                      stadium = 'Tropicana Field', 
                                      team_home = 'Rays', 
                                      temp = 70, 
                                      state = State$new(inning = 10, to = FALSE, 
                                                        on_1b = FALSE, on_2b = TRUE,
                                                        on_3b = FALSE, outs = 0, 
                                                        bat_score = 3, fld_score = 4,
                                                        bat_lineup_order = 1, 
                                                        pitch_number = 0)) {
    
    # Loops through dataframes of players to create lineups
    # Much more efficient than prior creation
    
    home_lineup_pos = c()
    visit_lineup_pos = c()
    
    for (i in 1:10) {
        home_lineup_pos = c(home_lineup_pos,
                            LineupPos$new(player = 
                                              sing$GetPlayers(name =
                                                                  home_lineup[i, 2][[1]])[[1]],
                                          position = home_lineup[i, 3][[1]]))
        visit_lineup_pos = c(visit_lineup_pos,
                             LineupPos$new(player = 
                                               sing$GetPlayers(name =
                                                                   visit_lineup[i, 2][[1]])[[1]],
                                          position = visit_lineup[i, 3][[1]]))
    }
    
    # Creates venue, atmosphere, and state from passed-in values as well
    # A bit messy but nothing too complex
    
    venue <- sing$GetVenues(stadium.name = stadium)[[1]]
    atmosphere <- Atmosphere$new(venue = venue, temperature = temp, 
                                 home_team = sing$GetTeams(name = team_home)[[1]])
    
    # Inner function to simulate through inning with selected pitcher
    
    find_best_reliever <- function(pitchers, sims) {
        
        # Loops through each of the pitchers to assess performance
        
        # Creates tibble of pitchers, with the results to be inputted later
        
        pitcher_results <- tibble()

        for (pitcher in pitchers) {

            # Splits it up by home and away
            
            if (state$top == FALSE) {
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
                
                pitcher_result <- tibble(player = pitcher,
                                         saves = 0, losses = 0, ties = 0)
            
                for (result in game_sim_results) {
                    if (result$away_score > result$home_score) {
                        pitcher_result <- pitcher_result %>% 
                            mutate(saves = saves + 1)
                    } else if (result$away_score < result$home_score) {
                        pitcher_result <- pitcher_result %>% 
                            mutate(losses = losses + 1)
                    } else if (result$away_score == result$home_score){
                        pitcher_result <- pitcher_result %>% 
                            mutate(ties = ties + 1)
                    }
                    
                }
                
                pitcher_results <- bind_rows(pitcher_results, pitcher_result)

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
                
                # Similar loop as before, now just for the home team
                
                pitcher_result <- tibble(player = pitcher,
                                         trailing = 0, holds = 0)

                for (result in game_sim_results) {
                    
                    if (result$home_score < result$away_score) {
                        pitcher_result <- pitcher_result %>% 
                            mutate(trailing = trailing + 1)
                    } else if (result$away_score == result$home_score){
                        pitcher_result <- pitcher_result %>% 
                            mutate(holds = holds + 1)
                    }
                }
                
                pitcher_results <- bind_rows(pitcher_results, pitcher_result)
                
            }
            
        }
        
        return(pitcher_results)
        
    }
    
    # Gets results of simulations, converts to percentages, and sorts
    
    pitcher_results <- find_best_reliever(pitcher_list, num_sims)
    
    if (state$top == FALSE) {
        pitcher_results <- pitcher_results %>% 
            mutate(save_pct = saves / num_sims,
                   loss_pct = losses / num_sims,
                   tie_pct = ties / num_sims) %>% 
            select(player, save_pct, loss_pct, tie_pct) %>% 
            arrange(desc(save_pct))
    } else {
        pitcher_results <- pitcher_results %>% 
            mutate(hold_pct = holds / num_sims,
                   trail_pct = trailing / num_sims) %>% 
            select(player, hold_pct, trail_pct) %>% 
            arrange(desc(hold_pct))
    }
    
    print(pitcher_results)
    
    return(pitcher_results)
    
}

tester <- inning_pred_extra_innings(1000)
