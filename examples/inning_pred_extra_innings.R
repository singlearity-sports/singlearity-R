source(file='common.R')
library(glue)

home_lineup_pos=c(
    LineupPos$new(player=sing$GetPlayers(name='Mookie Betts')[[1]], position='CF'),
    LineupPos$new(player=sing$GetPlayers(name='Gavin Lux')[[1]], position='2B'),
    LineupPos$new(player=sing$GetPlayers(name='Max Muncy')[[1]], position='1B'),
    LineupPos$new(player=sing$GetPlayers(name='Justin Turner')[[1]], position='3B'),
    LineupPos$new(player=sing$GetPlayers(name='Cody Bellinger')[[1]], position='RF'),
    LineupPos$new(player=sing$GetPlayers(name='Corey Seager')[[1]], position='SS'),
    LineupPos$new(player=sing$GetPlayers(name='Pollock')[[1]], position='DH'),
    LineupPos$new(player=sing$GetPlayers(name='Joc Pederson')[[1]], position='LF'),
    LineupPos$new(player=sing$GetPlayers(name='Will Smith', position='C')[[1]], position='C'),
    LineupPos$new(player=sing$GetPlayers(name='Clayton Kershaw')[[1]], position='P')
)

visit_lineup_pos=c(
    LineupPos$new(player=sing$GetPlayers(name='Yastrzemski')[[1]], position='LF'),
    LineupPos$new(player=sing$GetPlayers(name='Brandon Belt')[[1]], position='1B'),
    LineupPos$new(player=sing$GetPlayers(name='Evan Longoria')[[1]], position='3B'),
    LineupPos$new(player=sing$GetPlayers(name='Alex Dickerson')[[1]], position='RF'),
    LineupPos$new(player=sing$GetPlayers(name='Brandon Crawford')[[1]], position='SS'),
    LineupPos$new(player=sing$GetPlayers(name='Mauricio Dubon')[[1]], position='2B'),
    LineupPos$new(player=sing$GetPlayers(name='Wilmer Flores')[[1]], position='DH'),
    LineupPos$new(player=sing$GetPlayers(name='Billy Hamilton')[[1]], position='CF'),
    LineupPos$new(player=sing$GetPlayers(name='Tyler Heineman')[[1]], position='C'),
    LineupPos$new(player=sing$GetPlayers(name='Johnny Cueto')[[1]], position='P')
)
venue <- sing$GetVenues(stadium.name = 'Dodger Stadium')[[1]]
atmosphere <- Atmosphere$new(venue = venue, temperature = 70, home_team = sing$GetTeams(name = 'Dodgers')[[1]])
    
bat_score_start<-2
fld_score_start<-3
bat_lineup_start<-7
pitch_number_start <- 0 #assume a fresh pitcher
state <- State$new(inning = 10, to = FALSE, on_1b = FALSE, on_2b = TRUE, on_3b = FALSE, 
                   outs = 0, bat_score = bat_score_start, fld_score = fld_score_start,
                   bat_lineup_order = bat_lineup_start, pitch_number = pitch_number_start)


find_best_reliever <- function(pitchers, sims) {
    for (pitcher in pitchers) {
        print(glue("Testing pitcher : {pitcher}"))
        visit_lineup_pos[[10]] <- LineupPos$new(player=sing$GetPlayers(name=pitcher)[[1]], position='P')
        visit_lineup <- Lineup$new(visit_lineup_pos)
        home_lineup <- Lineup$new(home_lineup_pos)
        game <- Game$new(visit_lineup = visit_lineup, home_lineup = home_lineup, atmosphere = atmosphere)
        game_sim_results <- sing$GetGameSim(BodyGetGameSimGameSimPost$new(game = game, start_state = state), num.sims = sims)
        #game_sim_results is an array of home_score and away_score.  Now calculate how many times each team won
        saves<-0
        losses<-0
        ties<-0
        for (result in game_sim_results) {
          if (result$away_score > result$home_score) {
            saves <- (saves + 1)
          } else if (result$away_score < result$home_score) {
            losses <- (losses + 1)
          } else if (result$away_score == result$home_score){
            ties <- (ties + 1)
          }
        }
   
        print(glue('Pitcher:', sprintf("%-20s", pitcher)  ,
              'Save Percentage: {format(round(saves/sims*100, 1), nsmall = 1)}%  ', 
              'Loss Percentage: {format(round(losses/sims*100, 1), nsmall = 1)}%  ',
              'Tie Percentage: {format(round(ties/sims*100, 1), nsmall = 1)}%  '))
    }
}

test_pitcher_list <- c('Tony Watson', 'Shaun Anderson', 'Trevor Gott', 'Jarlin Garcia', 'Wandy Peralta')
find_best_reliever(test_pitcher_list, sims = 2000)

