source(file='common.R')
library(glue)

home_lineup_pos=c(
  LineupPos$new(player=sing$GetPlayers(name='Austin Hays')[[1]], position='CF'),
  LineupPos$new(player=sing$GetPlayers(name='Anthony Santander')[[1]], position='RF'),
  LineupPos$new(player=sing$GetPlayers(name='Jose Iglesias')[[1]], position='SS'),
  LineupPos$new(player=sing$GetPlayers(name='Rio Ruiz')[[1]], position='3B'),
  LineupPos$new(player=sing$GetPlayers(name='Hanser Alberto')[[1]], position='2B'),
  LineupPos$new(player=sing$GetPlayers(name='Renato Nunez')[[1]], position='1B'),
  LineupPos$new(player=sing$GetPlayers(name='Dwight Smith')[[1]], position='DH'),
  LineupPos$new(player=sing$GetPlayers(name='Pedro Severino')[[1]], position='C'),
  LineupPos$new(player=sing$GetPlayers(name='DJ Stewart')[[1]], position='CF'),
  LineupPos$new(player=sing$GetPlayers(name='Tommy Milone')[[1]], position='P')
)

visit_lineup_pos=c(
  LineupPos$new(player=sing$GetPlayers(name='Yandy Diaz')[[1]], position='3B'),
  LineupPos$new(player=sing$GetPlayers(name='Jose Martinez')[[1]], position='1B'),
  LineupPos$new(player=sing$GetPlayers(name='Yoshi Tsutsugo')[[1]], position='DH'),
  LineupPos$new(player=sing$GetPlayers(name='Hunter Renfroe')[[1]], position='RF'),
  LineupPos$new(player=sing$GetPlayers(name='Manuel Margot')[[1]], position='LF'),
  LineupPos$new(player=sing$GetPlayers(name='Mike Brosseau')[[1]], position='3B'),
  LineupPos$new(player=sing$GetPlayers(name='Willy Adames')[[1]], position='SS'),
  LineupPos$new(player=sing$GetPlayers(name='Kevin Kiermaier')[[1]], position='CF'),
  LineupPos$new(player=sing$GetPlayers(name='Mike Zunino')[[1]], position='C'),
  LineupPos$new(player=sing$GetPlayers(name='Tyler Glasnow')[[1]], position='P')
)

batter_list = c('Austin Hays', 'Anthony Santander', 'Jose Iglesias', 'Rio Ruiz', 'Hanser Alberto', 'Renato Nunez', 'Dwight Smith', 'Pedro Severino', 'DJ Stewart')

venue <- sing$GetVenues(stadium.name = 'Oriole Park')[[1]]
atmosphere <- Atmosphere$new(venue = venue, temperature = 70, home_team = sing$GetTeams(name = 'Orioles')[[1]])

bat_score_start<-2
fld_score_start<-3
pitch_number_start <- 0 #assume a fresh pitcher

# create dataframe to store results
results <- data.frame(matrix(nrow = 0, ncol = 5))
names(results) <- c("pitcher", "hitter", "save_pct", "loss_pct", "tie_pct")

j <- 0

find_best_reliever <- function(pitchers, sims) {
  for(i in 1:9) {
    for (pitcher in pitchers) {
      j = j + 1
      visit_lineup_pos[[10]] <- LineupPos$new(player=pitcher, position='P')
      visit_lineup <- Lineup$new(visit_lineup_pos)
      home_lineup <- Lineup$new(home_lineup_pos)
      pitch_number_start <- 0
      
      game <- Game$new(visit_lineup = visit_lineup, home_lineup = home_lineup, atmosphere = atmosphere)
      game_sim_results <- sing$GetGameSim(BodyGetGameSimGameSimPost$new(game = game, start_state = State$new(inning = 10, top = FALSE, on_1b = FALSE, on_2b = TRUE, on_3b = FALSE, 
                                                                                                             outs = 0, bat_score = bat_score_start, fld_score = fld_score_start,
                                                                                                             bat_lineup_order = i, pitch_number = pitch_number_start)), num.sims = sims)
      
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
      
      
      print(glue('Pitcher:', sprintf("%-20s", pitcher$full_name)  ,
                 'Save Percentage: {format(round(saves/sims*100, 1), nsmall = 1)}%  ',
                 'Loss Percentage: {format(round(losses/sims*100, 1), nsmall = 1)}%  ',
                 'Tie Percentage: {format(round(ties/sims*100, 1), nsmall = 1)}%  '))
      
      currPitcher = paste(pitcher$full_name, " (", pitcher$pitch_hand, ")", sep = "")
      currHitter = batter_list[i]
      
      currCombination <- c(currPitcher, currHitter)
      
      results <- rbind(results, currCombination)
      
      
      results$pitcher[j] <- currPitcher
      results$hitter[j] <- currHitter
      results$save_pct[j] <- saves/sims*100
      results$loss_pct[j] <- losses/sims*100
      results$tie_pct[j] <- ties/sims*100
      
    }
    
    
  }
  
  results <- results[,3:7]
  
  print("Printing results")
  
  print(results)
  
  return(results)
  
}

test_pitcher_list <- sing$GetPlayers(team.name = 'Rays', position = 'P', on.40 = TRUE)

myResults = find_best_reliever(test_pitcher_list, sims = 2000)

print(myResults)

myResults$save_pct <- as.numeric(myResults$save_pct)

ggplot(myResults, aes(pitcher, hitter, fill = save_pct)) + geom_tile() + scale_fill_distiller(palette = "Spectral") + 
  geom_text(aes(label = save_pct)) + theme(legend.position = "none", axis.text.x = element_text(angle = 90)) + labs(title = "Pitcher Save Probability", subtitle = "Assumes an extra inning game with a one run lead in the bottom of the 10th, no outs, man on 2nd, pitcher pitch_count of 0")


