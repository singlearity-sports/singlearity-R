context("Singlearity")
library(tidyverse)

# Moves back to main directory before running test

setwd('../..')
source('examples/inning_pred_extra_innings')

test_that("test-pa_pred_extras", {
  
  # Attempting to determine the best NYY reliever to face Juan Soto in extras
  # 1-0 Yankees, bottom 10, runner starting on second
  
  batters <- c('Juan Soto')
  pitchers <- c('Chad Green', 'Aroldis Chapman', 'Adam Ottavino', 'Luis Cessa',
                'Jonathan Loaisiga', 'Jonathan Holder', 'Ben Heller')
  stadium <- 'Nationals Park'
  home <- 'Nationals'
  state <- State$new(on_1b = F, on_2b = T, on_3b = F, 
                     pitch_number = 0, inning = 10, outs = 0, 
                     top = FALSE, bat_score = 0, fld_score = 1)
  
  results <- pa_pred_simple(batters, pitchers, stadium, home, state) %>% 
    arrange(woba_exp)
  
  print(results)
  
})
