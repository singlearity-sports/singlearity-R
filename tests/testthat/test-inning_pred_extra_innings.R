context("Singlearity")
library(tidyverse)

# Moves back to main directory before running test

setwd('../..')
source('examples/inning_pred_extra_innings.R')

test_that("test-pa_pred_extras", {
  
  # Attempting to determine the best NYY reliever to face the Rays in extras
  # 4-3 Yankees, bottom 10, runner starting on second
  
  # Rays lineup
  
  home1 <- c('Austin Meadows', 'DH')
  home2 <- c('Brandon Lowe', 'LF')
  home3 <- c('Yandy Diaz', '3B')
  home4 <- c('Ji-Man Choi', '1B')
  home5 <- c('Willy Adames', 'SS')
  home6 <- c('Joey Wendle', '2B')
  home7 <- c('Manuel Margot', 'RF')
  home8 <- c('Kevin Kiermaier', 'CF')
  home9 <- c('Michael Perez', 'C')
  home_starter <- c('Tyler Glasnow', 'P')
  
  home_lineup <- as_tibble(rbind(home1, home2, home3, home4, home5, home6,
                                 home7, home8, home9, home_starter)) %>% 
    rename(name = V1, pos = V2) %>% 
    mutate(lineup = 1:10) %>% 
    select(lineup, name, pos)

  # Yankees lineup
  
  away1 <- c('LeMahieu', '2B')
  away2 <- c('Aaron Judge', 'RF')
  away3 <- c('Gleyber Torres', 'SS')
  away4 <- c('Giancarlo Stanton', 'DH')
  away5 <- c('Aaron Hicks', 'CF')
  away6 <- c('Luke Voit', '1B')
  away7 <- c('Gary Sanchez', 'C')
  away8 <- c('Urshela', '3B')
  away9 <- c('Miguel Andujar', 'LF')
  away_starter <- c('Gerrit Cole', 'P')
  
  away_lineup <- as_tibble(rbind(away1, away2, away3, away4, away5, away6,
                                 away7, away8, away9, away_starter)) %>% 
    rename(name = V1, pos = V2) %>% 
    mutate(lineup = 1:10) %>% 
    select(lineup, name, pos)
  
  # Pitchers to compare
  
  pitchers <- c('Aroldis Chapman', 'Chad Green', 'Adam Ottavino',
                'Luis Cessa', 'Brooks Kriske')
  
  # Running through the simulation and assigning to a variable

  sim_results <- inning_pred_extra_innings()
  
  # Doing some quick sanity checks to make sure that our numbers make sense
  
  # That the data is sorted correctly
  
  expect_gt(pull(slice(sim_results, 1), save_pct), 
            pull(slice(sim_results, 2), save_pct))
  
})
