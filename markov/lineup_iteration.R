# Iterating over matrix combinations

library(singlearity)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(baseballr))
suppressPackageStartupMessages(library(lubridate))
library(combinat)

# Assumes user is in the overarching "Singlearity" directory 

source(file = "markov/markov.R")
source(file = "markov/get_core_data.R")
sing <- GetSinglearityClient()

# Uses baseballr functions to get info
# Here, we're looking at the NL in the 2019 All-Star Game

game_id <- 567633

batting_info <- get_batting_orders(game_id) %>% 
  filter(team == "away") 

lineup <- batting_info %>% 
  arrange(batting_order) %>%
  select(id) %>% 
  pull()

pitcher <- sing$GetPlayers(name = "Justin Verlander")[[1]]$mlb_id

game_info <- get_game_info_mlb(567633)

stadium <- game_info %>% 
  select(venue_name) %>% 
  pull()

# Using Cleveland as the default home team, as game's at Progressive Field

name <- "Indians"

temperature <- game_info %>% 
  select(temperature) %>% 
  pull() %>% 
  as.numeric()

date <- game_info %>% 
  select(game_date) %>% 
  pull()

# Gets transition matrices

tmatrices <- markov_matrices(standard = FALSE,
                             state = State$new(top = TRUE),
                             lineup = as.list(lineup),
                             pitcher = pitcher,
                             stadium = stadium,
                             home = name,
                             temp = temperature,
                             date = date)

# Gets permutations of this list

tmatrices_perm <- permn(tmatrices)

# Tibble into which we'll collect

results_perm <- tibble(player = list(),
                       exp_runs = numeric())

tracker <- 0

for (i in 1:length(tmatrices_perm)) {
  
  tracker <- tracker + 1
  
  if (tracker %% 100 == 0) {print(tracker)}
  
  matrices <- tmatrices_perm[[i]]
  
  runs <- markov_half_inning(idx = 1,
                             tmatrix_list = matrices,
                             state = State$new(top = T)) %>% 
    pluck(1)
  
  results_perm <- results_perm %>% 
    add_row(player = matrices, exp_runs = runs)
  
}

