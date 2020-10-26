# Collecting experimental results

library(singlearity)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(baseballr))
suppressPackageStartupMessages(library(lubridate))

# Assumes user is in the overarching "Singlearity" directory 

source(file = "markov/markov.R")
source(file = "R/get_singlearity_client.R")
sing <- GetSinglearityClient()

# Loads in the different relevant data files
# All data is limited to first innings and PA-ending pitches
# Uses baseballr to get important info like runs to end of inning as well
# We then merge them all into one file

pbp_2020_first <- read_csv("markov/data/pbp_2020_first.csv") %>% 
  mutate(release_speed = as.numeric(release_speed),
         release_pos_x = as.numeric(release_pos_x),
         release_pos_y = as.numeric(release_pos_y),
         release_pos_z = as.numeric(release_pos_z),
         zone = as.numeric(zone),
         pfx_x = as.numeric(pfx_x),
         pfx_z = as.numeric(pfx_z),
         plate_x = as.numeric(plate_x),
         plate_z = as.numeric(plate_z),
         vx0 = as.numeric(vx0),
         vy0 = as.numeric(vy0),
         vz0 = as.numeric(vz0),
         ax = as.numeric(ax),
         ay = as.numeric(ay),
         az = as.numeric(az),
         sz_top = as.numeric(sz_top),
         sz_bot = as.numeric(sz_bot),
         effective_speed = as.numeric(effective_speed),
         release_spin_rate = as.numeric(release_spin_rate),
         release_extension = as.numeric(release_extension),
         fielder_2 = as.numeric(fielder_2),
         fielder_3 = as.numeric(fielder_3),
         fielder_4 = as.numeric(fielder_4),
         fielder_5 = as.numeric(fielder_5),
         fielder_6 = as.numeric(fielder_6),
         fielder_7 = as.numeric(fielder_7),
         fielder_8 = as.numeric(fielder_8),
         fielder_9 = as.numeric(fielder_9)) %>% 
  select(-fielder_2_1) %>% 
  mutate(on_3b = na_if(on_3b, "null"),
         on_2b = na_if(on_2b, "null"),
         on_1b = na_if(on_1b, "null")) %>% 
  run_expectancy_code

pbp_2019_first <- read_csv("markov/data/pbp_2019_first.csv") %>% 
  mutate(release_speed = as.numeric(release_speed),
         release_pos_x = as.numeric(release_pos_x),
         release_pos_y = as.numeric(release_pos_y),
         release_pos_z = as.numeric(release_pos_z),
         zone = as.numeric(zone),
         pfx_x = as.numeric(pfx_x),
         pfx_z = as.numeric(pfx_z),
         plate_x = as.numeric(plate_x),
         plate_z = as.numeric(plate_z),
         vx0 = as.numeric(vx0),
         vy0 = as.numeric(vy0),
         vz0 = as.numeric(vz0),
         ax = as.numeric(ax),
         ay = as.numeric(ay),
         az = as.numeric(az),
         sz_top = as.numeric(sz_top),
         sz_bot = as.numeric(sz_bot),
         effective_speed = as.numeric(effective_speed),
         release_spin_rate = as.numeric(release_spin_rate),
         release_extension = as.numeric(release_extension),
         fielder_2 = as.numeric(fielder_2),
         fielder_3 = as.numeric(fielder_3),
         fielder_4 = as.numeric(fielder_4),
         fielder_5 = as.numeric(fielder_5),
         fielder_6 = as.numeric(fielder_6),
         fielder_7 = as.numeric(fielder_7),
         fielder_8 = as.numeric(fielder_8),
         fielder_9 = as.numeric(fielder_9)) %>% 
  select(-fielder_2_1) %>% 
  mutate(on_3b = na_if(on_3b, "null"),
         on_2b = na_if(on_2b, "null"),
         on_1b = na_if(on_1b, "null")) %>% 
  run_expectancy_code

pbp_2018_first <- read_csv("markov/data/pbp_2018_first.csv") %>% 
  mutate(release_speed = as.numeric(release_speed),
         release_pos_x = as.numeric(release_pos_x),
         release_pos_y = as.numeric(release_pos_y),
         release_pos_z = as.numeric(release_pos_z),
         zone = as.numeric(zone),
         pfx_x = as.numeric(pfx_x),
         pfx_z = as.numeric(pfx_z),
         plate_x = as.numeric(plate_x),
         plate_z = as.numeric(plate_z),
         vx0 = as.numeric(vx0),
         vy0 = as.numeric(vy0),
         vz0 = as.numeric(vz0),
         ax = as.numeric(ax),
         ay = as.numeric(ay),
         az = as.numeric(az),
         sz_top = as.numeric(sz_top),
         sz_bot = as.numeric(sz_bot),
         effective_speed = as.numeric(effective_speed),
         release_spin_rate = as.numeric(release_spin_rate),
         release_extension = as.numeric(release_extension),
         fielder_2 = as.numeric(fielder_2),
         fielder_3 = as.numeric(fielder_3),
         fielder_4 = as.numeric(fielder_4),
         fielder_5 = as.numeric(fielder_5),
         fielder_6 = as.numeric(fielder_6),
         fielder_7 = as.numeric(fielder_7),
         fielder_8 = as.numeric(fielder_8),
         fielder_9 = as.numeric(fielder_9)) %>% 
  select(-fielder_2_1) %>% 
  mutate(on_3b = na_if(on_3b, "null"),
         on_2b = na_if(on_2b, "null"),
         on_1b = na_if(on_1b, "null")) %>% 
  run_expectancy_code

pbp_2017_first <- read_csv("markov/data/pbp_2017_first.csv") %>% 
  mutate(release_speed = as.numeric(release_speed),
         release_pos_x = as.numeric(release_pos_x),
         release_pos_y = as.numeric(release_pos_y),
         release_pos_z = as.numeric(release_pos_z),
         zone = as.numeric(zone),
         pfx_x = as.numeric(pfx_x),
         pfx_z = as.numeric(pfx_z),
         plate_x = as.numeric(plate_x),
         plate_z = as.numeric(plate_z),
         vx0 = as.numeric(vx0),
         vy0 = as.numeric(vy0),
         vz0 = as.numeric(vz0),
         ax = as.numeric(ax),
         ay = as.numeric(ay),
         az = as.numeric(az),
         sz_top = as.numeric(sz_top),
         sz_bot = as.numeric(sz_bot),
         effective_speed = as.numeric(effective_speed),
         release_spin_rate = as.numeric(release_spin_rate),
         release_extension = as.numeric(release_extension),
         fielder_2 = as.numeric(fielder_2),
         fielder_3 = as.numeric(fielder_3),
         fielder_4 = as.numeric(fielder_4),
         fielder_5 = as.numeric(fielder_5),
         fielder_6 = as.numeric(fielder_6),
         fielder_7 = as.numeric(fielder_7),
         fielder_8 = as.numeric(fielder_8),
         fielder_9 = as.numeric(fielder_9)) %>% 
  select(-fielder_2_1) %>% 
  mutate(on_3b = na_if(on_3b, "null"),
         on_2b = na_if(on_2b, "null"),
         on_1b = na_if(on_1b, "null")) %>% 
  run_expectancy_code

pbp_2016_first <- read_csv("markov/data/pbp_2016_first.csv") %>% 
  mutate(release_speed = as.numeric(release_speed),
         release_pos_x = as.numeric(release_pos_x),
         release_pos_y = as.numeric(release_pos_y),
         release_pos_z = as.numeric(release_pos_z),
         zone = as.numeric(zone),
         pfx_x = as.numeric(pfx_x),
         pfx_z = as.numeric(pfx_z),
         plate_x = as.numeric(plate_x),
         plate_z = as.numeric(plate_z),
         vx0 = as.numeric(vx0),
         vy0 = as.numeric(vy0),
         vz0 = as.numeric(vz0),
         ax = as.numeric(ax),
         ay = as.numeric(ay),
         az = as.numeric(az),
         sz_top = as.numeric(sz_top),
         sz_bot = as.numeric(sz_bot),
         effective_speed = as.numeric(effective_speed),
         release_spin_rate = as.numeric(release_spin_rate),
         release_extension = as.numeric(release_extension),
         fielder_2 = as.numeric(fielder_2),
         fielder_3 = as.numeric(fielder_3),
         fielder_4 = as.numeric(fielder_4),
         fielder_5 = as.numeric(fielder_5),
         fielder_6 = as.numeric(fielder_6),
         fielder_7 = as.numeric(fielder_7),
         fielder_8 = as.numeric(fielder_8),
         fielder_9 = as.numeric(fielder_9)) %>% 
  select(-fielder_2_1) %>% 
  mutate(on_3b = na_if(on_3b, "null"),
         on_2b = na_if(on_2b, "null"),
         on_1b = na_if(on_1b, "null")) %>% 
  run_expectancy_code

pbp_2015_first <- read_csv("markov/data/pbp_2015_first.csv") %>% 
  mutate(release_speed = as.numeric(release_speed),
         release_pos_x = as.numeric(release_pos_x),
         release_pos_y = as.numeric(release_pos_y),
         release_pos_z = as.numeric(release_pos_z),
         zone = as.numeric(zone),
         pfx_x = as.numeric(pfx_x),
         pfx_z = as.numeric(pfx_z),
         plate_x = as.numeric(plate_x),
         plate_z = as.numeric(plate_z),
         vx0 = as.numeric(vx0),
         vy0 = as.numeric(vy0),
         vz0 = as.numeric(vz0),
         ax = as.numeric(ax),
         ay = as.numeric(ay),
         az = as.numeric(az),
         sz_top = as.numeric(sz_top),
         sz_bot = as.numeric(sz_bot),
         effective_speed = as.numeric(effective_speed),
         release_spin_rate = as.numeric(release_spin_rate),
         release_extension = as.numeric(release_extension),
         fielder_2 = as.numeric(fielder_2),
         fielder_3 = as.numeric(fielder_3),
         fielder_4 = as.numeric(fielder_4),
         fielder_5 = as.numeric(fielder_5),
         fielder_6 = as.numeric(fielder_6),
         fielder_7 = as.numeric(fielder_7),
         fielder_8 = as.numeric(fielder_8),
         fielder_9 = as.numeric(fielder_9)) %>% 
  select(-fielder_2_1) %>% 
  mutate(on_3b = na_if(on_3b, "null"),
         on_2b = na_if(on_2b, "null"),
         on_1b = na_if(on_1b, "null")) %>% 
  run_expectancy_code

pbp_first <- bind_rows(pbp_2020_first, pbp_2019_first, pbp_2018_first, 
                       pbp_2017_first, pbp_2016_first) %>% 
  arrange(desc(game_date))

# Creates 1st inning RE24 tables for each season

season_re24 <- function(pbp) {
  
  table <- pbp %>% 
    run_expectancy_table() %>% 
    mutate(on_1b = str_detect(base_out_state, "1b"),
           on_2b = str_detect(base_out_state, "2b"),
           on_3b = str_detect(base_out_state, "3b"),
           outs = case_when(
             str_detect(base_out_state, "0  outs") ~ 0,
             str_detect(base_out_state, "1  outs") ~ 1,
             str_detect(base_out_state, "2  outs") ~ 2
           )) %>% 
    select(outs, on_1b, on_2b, on_3b, base_out_state, avg_re)
  
  return(table)
  
}

# Uses above function to get 1st inning RE24 tables for each year

re24_2019_first <- season_re24(pbp_2019_first)
re24_2018_first <- season_re24(pbp_2018_first)
re24_2017_first <- season_re24(pbp_2017_first)
re24_2016_first <- season_re24(pbp_2016_first)
re24_2015_first <- season_re24(pbp_2015_first)

# Error tracker (first elmt. = Singlearity, second elmt. = standard)

error <- c(0, 0)
num_pa <- 0

# Function to get difference between Singlearity predictions and runs scored

inning_diff <- function(game_id) {
  
  # Gets batting orders using game ID
  
  lineups <- get_batting_orders(game_id)
  
  lineup_away <- lineups %>% 
    filter(team == "away") %>% 
    arrange(batting_order)
  
  lineup_home <- lineups %>% 
    filter(team == "home") %>% 
    arrange(batting_order)
  
  # Creates lineup using one call to markov_matrices(), including vars. like date
  
  game_info <- get_game_info_mlb(game_id)
  
  info_away <- list()
  
  # Creates list of player names
  
  info_away[[1]] <- as.list(lineup_away$fullName)
  
  # Gets pitcher name
  
  info_away[[2]] <- pbp_first %>% 
    filter(game_pk == game_id, inning_topbot == "Top") %>% 
    select(pitcher) %>% 
    pull() %>% 
    sing$GetPlayers(id = .) %>% 
    pluck(1, "full_name")
  
  # Gets stadium name
  
  info_away[[3]] <- game_info %>% 
    select(venue_name) %>% 
    pull()
  
  # Gets name of home team
  
  info_away[[4]] <- lineup_home %>% 
    select(teamName) %>% 
    slice(1) %>% 
    pull() %>% 
    word(2, -1)
  
  # Corrects team name if what's pulled above is incorrect
  
  if (info_away[[4]] %in% c("Bay Rays", "York Yankees", "City Royals",
                            "Angeles Angels", "York Mets", "Louis Cardinals",
                            "Angeles Dodgers", "Diego Padres", "Francisco Giants")) {
    
    info_away[[4]] <- info_away[[4]] %>% 
      word(2)
      
  }
  
  # Gets temperature
  
  info_away[[5]] <- game_info %>% 
    select(temperature) %>% 
    pull() %>% 
    as.integer()
  
  # Gets date
  
  info_away[[6]] <- game_info %>% 
    select(game_date) %>% 
    pull()
  
  # Gets transition matrices for away team
  
  tmatrices_away <- markov_matrices(standard = FALSE,
                                    state = State$new(top = TRUE),
                                    info = info_away)
  
  # Does same as above, but for the home lineup
  
  info_home <- list()
  
  # Creates list of player names
  
  info_home[[1]] <- as.list(lineup_home$fullName)
  
  # Gets pitcher name
  
  info_home[[2]] <- pbp_first %>% 
    filter(game_pk == game_id, inning_topbot == "Bot") %>% 
    select(pitcher) %>% 
    pull() %>% 
    sing$GetPlayers(id = .) %>% 
    pluck(1, "full_name")
  
  # Gets stadium name
  
  info_home[[3]] <- info_away[[3]]
  
  # Gets name of home team
  
  info_home[[4]] <- info_away[[4]]
  
  # Gets temperature
  
  info_home[[5]] <- info_away[[5]]
  
  # Gets date
  
  info_home[[6]] <- info_away[[6]]
  
  # Gets transition matrices for home team
  
  tmatrices_home <- markov_matrices(info = info_home)
  
  # Isolates play-by-play outcomes for each half-inning
  
  pbp_away <- pbp_first %>% 
    filter(game_pk == game_id, inning_topbot == "Top")
  
  pbp_home <- pbp_first %>% 
    filter(game_pk == game_id, inning_topbot == "Bot")
  
  for (pa in seq_len(nrow(pbp_away))) {
    
    # Gets index for batting order, w/ extra precaution should a team bat around
    
    index <- pa %% 9
    
    if (index == 0) {
      
      index <- 9
      
    }
    
    # Grabs the specific plate appearance
    
    ab <- pbp_away %>% 
      slice(pa)
    
    # Creates state
    
    test_state <- State$new(top = TRUE,
                            outs = pull(select(ab, outs_when_up)),
                            on_1b = !is.na(pull(select(ab, on_1b))),
                            on_2b = !is.na(pull(select(ab, on_2b))),
                            on_3b = !is.na(pull(select(ab, on_3b))))
    
    # Gets Markov predictions, specifically expected runs
    
    runs_exp <- markov_half_inning(idx = index,
                                   tmatrix_list = tmatrices_away,
                                   state = test_state) %>% 
      pluck(1)
    
    # Updates error for Singlearity
    
    error[1] <- (runs_exp - pull(select(ab, runs_to_end_inning)))^2
    num_pa <- num_pa + 1
    
    # Updates error for standard pred., using previous year's RE24 table
    
    year_prev <- game_info %>% 
      select(game_date) %>% 
      pull() %>% 
      as.Date() %>% 
      year() - 1
    
  }
  
  # For each PA:
  # Gets expected runs using markov_half_inning()
  # Finds difference between prediction and runs_to_end_inning
  # Squares and adds to running Singlearity total
  # Also gets expected runs in the state from RE24 tables
  # Finds difference between RE24 pred. and runs_to_end_inning
  # Squares and adds to running RE24 total
  # Goal: at end, compare Singlearity and RE24 totals
  
  # Add venue and temperature (baseballr function?)
  # Use RE24 table from year before?
  # Exclude innings where pitcher changes?

}


