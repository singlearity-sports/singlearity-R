# Collecting experimental results

library(singlearity)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(baseballr))

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

re24_2020_first <- season_re24(pbp_2020_first)
re24_2019_first <- season_re24(pbp_2019_first)
re24_2018_first <- season_re24(pbp_2018_first)
re24_2017_first <- season_re24(pbp_2017_first)
re24_2016_first <- season_re24(pbp_2016_first)

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


