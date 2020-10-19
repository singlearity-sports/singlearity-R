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
                       pbp_2017_first, pbp_2016_first)










# Function to generate list of needed dates, given start and end

date_list <- function(start, end) {
  seq(as.Date(start), as.Date(end), by = "days")
}

# Uses start and end dates of 2015-2020 MLB seasons to get lists of dates

dates_2015 <- date_list("2015-04-05", "2015-10-04")
dates_2016 <- date_list("2016-04-03", "2016-10-02")
dates_2017 <- date_list("2017-04-02", "2017-10-01")
dates_2018 <- date_list("2018-03-29", "2018-10-01")
dates_2019 <- date_list("2019-03-28", "2019-09-29")
dates_2020 <- date_list("2020-07-23", "2020-09-27")

# Could just scrape data from start date to end date
# But we want the data to be in order of date played and ordered within game
# So instead we write a function to get data for each date
# And within that function we reorder the data by inning for each game

scrape_day <- function(date) {
  
  # Uses baseballr to get the data for that day
  
  scraped <- scrape_statcast_savant(start_date = date, end_date = date, 
                                    player_type = "pitcher")
  
  # Sorts the data by game
  
  mod_vector <- vector()
  for(i in 1:length(sort(unique(scraped$game_pk)))) {
    game <- scraped[scraped$game_pk == sort(unique(scraped$game_pk))[i], ]
    mod_vector <- rbind(mod_vector, game)
  }
  
  # Returns as a dataframe
  # Only looking at first-inning outcomes
  
  scraped <- as.data.frame(mod_vector)
  df <- map_df(scraped, rev) %>% 
    filter(inning == 1, events != "null")

  return(df)
}

# Function to get yearly data, write to csv, and then remove from envmt.
# Saves processing power - had difficulty collecting all the data
# "year" line creates a string that extracts name of the passed-in dataframe
# Sets environment to .GlobalEnv when removing file to ensure it's removed

write_and_remove <- function(data) {
  var <- ".csv"
  year <- deparse(substitute(data))
  write_csv(data, path = paste0("data/", paste0(year, var)))
  rm(list = as.character(substitute(data)), envir = .GlobalEnv)
}

# Scrapes data for each year

data_2015 <- map_dfr(dates_2015, scrape_day)
write_and_remove(data_2015)

data_2016 <- map_dfr(dates_2016, scrape_day)
write_and_remove(data_2016)

data_2017 <- map_dfr(dates_2017, scrape_day)
write_and_remove(data_2017)

data_2018 <- map_dfr(dates_2018, scrape_day)
write_and_remove(data_2018)

data_2019 <- map_dfr(dates_2019, scrape_day)
write_and_remove(data_2019)

data_2020 <- map_dfr(dates_2020, scrape_day)
write_and_remove(data_2020)

# Loads files back in

load_files <- function(path) { 
  files <- dir(path, pattern = "data_\\d{4}.csv", full.names = TRUE)
  bind_rows(map_df(files, read_csv))
}

# loads in all files in this directory and combines into single dataframe

pitches <- load_files("data")
