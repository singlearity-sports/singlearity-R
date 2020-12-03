# Collecting experimental results

library(singlearity)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(baseballr))
suppressPackageStartupMessages(library(lubridate))

# Assumes user is in the overarching "Singlearity" directory 

source(file = "markov/markov.R")
source(file = "R/get_singlearity_client.R")
source(file = "examples/pa_pred_simple.R")
source(file = "markov/get_core_data.R")
sing <- GetSinglearityClient()

# Loads in the different relevant data files
# All data is limited to first innings and PA-ending pitches
# Uses baseballr to get important info like runs to end of inning as well
# We then merge them all into one file

read_pbp_file <- function(filepath) {
  
  file <- read_csv(filepath) %>% 
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
  
  return(file)
  
}

pbp_2020_first <- read_pbp_file("markov/data/pbp_2020_first.csv")
pbp_2019_first <- read_pbp_file("markov/data/pbp_2019_first.csv")
pbp_2018_first <- read_pbp_file("markov/data/pbp_2018_first.csv") 
pbp_2017_first <- read_pbp_file("markov/data/pbp_2017_first.csv")
pbp_2016_first <- read_pbp_file("markov/data/pbp_2016_first.csv")
pbp_2015_first <- read_pbp_file("markov/data/pbp_2015_first.csv")

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

# Tibble in which to collect game information
# Getting the relevant game IDs before that

game_ids <- pbp_2018_first %>%
  select(game_pk) %>% 
  unique() %>%
  pull()

game_info <- tibble(game_date = character(),
                    game_id = numeric(),
                    away_lineup = list(),
                    pitcher_vs_away = numeric(),
                    home_lineup = list(),
                    pitcher_vs_home = numeric(),
                    stadium = character(),
                    team_home = character(),
                    temperature = numeric())

# Iterating over the selected games to grab info

tracker <- 0

for (game in game_ids) {
  
  tracker <- tracker + 1
  print(tracker)
  
  game_info <- bind_rows(game_info, get_game_info(game))
  
}

# Creates PA prediction function for use in grabbing wOBAs

pa_pred_fn <- function(batters = c(605141),
                       pitchers = c(605182),
                       states =  list(State$new()), 
                       atmosphere = Atmosphere$new(venue = 
                                                     sing$GetVenues(stadium.name = 
                                                                      'Dodger Stadium')[[1]], 
                                                   temperature = 70, 
                                                   home_team = sing$GetTeams(name = 
                                                                               'Dodgers')[[1]]),
                           date = format(Sys.Date(), "%Y-%m-%d"),
                           predictiontype = 'ab_outcome') 
  {
  
  candidate_batters <- list()
  candidate_pitchers <- list()
  
  for (batter in batters)
  {
    candidate_batters <- append(candidate_batters, sing$GetPlayers(id=batter))
  }
  
  for (pitcher in pitchers)
  {
    candidate_pitchers <- append(candidate_pitchers, sing$GetPlayers(id=pitcher))
  }
  
  #initialize empty lists
  matchups <- list()
  
  for (i in seq(length(states))) {
    for (p in candidate_pitchers) {
      matchups <- append(matchups, Matchup$new(batter = candidate_batters[[i]], 
                                               pitcher = candidate_pitchers[[1]], 
                                               atmosphere = atmosphere, 
                                               state = states[[i]], 
                                               date = date))
    }
  }
  
  results <- sing$GetPaSim(matchup = matchups)
  return(results)
}

# Similar procedure as above, but now getting predictions instead

result_data <- tibble(game_date = character(),
                      game_id = numeric(),
                      batter_id = numeric(),
                      batter_name = character(),
                      pitcher_id = numeric(),
                      pitcher_name = character(),
                      top_bot = character(),
                      start = character(),
                      end = character(),
                      pred_woba = numeric(),
                      pred_sing = numeric(),
                      pred_re24 = numeric(),
                      actual = numeric())

# Function to get difference between Singlearity predictions and runs scored

inning_diff <- function(game) {
  
  # Creates lineup using one call to markov_matrices(), including vars. like date
  # Uses info we've already gathered
  
  indiv_game_info <- game_info %>% 
    filter(game_id == game)

  # Gets transition matrices for away team
  
  tmatrices_away <- markov_matrices(standard = FALSE,
                                    state = State$new(top = TRUE),
                                    lineup = as.list(indiv_game_info$away_lineup[[1]]),
                                    pitcher = indiv_game_info$pitcher_vs_away,
                                    stadium = indiv_game_info$stadium,
                                    home = indiv_game_info$team_home,
                                    temp = indiv_game_info$temperature,
                                    date = indiv_game_info$game_date)

  # Gets transition matrices for home team
  
  tmatrices_home <- markov_matrices(standard = FALSE,
                                    state = State$new(top = FALSE),
                                    lineup = as.list(indiv_game_info$home_lineup[[1]]),
                                    pitcher = indiv_game_info$pitcher_vs_home,
                                    stadium = indiv_game_info$stadium,
                                    home = indiv_game_info$team_home,
                                    temp = indiv_game_info$temperature,
                                    date = indiv_game_info$game_date)

  # Isolates play-by-play outcomes for each half-inning
  
  pbp_away <- pbp_first %>% 
    filter(game_pk == game, inning_topbot == "Top")
  
  pbp_home <- pbp_first %>% 
    filter(game_pk == game, inning_topbot == "Bot")
  
  # Creating function over which to iterate to get results
  
  pa_iterate <- function(pbp, indiv_game_info, tmatrices, away) {
    
    state_list <- list()
    for (row in 1:nrow(pbp)) {

      ab <- pbp %>% 
        slice(row)
      state_list <- append(state_list, 
                           State$new(top = away,
                                     outs = pull(select(ab, outs_when_up)),
                                     on_1b = !is.na(pull(select(ab, on_1b))),
                                     on_2b = !is.na(pull(select(ab, on_2b))),
                                     on_3b = !is.na(pull(select(ab, on_3b)))))
    }

    if (away) {
      
      pa_pred <- pa_pred_fn(batters = indiv_game_info$away_lineup[[1]],
                            pitchers = indiv_game_info$pitcher_vs_away,
                            state = state_list,
                            atmosphere = Atmosphere$new(venue = sing$GetVenues(stadium.name = indiv_game_info$stadium)[[1]],
                                                        temperature = indiv_game_info$temperature, 
                                                        home_team = sing$GetTeams(name = indiv_game_info$team_home)[[1]]),
                            date = indiv_game_info$game_date)
    } else {
      
      pa_pred <- pa_pred_fn(batters = indiv_game_info$home_lineup[[1]],
                            pitchers = indiv_game_info$pitcher_vs_home,
                            state = state_list,
                            atmosphere = Atmosphere$new(venue = sing$GetVenues(stadium.name = indiv_game_info$stadium)[[1]],
                                                        temperature = indiv_game_info$temperature, 
                                                        home_team = sing$GetTeams(name = indiv_game_info$team_home)[[1]]),
                            date = indiv_game_info$game_date)
    }
    
    print(pa_pred)
    for (pa in seq_len(nrow(pbp))) {

      # Gets index for batting order, w/ extra precaution should a team bat around
      
      index <- pa %% 9
      
      if (index == 0) {
        index <- 9
      }
      
      ab <- pbp %>% 
        slice(pa)
      
      test_state <- State$new(top = away,
                              outs = pull(select(ab, outs_when_up)),
                              on_1b = !is.na(pull(select(ab, on_1b))),
                              on_2b = !is.na(pull(select(ab, on_2b))),
                              on_3b = !is.na(pull(select(ab, on_3b))))

      # Gets wOBA prediction
      
      woba <- pa_pred %>% 
        filter(batter == pull(select(ab, batter))) %>% 
        select(woba_exp) %>% 
        pull()

      batter_name <- pa_pred %>% 
        filter(batter == pull(select(ab, batter))) %>%
        select(batter_name) %>% 
        pull()
      
      pitcher_name <- pa_pred %>% 
        filter(batter == pull(select(ab, batter))) %>%
        select(pitcher_name) %>% 
        pull()
      
      # Gets Markov predictions, specifically expected runs
      
      runs_exp <- markov_half_inning(idx = index,
                                     tmatrix_list = tmatrices,
                                     state = test_state) %>% 
        pluck(1) %>% 
        as.numeric()
      
      # Gets prediction for this state using previous year's RE24
      
      year_prev <- indiv_game_info %>% 
        select(game_date) %>% 
        pull() %>% 
        as.Date() %>% 
        year() - 1
      
      re24 <- get(paste0("re24_", year_prev, "_first"))
      
      re_est <- re24 %>% 
        filter(outs == test_state$outs,
               on_1b == test_state$on_1b,
               on_2b == test_state$on_2b,
               on_3b == test_state$on_3b) %>% 
        select(avg_re) %>% 
        pull() %>% 
        as.numeric()
      
      # Adds into tibble
      
      result_data <- result_data %>% 
        add_row(game_date = indiv_game_info$game_date,
                game_id = indiv_game_info$game_id,
                batter_id = ab$batter,
                batter_name = batter_name,
                pitcher_id = ab$pitcher,
                pitcher_name = pitcher_name,
                top_bot = ab$inning_topbot,
                start = ab$base_out_state,
                end = ab$next_base_out_state,
                pred_woba = woba,
                pred_sing = runs_exp,
                pred_re24 = re_est,
                actual = ab$runs_to_end_inning)
    }
    
    return(result_data)
    
  }
  
  result_data <- pa_iterate(pbp_away, indiv_game_info, tmatrices_away, T)
  result_data <- pa_iterate(pbp_home, indiv_game_info, tmatrices_home, F)
  
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

  return(result_data)
  
}

game_ids <- pbp_2018_first %>% 
  select(game_pk) %>% 
  unique() %>% 
  pull()

# Creates tracker and results tibble

tracker <- 0
results_all <- tibble(game_date = character(),
                      game_id = numeric(),
                      batter_id = numeric(),
                      batter_name = character(),
                      pitcher_id = numeric(),
                      pitcher_name = character(),
                      top_bot = character(),
                      start = character(),
                      end = character(),
                      pred_woba = numeric(),
                      pred_sing = numeric(),
                      pred_re24 = numeric(),
                      actual = numeric())

for (game in game_ids) {
  
  tracker <- tracker + 1
  print(tracker)
  
  results_all <- bind_rows(results_all, inning_diff(game))

}

# Finds errors

rmse_re24 <- sqrt((results_all %>% mutate(sqdiff_re24 = (pred_re24 - actual)^2) %>% select(sqdiff_re24) %>% pull() %>% sum()) / nrow(results_all))
rmse_sing <- sqrt((results_all %>% mutate(sqdiff_sing = (pred_sing - actual)^2) %>% select(sqdiff_sing) %>% pull() %>% sum()) / nrow(results_all))

