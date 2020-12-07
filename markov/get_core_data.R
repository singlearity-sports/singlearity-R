suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(baseballr))

# Replicating much of the code previously in experimental.R
# Downloading this data will reduce future runtime

all_game_info <- tibble(game_date = character(),
                        game_id = numeric(),
                        away_lineup = list(),
                        pitcher_vs_away = numeric(),
                        home_lineup = list(),
                        pitcher_vs_home = numeric(),
                        stadium = character(),
                        team_home = character(),
                        temperature = numeric())

get_game_info <- function(game_id) {
  
  # Gets batting orders and pitchers
  
  lineups <- get_batting_orders(game_id)
  
  lineup_away <- lineups %>% 
    filter(team == "away") %>% 
    arrange(batting_order)
  
  lineup_home <- lineups %>% 
    filter(team == "home") %>% 
    arrange(batting_order)
  
  pitcher_vs_home <- pbp_first %>% 
    filter(game_pk == game_id, inning_topbot == "Bot") %>% 
    select(pitcher) %>% 
    slice(1) %>% 
    pull()
  
  pitcher_vs_away <- pbp_first %>% 
    filter(game_pk == game_id, inning_topbot == "Top") %>% 
    select(pitcher) %>% 
    slice(1) %>% 
    pull()
  
  # Gets general game info
  
  game_info <- get_game_info_mlb(game_id)
  
  # Creates list of player names
  
  away <- lineup_away$id
  home <- lineup_home$id
  
  # Gets stadium name
  
  stad <- game_info %>% 
    select(venue_name) %>% 
    pull()
  
  # Replaces stadiums not in Singlearity w/ Progressive Field, a neutral ballpark
  # Also renames stadiums that have changed names over the past few years
  
  if (stad %in% c("Sahlen Field", "Turner Field", "Tokyo Dome",
                  "Globe Life Park in Arlington", "London Stadium",
                  "Estadio de Beisbol Monterrey", "BB&T Ballpark",
                  "TD Ameritrade Park", "Hiram Bithorn Stadium")) {
    
    stad <- "Progressive Field"
    
  } else if (stad == "Safeco Field") {
    
    stad <- "T-Mobile Park"
    
  } else if (stad == "SunTrust Park") {
    
    stad <- "Truist Park"
    
  } else if (stad == "AT&T Park") {
    
    stad <- "Oracle Park"
    
  } else if (stad == "O.co Coliseum") {
    
    stad <- "Oakland Coliseum"
    
  } else if (stad == "U.S. Cellular Field") {
    
    stad <- "Guaranteed Rate Field"
    
  } else if (stad == "Angel Stadium of Anaheim") {
    
    stad <- "Angel Stadium"
    
  }
  
  # Gets name of home team
  
  name <- lineup_home %>% 
    select(teamName) %>% 
    slice(1) %>% 
    pull() %>% 
    word(2, -1)
  
  # Corrects team name if what's pulled above is incorrect
  
  if (name %in% c("Bay Rays", "York Yankees", "City Royals",
                       "Angeles Angels", "York Mets", "Louis Cardinals",
                       "Angeles Dodgers", "Diego Padres", "Francisco Giants")) {
    
    name <- name %>% 
      word(2)
    
  }
  
  # Changes stadium home teams to properly align with normal home team
  
  if (stad == "Progressive Field") {
    
    name <- "Indians"
    
  }
  
  # Gets temperature
  
  temp <- game_info %>% 
    select(temperature) %>% 
    pull() %>% 
    as.integer()
  
  # Gets date
  
  date <- game_info %>% 
    select(game_date) %>% 
    pull() %>% 
    as.character()
  
  # Adds all this info to the dataset

  all_game_info <- all_game_info %>% 
    add_row(game_date = date,
            game_id = game_id,
            away_lineup = list(away),
            pitcher_vs_away = pitcher_vs_away,
            home_lineup = list(home),
            pitcher_vs_home = pitcher_vs_home,
            stadium = stad,
            team_home = name,
            temperature = temp)
  
  return(all_game_info)

}
