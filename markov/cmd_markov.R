#!/usr/local/bin/Rscript

# Command-line functionality for Markov chains

library(singlearity)
suppressPackageStartupMessages(library(optparse))

# Assumes user is in the overarching "Singlearity" directory 

source(file = "markov/markov.R")
source(file = "R/get_singlearity_client.R")

sing <- GetSinglearityClient()

# Creates list of command-line argument options

option_list = list(
  make_option(c("--start", "-s"), type = "integer", default = 1,
              help = "Starting position in the batting order. Default %default"),
  make_option(c("--batters", "-b"), type = "character", 
              default = "Mookie Betts, Max Muncy, Justin Turner, Cody Bellinger, Corey Seager, AJ Pollock, Joc Pederson, Austin Barnes, Gavin Lux",
              help = "Comma-separated list of batter names. Default '%default'"),
  make_option(c("--pitcher", "-p"), type = "character", default = "Chris Paddack",
              help = "Comma-separated list of pitcher names. Default '%default'"),
  make_option(c("--venue", "-v"), type = "character", default = "Dodger Stadium",
              help = "Venue name. Default '%default'"),
  make_option(c("--hometeam"), type = "character", default = "Dodgers",
              help = "Name of home team. Default %default"),
  make_option(c("--inning", "-i"), type = "integer", default = 1,
              help = "Inning. Default %default"),
  make_option(c("--outs", "-o"), type = "integer", default = 0,
              help = "Outs. Default %default"),
  make_option(c("--away", "-a"), action = "store_true", default = FALSE,
              help = "True if team is away. Default %default"),
  make_option(c("--on1b"), action = "store_true", default = FALSE,
              help = "True if runner on first. Default %default"),
  make_option(c("--on2b"), action = "store_true", default = FALSE,
              help = "True if runner on second. Default %default"),
  make_option(c("--on3b"), action = "store_true", default = FALSE,
              help = "True if runner on third. Default %default"),
  make_option(c("--temperature", "-t"), type = "integer", default = 70,
              help = "Temperature at start time. Default %default"),
  make_option(c("--date", "-d"), type = "character", default = "2020-10-01",
              help = "Date of matchup. Default %default"),
  make_option(c("--pitchnumber", "-c"), type = "integer", default = 0,
              help = "Pitcher's pitch count at start of the at-bat. Default %default"),
  make_option(c("--standard"), action = "store_true", default = FALSE,
              help = "True if using league avg. for transition matrices. Default %default"),
  make_option(c("--plot"), type = "character", default = '',
              help = "Comma-separated list of values to plot; e.g., 'woba, hr, so'")
)

# Creates capabilities to parse arguments

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Creates list of arguments

# Lineup start

start <- opt$start

# Creates new state

state <- State$new(inning = opt$inning, outs = opt$outs, top = opt$away,
                   on_1b = opt$on1b, on_2b = opt$on2b, on_3b = opt$on3b,
                   pitch_number = opt$pitchnumber)

# Runs generic Markov if standard option is true

standard <- opt$standard

if (standard) {
  
  # Gets transition matrices for standard option
  
  matrices <- markov_matrices(standard = TRUE)
  
  # Gets Markov chain results, using given matrices
  
  results <- markov_half_inning(idx = start, 
                                tmatrix_list = matrices, 
                                state = state)
  print(results[[1]])
  results[[2]]
  
} else {
  
  # Runs Singlearity Markov chain otherwise
  
  # Lineup
  
  lineup <- as.list(strsplit(opt$batters, ",")[[1]])
  for (i in 1:length(lineup)) {
    lineup[[i]] = trimws(lineup[[i]])
  }
  
  # Pitcher
  
  pitcher <- opt$pitcher
  
  # Stadium
  
  stad <- opt$venue
  
  # Home team
  
  home <- opt$hometeam
  
  # Temperature
  
  temp <- opt$temperature
  
  # Date
  
  date <- opt$date
  
  # Creating list of above inputs
  
  info <- list(lineup, pitcher, stad, home, temp, date)
  
  # Gets transition matrices
  
  matrices <- markov_matrices(standard = FALSE,
                              state = state,
                              info = info)
  
  # Gets Markov chain results, using given matrices
  
  results <- markov_half_inning(idx = start, 
                                tmatrix_list = matrices, 
                                state = state)
  print(results[[1]])
  results[[2]]
  
}
