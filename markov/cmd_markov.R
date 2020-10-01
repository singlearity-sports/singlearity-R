#!/usr/local/bin/Rscript

# Command-line functionality for Markov chains

suppressPackageStartupMessages(library(optparse))

# Assumes user is in the overarching "Singlearity" directory 

source("markov/markov.R")
source("examples/common.R")

# Creates list of command-line argument options

option_list = list(
  make_option(c("--start", "-s"), type = "integer", default = 1,
              help = "Starting position in the batting order. Default %default"),
  make_option(c("--batters", "-b"), type = "character", 
              default = "Mookie Betts, Max Muncy, Justin Turner, Cody Bellinger,
              Corey Seager, AJ Pollock, Joc Pederson, Austin Barnes, Gavin Lux",
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
  make_option(c("--on1b", "-1"), action = "store_true", default = FALSE,
              help = "True if runner on first. Default %default"),
  make_option(c("--on2b", "-2"), action = "store_true", default = FALSE,
              help = "True if runner on second. Default %default"),
  make_option(c("--on3b", "-3"), action = "store_true", default = FALSE,
              help = "True if runner on third. Default %default"),
  make_option(c("--temperature", "-t"), type = "integer", default = 70,
              help = "Temperature at start time. Default %default"),
  make_option(c("--pitchnumber", "-c"), type = "integer", default = 0,
              help = "Pitcher's pitch count at start of the at-bat. Default %default"),
  make_option(c("--plot"), type = "character", default = '',
              help = "Comma-separated list of values to plot; e.g., 'woba, hr, so'")
)

# Creates capabilities to parse arguments

opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

# Creates list of arguments

# Lineup start

start = opt$start

# Lineup

lineup = as.list(strsplit(opt$batters, ",")[[1]])
for (i in 1:length(lineup)) {
  lineup[[i]] = trimws(lineup[[i]])
}

# Pitcher

pitcher = opt$pitcher

# Stadium

stad = opt$venue

# Home team

home = opt$hometeam

# Temperature

temp = opt$temperature

# Creating list of above inputs

info <- list(lineup, pitcher, stad, home, temp)

# Creates new state

state = State$new(inning = opt$inning, outs = opt$outs, top = opt$away,
                  on_1b = opt$on1b, on_2b = opt$on2b, on_3b = opt$on3b,
                  pitch_number = opt$pitchnumber)

# Calling Markov function

markov_half_inning(start, info, state)

