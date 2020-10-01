# Command-line functionality for Markov chains

library(optparse)

source("examples/common.R")
source("markov/markov.R")

# Creates list of command-line argument options

option_list = list(
  make_option(c("--batters"), type = "character", 
              default = "Mookie Betts, Max Muncy, Justin Turner, Cody Bellinger,
              Corey Seager, AJ Pollock, Joc Pederson, Austin Barnes, Gavin Lux",
              help = "Comma-separated list of batter names. Default '%default'"),
  make_option(c("--pitchers"), type = "character", default = "Chris Paddack",
              help = "Comma-separated list of pitcher names. Default '%default'"),
  make_option(c("--venue"), type = "character", default = "Dodger Stadium",
              help = "Venue name. Default '%default' "),
  make_option(c("--hometeam"), type = "character", default = "Dodgers",
              help = "Name of home team. Default %default"),
  make_option(c("--inning"), type = "integer", default = 1,
              help = "Inning. Default %default"),
  make_option(c("--outs"), type = "integer", default = 0,
              help = "Outs. Default %default"),
  make_option(c("--on1b"), action = "store_true", default = FALSE,
              help = "True if runner on first. Default %default"),
  make_option(c("--on2b"), action = "store_true", default = FALSE,
              help = "True if runner on second. Default %default"),
  make_option(c("--on3b"), action = "store_true", default = FALSE,
              help = "True if runner on third. Default %default"),
  make_option(c("--temperature"), type = "integer", default = 70,
              help = "Temperature at start time. Default %default"),
  make_option(c("--batscore"), type = "integer", default = 0,
              help = "Batting team's score. Default %default"),
  make_option(c("--fieldscore"), type = "integer", default = 0,
              help = "Fielding team's score. Default %default"),
  make_option(c("--pitchnumber"), type = "integer", default = 0,
              help = "Pitcher's pitch count at start of the at-bat"),
  make_option(c("--plot"), type = "character", default = '',
              help = "comma separated list of values to plot; e.g., 'woba, hr, so'")
)
