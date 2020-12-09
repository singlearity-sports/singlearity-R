#!/usr/local/bin/Rscript


library(singlearity)
library(optparse)
sing <- GetSinglearityClient()

source(file='utils.R')

    option_list = list(
    make_option(c("--batters"), type="character", default="Mookie Betts, Cody Bellinger",
                help="comma separated list of batter names.   Default '%default'"),
    make_option(c("--pitchers"), type="character", default="Mike Clevinger, Chris Paddack",
                help="comma separated list of pitcher names.  Default '%default'"),
    make_option(c("--venue"), type="character", default="Dodger Stadium",
              help="venue name.  Default '%default' "),
    make_option(c("--hometeam"), type="character", default="Dodgers",
              help="name of home team.  Default %default"),
    make_option(c("--date"), type="character", default=format(Sys.Date(), "%Y-%m-%d"),
              help="date of the game (use format like 2018-08-25).  Defaults to today's date"),
    make_option(c("--inning"), type="integer", default=1,
               help="inning. Default %default"),
    make_option(c("--bottom"), action="store_true", default=FALSE,
              help="true if bottom half of the inning. Default %default"),
    make_option(c("--outs"), type="integer", default=0,
               help="outs. Default %default"),
    make_option(c("--on1b"), action="store_true", default=FALSE,
              help="true if runner on first. Default %default"),
    make_option(c("--on2b"), action="store_true", default=FALSE,
              help="true if runner on second. Default %default"),
    make_option(c("--on3b"), action="store_true", default=FALSE,
              help="true if runner on third. Default %default"),
    make_option(c("--temperature"), type="integer", default=70,
              help="temperature at start time.  Default %default"),
    make_option(c("--batscore"), type="integer", default=0,
              help="batting team's score. Default %default"),
    make_option(c("--fieldscore"), type="integer", default=0,
              help="fielding team's score. Default %default"),
    make_option(c("--pitchnumber"), type="integer", default=0,
              help="pitcher's pitch count at start of the at bat"),
    make_option(c("--predictiontype"), type="character", default="ab_outcome",
              help="Type of prediction.  Choose either 'ab_outcome', 'ab_woba', 'ab_woba_no_state'.  Default '%default'"),
    make_option(c("--plot"), type="character", default='',
              help="comma separated list of values to plot e.g. 'woba, hr, so'")
    )
  
  opt_parser = OptionParser(option_list=option_list)
  opt = parse_args(opt_parser)
  batters_list = as.list(strsplit(opt$batters, ",")[[1]])
  pitchers_list = as.list(strsplit(opt$pitchers, ",")[[1]])
  
  plot_list = as.list(strsplit(opt$plot, ",")[[1]])

  candidate_batters <- list()
  candidate_pitchers <- list()
  for (batter in batters_list)
  {
    candidate_batters <- append(candidate_batters, sing$GetPlayers(name=trimws(batter)))
  }
   
  for (pitcher in pitchers_list)
  {
     candidate_pitchers <- append(candidate_pitchers, sing$GetPlayers(name=trimws(pitcher)))
  }
  
  state <- State$new(inning = opt$inning, 
                         on_1b = opt$on1b, 
                         on_2b = opt$on2b,
                         on_3b = opt$on3b, 
                         outs = opt$outs, 
                         top = !(opt$bottom),
                         bat_score = opt$batscore,
                         fld_score = opt$fieldscore,
                         pitch_number = opt$pitchnumber)
  venue <- sing$GetVenues(stadium.name = opt$venue)[[1]]
  atmosphere <- Atmosphere$new(venue = venue, temperature = opt$temperature, home_team = sing$GetTeams(name = opt$hometeam)[[1]])
   
 
  matchups <- list()
  for (b in candidate_batters) 
  {
    for (p in candidate_pitchers)
    {
      matchups <- append(matchups, Matchup$new(batter = b, pitcher = p, atmosphere = atmosphere, state = state, date = opt$date))
    }
  }

  results <- sing$GetPaSim(matchup = matchups, model.name = opt$predictiontype)
  print(results)
  
  if (length(plot_list) > 0) {
      plot_pa_pred_results(results, plot_list, state, atmosphere)
  }

