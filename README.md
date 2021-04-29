![enter image description here](https://www.singlearity.com/static/assets/Logo-PNG.png)
# Welcome to Singlearity!


Singlearity is a web-based service for baseball analytics.  It uses machine learning to make predictions based on a wide range of player and historical data.    These predictions can be used to make more effective pre-game and in-game strategy decisions and to provide for more accurate game simulations.

This work was presented at the 2021 SABR Conference and the 2021 Sloan Sports Analytics Conference.   You can view the [technical paper](https://www.singlearity.com/static/assets/sloan-singlearity.pdf) or the [Powerpoint presentation](https://www.singlearity.com/static/assets/Singlearity-sabr.pptx).

You can also view an earlier article about Singlearity that appeared on [Baseball Prospectus](https://www.baseballprospectus.com/news/article/59993/singlearity-using-a-neural-network-to-predict-the-outcome-of-plate-appearances/).

# Description

This repository contains sample R code for making programmatic calls to the Singlearity web service hosted at api.singlearity.com.

There are two closely related types of predictions that can be obtained:

* **Batter vs. Pitcher predictions (Singlearity-PA)**.   To generate batter vs. pitcher predictions, you must programmatically generate a **Matchup**.  A Matchup consists of a **Batter**, a **Pitcher**, the **Atmosphere** (containing things such as game location and weather), and a **State** (containing things such as score, inning, and baserunners).  A list of matchups can be submitted to the Singlearity server and it will return predicted outcomes for each matchup.  Visit the [singlearity.com](https://www.singlearity.com) website to see a GUI version of Singlearity-PA.

* **Game simulation predictions (Singlearity-Game)**.   Game simulations work by running hundreds or thousands of Monte Carlo simulations using the plate appearance outcomes provided by Singlearity-PA.   To generate game simulations, you must programmatically create home and away **Lineup**s.   The simulation may optionally include a starting **State** at some intermediate point in the game.  This would allow you to simulate, for instance, a tie game in the bottom of the 9th inning game with multiple runners on base, the #5 hitter coming to bat with a tired pitcher on the mound.   Currently, Singlearity-Game only supports simulating to the end of the half-inning.  With a very short piece of code, you could, for instance, simulate how successful each of ten different relievers would be in holding a lead in the bottom of the 10th inning when facing a given portion of the lineup.    It is possible to accurately simulate hundreds or thousands of games in just a few seconds. 

# Markov Chain Based Simulations

There is open source code included here for generating run expectancies using Singlearity-PA projections in combination with Markov chains.   See [https://github.com/singlearity-sports/singlearity-R/tree/master/markov](here) for a full description.

# Requirements

R 3.3+

## Installation

Obtain a Trial API key through the [Singlearity Contact Form](https://docs.google.com/forms/d/e/1FAIpQLSdO_K9_6cGBG_iStuSMKbqUBRX3Z8RAYzNVFRBVIXuumVSjAg/viewform?usp=sf_link)



### Prerequisites

Install the dependencies

```
#R code
install.packages("devtools")
devtools::install_github("singlearity-sports/singlearity-R")
```


## Example Usage

**Create it**

Create a file ```pa_pred_very_simple.R``` in the same local directory with:

```
#pa_pred_very_simple.R

##########################################
# Make predictions for groups of batters vs groups of pitchers
##########################################
library(singlearity)
sing <- GetSinglearityClient()

#list of batters
batter_list = c('Mookie Betts', 'Justin Turner', 'Max Muncy', 'Cody Bellinger')

#list of pitchers
pitcher_list = c('Chris Paddack', 'Emilio Pagan')

#initialize empty lists
candidate_batters <- list()
candidate_pitchers <- list()

for (batter in batter_list)
{
  candidate_batters <- append(candidate_batters, sing$GetPlayers(name=batter))
}

for (pitcher in pitcher_list)
{
  candidate_pitchers <- append(candidate_pitchers, sing$GetPlayers(name=pitcher))
}

venue <- sing$GetVenues(stadium.name = 'Dodger Stadium')[[1]]
atmosphere <- Atmosphere$new(venue = venue, temperature = 70, home_team = sing$GetTeams(name = 'Dodgers')[[1]])

matchups <- list()
for (b in candidate_batters)
{
  for (p in candidate_pitchers)
  {
    matchups <- append(matchups, Matchup$new(batter = b, pitcher = p, atmosphere = atmosphere, state = State$new()))
  }
}

results <- sing$GetPaSim(matchup = matchups)
results <- results[c('batter_name', 'pitcher_name', 'hr_exp', 'so_exp', 'ba_exp', 'ops_exp', 'woba_exp')]
results <- results[order(results$woba_exp, decreasing = TRUE), ]
print(results)
```

**Run it**
```
env SINGLEARITY_API_KEY=YOUR_API_KEY R -f pa_pred_very_simple.R
```
**Results**
```
     batter_name  pitcher_name     hr_exp    so_exp    ba_exp   ops_exp  woba_exp
1 Cody Bellinger  Emilio Pagan 0.06445228 0.2767457 0.2708578 0.8931689 0.3793040
5 Cody Bellinger Chris Paddack 0.06079568 0.2637437 0.2697733 0.8667926 0.3687907
7   Mookie Betts Chris Paddack 0.04069185 0.2468077 0.2526243 0.7726798 0.3346327
3   Mookie Betts  Emilio Pagan 0.04121564 0.2647337 0.2463460 0.7623101 0.3301859
6  Justin Turner Chris Paddack 0.04053899 0.2703831 0.2423686 0.7216711 0.3125078
2      Max Muncy  Emilio Pagan 0.04984803 0.3925197 0.2065340 0.7132002 0.3099304
4  Justin Turner  Emilio Pagan 0.04254675 0.2940399 0.2347854 0.7125016 0.3079187
8      Max Muncy Chris Paddack 0.04582457 0.3740999 0.2098127 0.7025451 0.3065602
```

## Command Line Usage
```examples/cmd_pa_pred.R``` is a command line utility for quickly generating batter vs. pitcher predictions.   It is a "swiss-army knife" for creating matchups with various different options.  The source code can also be examined to view the R code necessary to generate different options.   To run this command line, type:

```cmd_pa_pred.R```      - you will likely need to make this file executable 
or 
```R -f cmd_pa_pred.R```

To see a list of options, type:
 ```cmd_pa_pred.R --help```

This results in:
```Usage: ./cmd_pa_pred.R [options]


Options:
	--batters=BATTERS
		comma separated list of batter names.   Default 'Mookie Betts, Cody Bellinger'

	--pitchers=PITCHERS
		comma separated list of pitcher names.  Default 'Mike Clevinger, Chris Paddack'

	--venue=VENUE
		venue name.  Default 'Dodger Stadium' 

	--hometeam=HOMETEAM
		name of home team.  Default Dodgers

	--date=DATE
		date of the game (use format like 2018-08-25).  Defaults to today's date

	--inning=INNING
		inning. Default 1

	--bottom
		true if bottom of the inning. Default FALSE 

--bottom
		true if top half of the inning. Default FALSE

	--outs=OUTS
		outs. Default 0

	--on1b
		true if runner on first. Default FALSE

	--on2b
		true if runner on second. Default FALSE

	--on3b
		true if runner on third. Default FALSE

	--temperature=TEMPERATURE
		temperature at start time.  Default 70

	--batscore=BATSCORE
		batting team's score. Default 0

	--fieldscore=FIELDSCORE
		fielding team's score. Default 0

	--pitchnumber=PITCHNUMBER
		pitcher's pitch count at start of the at bat

	--predictiontype=PREDICTIONTYPE
		Type of prediction.  Choose either 'ab_outcome', 'ab_woba', 'ab_woba_no_state'.  Default 'ab_outcome'

	--plot=PLOT
		comma separated list of values to plot e.g. 'woba, hr, so'

	-h, --help
		Show this help message and exit

```
**Sample Command Line**
Here is an example of using the command line to generate matchups.
``` ./cmd_pa_pred.R --batters='Joc Pederson, Giancarlo Stanton' --pitchers='Blake Snell, Zack Greinke' --venue='Yankee Stadium' --hometeam='Yankees' --inning=6 --pitchnumber=80 --outs=1 --on2b --temperature=90 --plot=woba```


## Example Visualizations
It is easy to combine the Singlearity prediction outputs with libraries such as ggplot to create visualizations which communicate the important information efficiently.  [Here is an example in R](https://github.com/singlearity-sports/singlearity-R/blob/master/examples/wOBA_by_pitch_count.R "Here is an example") showing each batter's expected wOBA as Max Scherzer's pitch count increases.

![enter image description here](https://github.com/singlearity-sports/singlearity-R/blob/master/resources/woba_by_pitch_count.png)

It is also easy to create visualizations which efficiently represent the data needed to make player substitutions.   [Here is an example in R](https://github.com/singlearity-sports/singlearity-R/blob/master/examples/batters_vs_pitchers.R "Here is an example in R") showing the predicted wOBA for each Dodgers batter vs. each Padres pitcher.  These predictions and their visualizations take only a few seconds to produce. 

![enter image description here](https://github.com/singlearity-sports/singlearity-R/blob/master/resources/woba_batter_vs_pitcher.png)
<!--stackedit_data:
eyJoaXN0b3J5IjpbLTExOTExOTE3MDFdfQ==
-->
