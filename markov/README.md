# Baseball and Markov Chains

This repository contains R code for simulating the results of baseball half-innings, using a Markov chain algorithm.

## Overview

Baseball has a relatively clean, distinct structure. Each plate appearance can be viewed as a separate matchup or event, and there are a fixed number of possible baserunner/out combinations, or states, for each plate appearance. While different plate appearances are distinct and exhibit a strong degree of independence, the results do rely heavily on which of the 24 states the game may be in before the start of plate appearance. For example, a double with nobody on base has a much different effect on the game than one with the bases loaded. As such, we want a way to combine the independence of individual plate appearances with the dependence of these events within innings. 

A Markov chain is a random probability model that exhibits one-step dependence. Put another way, the probability of transitioning from state *i* to state *j* is always the same, no matter what happened prior to state *i* - this probability depends solely on state *i*. This is called the Markov property, and it simplifies probability calculations: we don't have to account for the entire past, only the most recent state. 

In a baseball sense, this means that the probability of an event happening is just dependent on the starting state, or baserunner/out combination. If we say *p<sub>i,j</sub>* is the probability of going from state *i* to state *j*, we can construct a 25 by 25 matrix containing all these probabilities, which is called the transition matrix (25, not 24, because we also need to be able to transition to an inning-ending three-out state). Many of these probabilities will be zero by definition, as it's impossible to go from a one-out state to a zero-out state, or directly from a non-baserunner state to a bases-loaded state. 

This file combines the power of Markov chains for simulating an inning of baseball with Singlearity's machine learning-based plate appearance prediction capabilities. By being able to more accurately predict the outcome of the matchups at baseball's core, we can leverage these capabilities to move Markov chain baseball prediction methods to practical and applicable situations from its current position in the more theoretical realm.

## Description

### Structure

There are four files that directly contribute to these Markov chain calculations: `tmatrix.R`, which calculates the transition matrices for each batter in a given lineup; `markov.R`, which contains functions to both source these transition matrices and also to calculate the expected runs from a half-inning; `get_core_data.R`, a file to get game-by-game player and environment information for a given season; and `experimental.R`, used to compare Markov chain predictions to actual results. We will walk through these files in the reverse order as mentioned above.

### `experimental.R`

After loading in several required packages and sourcing various other files, the first function `read_pbp_file` reads in downloaded first inning play-by-play data (from Baseball Savant) and saves locally, via the provided file path. The last line uses a `baseballr` function to convert it into a format that can later be further converted into a run expectancy table. Combined into one file, this collects all plate appearances from the first inning over the past six seasons.

Next is the `season_re24` function. This takes the year-by-year play-by-play data and converts it into a run expectancy table for each year using another `baseballr` function. This is used later when comparing Singlearity's predictions to the expected results.

After this, we want to get the game-level information from which we can accurately construct our transition matrices. To do this, we use a function sourced from `get_core_data.R` to get the relevant information and store it locally in a tibble (more on this function below). Now, when we compute our transition matrices, we can search our pre-created tibble for the right data, as opposed to making a call to the `baseballr` server for each game.

INNING DIFF

### `get_core_data.R`

This file is oriented around one function, `get_game_info`, which takes as input a unique game identification code and returns a line to add into the existing game information tibble. The file starts by declaring a blank tibble so that the environment has a reference to add the newest information to.

The function itself again relies on existing `baseballr` functionality. `get_batting_orders` gets the starting lineups for a given game, and we separate those into home and away, focusing on player IDs. Next, we filter our overall play-by-play dataset for the relevant game and half-inning, obtaining the player ID for the pitcher facing each of these lineups. 

Other information is environment-specific: using `get_game_info_mlb`, we can obtain stadium, home team, temperature, and date information in a relatively straightforward manner. A complication here is that there are some stadiums not in Singlearity's database, so we replace those with Progressive Field, a relatively neutral ballpark; additionally, some stadiums have changed names over the past few years, so those are manually adjusted as well. For team names with more than two words (e.g., "St. Louis Cardinals" or "New York Yankees" as opposed to "Baltimore Orioles"), we also make sure we're grabbing the proper name for those - straightforward because there aren't that many of these.

Along with date and temperature info, this is returned as an addition to the existing dataset.

### `markov.R`

### `tmatrix.R`

## Usage

### Installation

Requires R version 3.3 or higher.

### Prerequisites

Certain dependencies must be installed: 

```
# Requirements to run

install.packages("devtools")
devtools::install_github("singlearity-sports/singlearity-R")
```

### Run

Copy the file to your local directory, and run it:

```
env SINGLEARITY_API_KEY=YOUR_API_KEY R -f markov.R
```

The current default matchup is the Dodgers batting in the bottom of the first against Chris Paddack. The result will be the expected runs scored in the inning and the corresponding probability:

```
[1] "Expected Runs: 0.54077"# A tibble: 8 x 2  `Expected Runs Scored` Probability
  <chr>                        <dbl>
1 0                          0.714  
2 1                          0.142  
3 2                          0.0790 
4 3                          0.0370 
5 4                          0.0169 
6 5                          0.00685
7 6                          0.00246
8 7+                         0.00171
```

## Command Line Usage

`markov/cmd_markov.R` enables dynamic user input via the command line. To see a list of possible inputs, type `cmd_markov.R --help`. 

### Sample

Below is an example command line input:

`./cmd_markov.R --start=2 --batters="DJ LeMahieu, Aaron Judge, Aaron Hicks, Giancarlo Stanton, Luke Voit, Brett Gardner, Gleyber Torres, Gio Urshela, Kyle Higashioka" --pitcher="Tyler Glasnow" --venue="Yankee Stadium" --hometeam="Yankees" --inning=3 --outs=1 --on2b --temperature=65 --pitchnumber=40`

And the resulting output:

```
[1] "Expected Runs: 0.51728"
# A tibble: 8 x 2
  `Expected Runs Scored` Probability
  <chr>                        <dbl>
1 0                         0.703   
2 1                         0.167   
3 2                         0.0746  
4 3                         0.0324  
5 4                         0.0153  
6 5                         0.00507 
7 6                         0.00167 
8 7+                        0.000966
```

To run the file using league-average transition matrices from the 2020 season, use the `--standard` argument.

## Acknowledgments

Singlearity is by no means the first to explore the applicability of Markov chains in baseball. For their prior work on the subject, we'd like to thank [Mark Pankin](http://www.pankin.com/markov/theory.htm) for his efforts in developing the theory of Markov and baseball, [statshacker](http://statshacker.com/blog/2018/05/07/the-markov-chain-model-of-baseball) for the linear algebra refresher, and [Daniel Ursin](https://dc.uwm.edu/cgi/viewcontent.cgi?article=1969&context=etd) for the foundation of the algorithm used in this file.
