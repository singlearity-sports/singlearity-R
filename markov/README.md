# Baseball and Markov Chains

This repository contains R code for simulating the results of baseball half-innings, using a Markov chain algorithm.

## Overview

Baseball has a relatively clean, distinct structure. Each plate appearance can be viewed as a separate matchup or event, and there are a fixed number of possible baserunner/out combinations, or states, for each plate appearance. While different plate appearances are distinct and exhibit a strong degree of independence, the results do rely heavily on which of the 24 states the game may be in before the start of plate appearance. For example, a double with nobody on base has a much different effect on the game than one with the bases loaded. As such, we want a way to combine the independence of individual plate appearances with the dependence of these events within innings. 

A Markov chain is a random probability model that exhibits one-step dependence. Put another way, the probability of transitioning from state *i* to state *j* is always the same, no matter what happened prior to state *i* - this probability depends solely on state *i*. This is called the Markov property, and it simplifies probability calculations: we don't have to account for the entire past, only the most recent state. 

In a baseball sense, this means that the probability of an event happening is just dependent on the starting state, or baserunner/out combination. If we say *p<sub>i,j</sub>* is the probability of going from state *i* to state *j*, we can construct a 25 by 25 matrix containing all these probabilities, which is called the transition matrix (25, not 24, because we also need to be able to transition to an inning-ending three-out state). Many of these probabilities will be zero by definition, as it's impossible to go from a one-out state to a zero-out state, or directly from a non-baserunner state to a bases-loaded state. 

This file combines the power of Markov chains for simulating an inning of baseball with Singlearity's machine learning-based plate appearance prediction capabilities. By being able to more accurately predict the outcome of the matchups at baseball's core, we can leverage these capabilities to move Markov chain baseball prediction methods to practical and applicable situations from its current position in the more theoretical realm.

## APIs

The primary functionality of Singlearity's Markov capabilities are contained within the three functions described below. We hope the outlined description enables both further experimentation with Singlearity and also a more thorough look into the applicability of Markov chains in baseball.

### `markov_matrices`

Returns a list of nine 25 by 25 transition matrices.

ARGUMENTS: `standard`: true or false. If true, all nine matrices returned are the league-average matrices from 2020. Default is false.  
`state`: a Singlearity state containing information on the half of the inning, baserunners, and number of outs. Default is nobody on and no outs in the home half of the inning.  
`lineup`: a list of nine MLB players, which serves as the batting lineup. Can be either player IDs or strings of names, preferably the former. Default can be changed in the `markov.R` file but is currently the Dodgers lineup.  
`pitcher`: the opposing pitcher, either as a player ID or a string. Default is currently Chris Paddack.  
`stadium`: a string with the current name of the home stadium. Default is currently Dodger Stadium.  
`home`: a string with the nickname of the home team (i.e., "Red Sox" or "Athletics"). Default is currently Dodgers.  
`temp`: an integer with the game temperature, in Fahrenheit. Default is currently 70.  
`date`: a string with the game date, in YYYY-MM-DD format. Default is currently 2020-10-01.

### `markov_half_inning`

Returns a list of length two, the first the numeric value of expected runs in the inning and the second a table containing the probability distribution for runs in the inning.

ARGUMENTS: `idx`: an integer from 1 to 9, inclusive, corresponding to the position in the batting order from which point to compute expected runs. Default is 1 (i.e., from the top of the lineup).  
`tmatrix_list`: a list of nine 25 by 25 transition matrices, corresponding to spots 1 through 9 in the batting order. Default is an empty call to `markov_matrices`, which returns the matrices for the default Singlearity call to that function.  
`state`: a Singlearity state containing information on the half of the inning, baserunners, and number of outs. Default is nobody on and no outs in the home half of the inning.

### `inning_diff`

Returns a dataframe of play-by-play players, states, predictions, and results, updated with each game.

ARGUMENTS: `game`: an integer corresponding to an MLB game ID, which is used to grab plate appearances from that game, information from which is then used with the previous two functions to get Markov-based predictions.

## Description

### Structure

There are four files that directly contribute to these Markov chain calculations: `tmatrix.R`, which calculates the transition matrices for each batter in a given lineup; `markov.R`, which contains functions to both source these transition matrices and also to calculate the expected runs from a half-inning; `get_core_data.R`, a file to get game-by-game player and environment information for a given season; and `experimental.R`, used to compare Markov chain predictions to actual results. We will walk through these files in the reverse order as mentioned above.

### `experimental.R`

After loading in several required packages and sourcing various other files, the first function `read_pbp_file` reads in downloaded first inning play-by-play data (from Baseball Savant) and saves locally, via the provided file path. The last line uses a `baseballr` function to convert it into a format that can later be further converted into a run expectancy table. Combined into one file, this collects all plate appearances from the first inning over the past six seasons.

Next is the `season_re24` function. This takes the year-by-year play-by-play data and converts it into a run expectancy table for each year using another `baseballr` function. This is used later when comparing Singlearity's predictions to the expected results.

After this, we want to get the game-level information from which we can accurately construct our transition matrices. To do this, we use a function sourced from `get_core_data.R` to get the relevant information and store it locally in a tibble (more on this function below). Now, when we compute our transition matrices, we can search our pre-created tibble for the right data, as opposed to making a call to the `baseballr` server for each game.

Next is the `inning_diff` function. This takes a game ID as input and returns the results tibble with a new row for each plate appearance in that game. The first step is using the game info dataset to get the players, stadium, etc. for that game. Once obtained, the transition matrices for both the home and away teams are calculated using this info as input. Before going into more detailed computations, we filter the overall play-by-play dataset to get the plate appearances for the home and away teams for that game.

We then iterate over both the home and away team plate appearances (away first) using the `pa_iterate` function. For each plate appearance, we get the batting order position for the player of that plate appearance and pull the relevant PA from the segmented dataset. We then take the base-out state from that pulled PA to create the state to base our calculations off of. In addition to our Markov chain predictions, we also want to get Singlearity's wOBA prediction for this plate appearance, so we use `pa_pred_simple` and info already gathered to obtain both that prediction and the player names.

To get expected runs via the Markov chain, we call `markov_half_inning`, passing in the batting order position of the player at bat, the list of transition matrices for their team, and the current base-out state. As for the run expectancy prediction, the naive prediction is the average runs scored from that point forward in the previous season in the first inning - i.e., the relevant RE24 (RE for run expectancy, 24 for the 24 possible base-out states). This grabs that previous year's RE24 table, isolates the situation that matches the current base-out state, and grabs that run prediction.

For each plate appearance for both home and away, we add a row to our results tibble, tracking the game date and ID, batter/pitcher names and IDs, the half of the inning, the state before and after the PA, the PA wOBA prediction, runs scored for both Singlearity and RE24, and the actual number of runs scored.

Exiting the `inning_diff` function, we have a few lines of code designed to iterate over the different games in a similar fashion to what was done to obtain game-level info a few hundred lines earlier. We then have two lines to calculate the RMSE for both the RE24 and Singlearity methods of prediction.

### `get_core_data.R`

This file is oriented around one function, `get_game_info`, which takes as input a unique game identification code and returns a line to add into the existing game information tibble. The file starts by declaring a blank tibble so that the environment has a reference to add the newest information to.

The function itself again relies on existing `baseballr` functionality. `get_batting_orders` gets the starting lineups for a given game, and we separate those into home and away, focusing on player IDs. Next, we filter our overall play-by-play dataset for the relevant game and half-inning, obtaining the player ID for the pitcher facing each of these lineups. 

Other information is environment-specific: using `get_game_info_mlb`, we can obtain stadium, home team, temperature, and date information in a relatively straightforward manner. A complication here is that there are some stadiums not in Singlearity's database, so we replace those with Progressive Field, a relatively neutral ballpark; additionally, some stadiums have changed names over the past few years, so those are manually adjusted as well. For team names with more than two words (e.g., "St. Louis Cardinals" or "New York Yankees" as opposed to "Baltimore Orioles"), we also make sure we're grabbing the proper name for those - straightforward because there aren't that many of these.

Along with date and temperature info, this is returned as an addition to the existing dataset.

### `markov.R`

This file contains two functions, `markov_matrices` and `markov_half_inning`, which are the core of the Markov chain calculations. Having called libraries and sourced functions, we start by declaring a value for `EPSILON`, which tracks the maximum imprecision permitted in our half-inning simulations. Additionally, we create defaults for our transition matrix computations - these are rarely used but can be utilized as a sanity check of sorts. These inputs - lineup, pitcher, stadium, home team, temperature, and date - are all inputs into `markov_matrices`, along with the game state (typically passed in when called in `experimental.R`) and "standard" argument.

If "standard" is set to true, then the transition matrices computed will consist solely of league averages, calling the `tmatrix_std` function sourced from `tmatrix.R` (explained below). More often than not, however, it is set to false, enabling the user to obtain the list of transition matrices via the `tmatrix_sing` function, from that same file (likewise explained below). Some of the more specific arguments to that function, such as half of the inning, number of the inning, and pitch count, come from the Singlearity `State` variable passed into `markov_matrices`. 

`markov_half_inning` is where the expected runs from the Markov chain is actually calculated. As inputs, it takes an index (i.e., the position in the batting order of the player initiating the chain), a list of transition matrices (obtained through a call to `markov_matrices`), and the base-out state. When this function is called from `experimental.R` using actual plate appearances, the index is matched to the spot in the lineup of the player who's batting. Additionally, the transition matrices come from a call to `markov_matrices` with the relevant information on the players and game state, and the state itself comes from the data's description of the current state at that moment in time.

We start by declaring an empty 21x25 scorekeeping matrix *S* - 21 rows because we assume a team can score anywhere from 0 to 20 runs in an inning, and 25 columns because there are 24 unique base-out states in an inning, plus the three-out inning-ending state. We then need to determine where to place the value of 1: our starting state. We always start in the first row, as we want to discern what the future runs scored will be (so we're starting from a state of zero), and the column where we start is determined by the starting base-out state. 

To determine the single-inning run distribution, we iterate until the sum of the final column of this scorekeeping matrix is within `EPSILON` of 1 - i.e., the summed probability of all possible runs scored is essentially 1. How do we get to this point? 

We start with our transition matrix *T*, and we write it as the sum of five different matrices *T<sub>0, 1, ..., 4</sub>*, with the subscript corresponding to the number of runs scored for each event in that matrix. For example, in *T<sub>1</sub>*, the element in the first row and first column is the probability of going from a no-runner, no-out state to that same no-runner no-out state, which occurs when the batter hits a solo home run (and thus scores one run). These matrices are all constructed in this same manner.

Next, using a temporary scorekeeping matrix of the same 21x25 dimensions as *S*, we iterate row-by-row with our Markov chain matrix multiplication. For the first row, corresponding to zero runs scored, we take the first row in *S* and multiply it by *T<sub>0</sub>*, the result of which will give a 1x25 row of the probability of transitioning from the initial state to a state where after this batter no runs have scored (whether via hit, out, or other). For the second row, we do the same, now multiplying the first row in *S* by *T<sub>1</sub>* to get the probability that after this first batter one run has scored (now becoming the second row of this temporary matrix). We do the same for all the other rows in *S*, but there's a limit as to how many runs we can expect to have scored after one batter (anywhere from 0 to 4, depending on the starting state). 

Thus, we iterate over to the next batter, using that batter's new *T* and corresponding *T<sub>0, 1, ..., 4</sub>*, and following the same process as above and updating *S* each time, we see that we're able to compute the probability that a given number of runs score in an inning, from zero up to twenty - this is the final column of *S*, the sum of which determines the point of convergence for this calculation (otherwise we'd be multiplying these matrices in an endless loop!). 

Having calculated this expected run distribution, we do a bit of cleaning to create a table of the run distribution, as well as overall expected runs, and return it in a list. When used in `experimental.R`, the number of expected runs is extracted from the output and used as the basis for comparison with RE24.

### `tmatrix.R`

This file contains functions that use either league averages or Singlearity to create player transition matrices. It starts by calling and sourcing requisite packages and files, and then creates variables for relevant transitions. The first portion, which consists of different event probabilities like the percent of errors that are two-base errors or the likelihood a runner scores from first base on a double, is important when categorizing the probabilities of different events in a transition matrix. The second has assorted league-wide averages from the 2020 season, averages used to create a league-average transition matrix.

Starting with `tmatrix_std`, the inputs are a wide array of league-wide event probabilities, from the probability of a single to the probability of a triple play. Using the `elmt_fill` function, which takes a matrix, row/column indices, and probabilities to sum and impute into said location, this function iterates over all 625 positions in a transition matrix, filling in the relevant probabilities for each transition. For example, for the 23rd row of the 24th column, which denotes the transition from a two-out state with runners on second and third to a two-out state with the bases loaded, the probability of such a transition occurring is the sum of the probabilities of a walk (both intentional and not), a hit-by-pitch, and a catcher's interference. 

This is done for all possible transitions, and then because it's possible to have multiple events occur in one play (like a single and an error), the rows are normalized to make sure the probabilities for each starting state sum to one. The full transition matrix is then returned.

`tmatrix_sing` follows a similar procedure, but it uses `get_results` to obtain Singlearity predictions on which the transition matrices are based. `get_results` takes as input the batter list, pitcher, stadium, home team, temperature, date, half, inning, and date information passed into `tmatrix_sing`. It then creates a list of batting states and uses Singlearity functions to create a new batter list, pitcher, venue, and atmosphere that's in a Singlearity-compatible format.

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
