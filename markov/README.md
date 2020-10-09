# Baseball and Markov Chains

This repository contains R code for simulating the results of baseball half-innings, using a Markov chain algorithm.

## Overview

Baseball has a relatively clean, distinct structure. Each plate appearance can be viewed as a separate matchup or event, and there are a fixed number of possible baserunner/out combinations, or states, for each plate appearance. While different plate appearances are distinct and exhibit a strong degree of independence, the results do rely heavily on which of the 24 states the game may be in before the start of plate appearance. For example, a double with nobody on base has a much different effect on the game than one with the bases loaded. As such, we want a way to combine the independence of individual plate appearances with the dependence of these events within innings. 

A Markov chain is a random probability model that exhibits one-step dependence. Put another way, the probability of transitioning from state *i* to state *j* is always the same, no matter what happened before, and only depends on state *i*. This is called the Markov property, and it simplifies probability calculations: we don't have to account for the entire past, only the most recent state. 

In a baseball sense, this means that the probability of an event happening is just dependent on the starting state, or baserunner/out combination. If we say *p<sub>i,j</sub>* is the probability of going from state *i* to state *j*, we can construct a 25 by 25 matrix containing all these probabilities, which is called the transition matrix (25, not 24, because we also need to be able to transition to an inning-ending three-out state). Many of these probabilities, of course, will be zero, as it's impossible to go from a one-out state to a zero-out state, or directly from a non-baserunner state to a bases-loaded state. 

This file combines the power of Markov chains for simulating an inning of baseball with Singlearity's machine learning-based plate appearance prediction capabilities. By being able to more accurately predict the outcome of the matchups at baseball's core, we can leverage these capabilities to move Markov chain baseball prediction methods to practical and applicable situations from its current position in the more theoretical realm.

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
# A tibble: 8 x 2
  `Expected Runs Scored` Probability
  <chr>                        <dbl>
1 0                          0.712  
2 1                          0.143  
3 2                          0.0795 
4 3                          0.0373 
5 4                          0.0171 
6 5                          0.00694
7 6                          0.00248
8 7+                         0.00173
```

## Command Line Usage

`markov/cmd_markov.R` enables dynamic user input via the command line. To see a list of possible inputs, type `cmd_markov.R --help`. 

### Sample

Below is an example command line input:

`./cmd_markov.R --start=2 --batters="DJ LeMahieu, Aaron Judge, Aaron Hicks, Giancarlo Stanton, Luke Voit, Brett Gardner, Gleyber Torres, Gio Urshela, Kyle Higashioka" --pitcher="Tyler Glasnow" --venue="Yankee Stadium" --hometeam="Yankees" --inning=3 --outs=1 --on2b --temperature=65 --pitchnumber=40`

And the resulting output:

`[1] "Expected Runs: 0.51728"`
`# A tibble: 8 x 2`
  `Expected Runs Scored` `Probability`
  `<chr>                        <dbl>`
`1 0                         0.703`   
`2 1                         0.167`   
`3 2                         0.0746` 
`4 3                         0.0324`  
`5 4                         0.0153`  
`6 5                         0.00507`
`7 6                         0.00167` 
`8 7+                        0.000966`

## Acknowledgments

Singlearity is by no means the first to explore the applicability of Markov chains in baseball. For their prior work on the subject, we'd like to thank [Mark Pankin](http://www.pankin.com/markov/theory.htm) for his efforts in developing the theory of Markov and baseball, [statshacker](http://statshacker.com/blog/2018/05/07/the-markov-chain-model-of-baseball) for the linear algebra refresher, and [Daniel Ursin](https://dc.uwm.edu/cgi/viewcontent.cgi?article=1969&context=etd) for the foundation of the algorithm used in this file.