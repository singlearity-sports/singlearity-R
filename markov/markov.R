# Markov chains in baseball

# Runners  ---  1--  -2-  --3  12-  1-3  -23  123
# Outs
#  0        1    4    7   10   13   16   19   22 
#  1        2    5    8   11   14   17   20   23
#  2        3    6    9   12   15   18   21   24

# T is a 28x28 matrix with transition probabilities from one state to another
# T_(i,j) gives the probability of going from state i to state j
# The sum of all transition probabilities in a given row must equal 1

# With help from https://tinyurl.com/y3t4om6o
library(singlearity)
sing <- GetSinglearityClient()

suppressPackageStartupMessages(library(wordspace))
suppressPackageStartupMessages(library(tidyverse))
source("markov/tmatrix.R")

# The error in the probability
# Exits when difference between the sum of all run-scoring probabilities and 1 is < EPSILON

EPSILON <- 1 / 100000

# Default list for Singlearity transition matrices

info_default <- list()

info_default[[1]] <- list("Mookie Betts", "Max Muncy", "Justin Turner", 
                          "Cody Bellinger", "Corey Seager", "AJ Pollock", 
                          "Joc Pederson", "Austin Barnes", "Gavin Lux")
info_default[[2]] <- "Chris Paddack"
info_default[[3]] <- "Dodger Stadium"
info_default[[4]] <- "Dodgers"
info_default[[5]] <- 70
info_default[[6]] <- "2020-10-01"

# Gets proper transition matrices for the Markov simulation

markov_matrices <- function(standard = FALSE, 
                            state = State$new(top = FALSE), 
                            info = info_default) {
  
  # If the user just wants the default league transition matrices
  
  if (standard) {
    
    # Some of this code is taken from the Singlearity transition matrix function
    # From tmatrix.R
    
    tmatrix_1 <- matrix(0, 25, 25)
    tmatrix_2 <- matrix(0, 25, 25)
    tmatrix_3 <- matrix(0, 25, 25)
    tmatrix_4 <- matrix(0, 25, 25)
    tmatrix_5 <- matrix(0, 25, 25)
    tmatrix_6 <- matrix(0, 25, 25)
    tmatrix_7 <- matrix(0, 25, 25)
    tmatrix_8 <- matrix(0, 25, 25)
    tmatrix_9 <- matrix(0, 25, 25)
    
    # Initializes list to improve speed and functionality
    
    tmatrix_list <- list(tmatrix_1, tmatrix_2, tmatrix_3, tmatrix_4, tmatrix_5,
                         tmatrix_6, tmatrix_7, tmatrix_8, tmatrix_9)
    
    for (i in 1:9) {
      
      tmatrix_list[[i]] <- tmatrix_std()
      
    }
    
  }
  
  else {
    
    # Assigns values from function
    # Also creates a list of the 24 possible batting states
    
    batters <- info[[1]]
    pitcher <- info[[2]]
    stadium <- info[[3]]
    home <- info[[4]]
    temp <- info[[5]]
    date <- info[[6]]
    away <- state$top
    inning <- state$inning
    pitch_ct <- state$pitch_number
    
    tmatrix_list <- tmatrix_sing(batters, pitcher, stadium, home, temp, date,
                                 away, inning, pitch_ct)
    
  }
  
  return(tmatrix_list)
  
}

# Uses Markov chains to get run-scoring probability distributions for a half-inning

markov_half_inning <- function(idx = 1, tmatrix_list = markov_matrices(), 
                               state = State$new(top = FALSE)) {
  
  # Initializing 21x25 scorekeeping matrix
  # 21 rows because we assume teams can score from 0 to 20 runs in a game
  # 25 columns because there are 25 unique states (including end of inning)
  
  scores <- matrix(0, 21, 25)
  
  # This corresponds to the starting state
  # 1 -> 8 correspond to no outs, 9 -> 16 are one out, 17 -> 24 two outs
  # In order within each: ---, 1--, -2-, --3, 12-, 1-3, -23, 123
  # ex. 2 corresponds to no outs, runner on first
  # ex. 13 corresponds to one out, runners on first and second
  # ex. 24 corresponds to two outs, bases loaded
  # The math to utilize this for the starting state is below
  
  state_input <- 1 + 8*state$outs + state$on_1b + 2*state$on_2b + 3*state$on_3b +
    ((state$on_1b & state$on_2b) | (state$on_1b & state$on_3b) | (state$on_2b & state$on_3b))
  
  scores[1, state_input] <- 1
  
  # Algorithm from:
  # https://pdfs.semanticscholar.org/563d/11f4baec14278357149a9726677453ba79a2.pdf
  
  while (abs(sum(scores[,25]) - 1) > EPSILON) {
    
    # Resets batting order index in case we go back to the top of the lineup 
    
    idx <- idx %% length(tmatrix_list)
    
    if (idx == 0) {
      idx <- 9
    }
    
    # Creates scoring probability matrices
    # Does this manually unfortunately, definitely a better way
    
    t_matrix <- tmatrix_list[[idx]]
    
    # This constructs run-scoring transition probabilities 
    # Does so for each possible number of runs on a given play
    # ex. Non-zero p_1 entries indicate a non_zero prob. of scoring 1 run on that transition
    
    p_1 <- matrix(0, 25, 25)
    p_1[1,1] <- t_matrix[1,1]
    p_1[2:4, 2:4] <- t_matrix[2:4, 2:4]
    p_1[5:7, 5:7] <- t_matrix[5:7, 5:7]
    p_1[8,8] <- t_matrix[8,8]
    p_1[9,9] <- t_matrix[9,9]
    p_1[10:12, 10:12] <- t_matrix[10:12, 10:12]
    p_1[13:15, 13:15] <- t_matrix[13:15, 13:15]
    p_1[16,16] <- t_matrix[16,16]
    p_1[17,17] <- t_matrix[17,17]
    p_1[18:20, 18:20] <- t_matrix[18:20, 18:20]
    p_1[21:23, 21:23] <- t_matrix[21:23, 21:23]
    p_1[24,24] <- t_matrix[24,24]
    
    p_2 <- matrix(0, 25, 25)
    p_2[2:4,1] <- t_matrix[2:4,1]
    p_2[5:7,2:4] <- t_matrix[5:7,2:4]
    p_2[8,5:7] <- t_matrix[8,5:7]
    p_2[10:12,9] <- t_matrix[10:12,9]
    p_2[13:15,10:12] <- t_matrix[13:15,10:12]
    p_2[16,13:15] <- t_matrix[16,13:15]
    p_2[18:20,17] <- t_matrix[18:20,17]
    p_2[21:23,18:20] <- t_matrix[21:23,18:20]
    p_2[24,21:23] <- t_matrix[24,21:23]
    
    p_3 <- matrix(0, 25, 25)
    p_3[5:7, 1] <- t_matrix[5:7, 1]
    p_3[8, 2:4] <- t_matrix[8, 2:4]
    p_3[13:15, 9] <- t_matrix[13:15, 9]
    p_3[16, 10:12] <- t_matrix[16, 10:12]
    p_3[21:23, 17] <- t_matrix[21:23, 17]
    p_3[24, 18:20] <- t_matrix[24, 18:20]
    
    p_4 <- matrix(0, 25, 25)
    p_4[8, 1] <- t_matrix[8, 1]
    p_4[16, 9] <- t_matrix[16, 9]
    p_4[24, 17] <- t_matrix[24, 17]
    
    # Creates matrix w/ probs. of scoring 0 runs from the others given
    
    p_0 <- t_matrix - p_1 - p_2 - p_3 - p_4
    
    # Creates empty 21x25 matrix
    
    temp <- matrix(0, 21, 25)
    
    # Adds expected runs to matrix, using same algorithm
    
    for (j in 1:21) {
      
      if (j == 1) {
        row <- scores[j,] %*% p_0
      }
      
      else if (j == 2) {
        row <- scores[j,] %*% p_0 + scores[j-1,] %*% p_1
      }
      
      else if (j == 3) {
        row <- scores[j,] %*% p_0 + scores[j-1,] %*% p_1 + scores[j-2,] %*% p_2
      }
      
      else if (j == 4) {
        row <- scores[j,] %*% p_0 + scores[j-1,] %*% p_1 + scores[j-2,] %*% p_2 +
          scores[j-3,] %*% p_3
      }
      
      else {
        row <- scores[j,] %*% p_0 + scores[j-1,] %*% p_1 + scores[j-2,] %*% p_2 +
          scores[j-3,] %*% p_3 + scores[j-4,] %*% p_4
      }
      
      temp[j,] <- row
      
    }
    
    scores <- temp
    
    idx <- idx + 1
    
  }
  
  # Turns output into a tibble to make output look presentable
  
  run_prob <- scores[,25]
  runs <- tibble(run_prob) %>% 
    mutate("Expected Runs Scored" = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
                                      13, 14, 15, 16, 17, 18, 19, 20),
           "Probability" = run_prob) %>% 
    select("Expected Runs Scored", "Probability")
  
  # Gets expected runs
  
  exp_runs <- runs %>% 
    mutate(prod = `Expected Runs Scored` * Probability) %>% 
    select(prod) %>% 
    sum() %>%
    round(digits = 5)
  
  runs <- runs %>% 
    mutate("Expected Runs Scored" = c("0", "1", "2", "3", "4", "5", "6", "7", "8", 
                                      "9", "10", "11", "12", "13", "14", "15", 
                                      "16", "17", "18", "19", "20")) %>% 
    add_row("Expected Runs Scored" = "7+",
            "Probability" = sum(select(slice(runs, 8:21), "Probability"))) %>% 
    slice(c(1:7, 22))

  return(list(paste("Expected Runs:", exp_runs), runs))
  
}

# Main function

main <- function() {

  # Creates default situation for prediction

  results <- markov_half_inning()
  
  print(results[[1]])
  print(results[[2]])

}

if (sys.nframe() == 0L) {
  
  main()
  
}
