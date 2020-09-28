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

library(wordspace)
library(tidyverse)
source("examples/common.R")
source("markov/tmatrix.R")

# The error in the probability
# Exits when difference between the sum of all run-scoring probabilities and 1 is < EPSILON

EPSILON <- 1 / 100000

# Uses Markov chains to get run-scoring probability distributions for a half-inning

markov_half_inning <- function(idx, info, state = State$new(top = FALSE)) {
  
  # Assigns values from function
  # Also creates a list of the 24 possible batting states
  
  batters <- info[[1]]
  pitcher <- info[[2]]
  stadium <- info[[3]]
  home <- info[[4]]
  temp <- info[[5]]
  away <- state$top
  inning <- state$inning
  pitch_ct <- state$pitch_number
  
  # List of the transition matrices for the nine batters
  
  tmatrix_list <- tmatrix_sing(batters, pitcher, stadium, home, temp, away,
                               inning, pitch_ct)

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
    mutate("Expected Runs Scored" = c("0", "1", "2", "3", "4", "5", "6", "7", 
                                      "8", "9", "10", "11", "12", "13", "14", 
                                      "15", "16", "17", "18", "19", "20"),
           "Probability" = run_prob) %>% 
    select("Expected Runs Scored", "Probability")
  
  runs <- runs %>% 
    add_row("Expected Runs Scored" = "7+",
            "Probability" = sum(select(slice(runs, 8:21), "Probability"))) %>% 
    slice(c(1:7, 22))

  return(runs)
  
}

# Creating function for dynamic user input
# Can be called from within RStudio or from the command line

main <- function() {
  
  # Creates default situation for prediction
  
  batters <- c("Mookie Betts", "Max Muncy", "Justin Turner", "Cody Bellinger", 
               "Corey Seager", "AJ Pollock", "Joc Pederson", "Austin Barnes", 
               "Gavin Lux")
  
  pitcher <- "Chris Paddack"
  
  stadium <- "Dodger Stadium"
  
  home <- "Dodgers"
  
  temp <- 75
  
  state <- State$new(top = FALSE)
  
  info <- list(batters, pitcher, stadium, home, temp)
  
  # Has two different options: default matchup or custom matchup
  
  if (interactive()) {
    
    # Asks for user input on matchup type
    
    cat("Run default matchup of Dodgers batting vs. Chris Paddack in the 
        bottom of the first inning? (TRUE or FALSE) ")
    default <- as.logical(readLines(con = stdin(), 1))
    
    # Checks to make sure a logical value was actually imputed
    
    while (is.na(default)) {
      
      cat("Run default matchup of Dodgers batting vs. Chris Paddack in the 
          bottom of the first inning? (TRUE or FALSE) ")
      default <- as.logical(readLines(con = stdin(), 1))
      
    }
    
    # Runs default matchup, starting at the top of the order
    
    if (default) {
      
      markov_half_inning(1, info)
      
    }
    
    else {
      
      # Asks whether user wants to input a new lineup
      
      cat("New lineup? (TRUE or FALSE) ")
      new_lineup <- as.logical(readLines(con = stdin(), 1))
      
      # Checks to make sure a logical value was actually imputed
      
      while (is.na(new_lineup)) {
        
        cat("New lineup? (TRUE or FALSE) ")
        new_lineup <- as.logical(readLines(con = stdin(), 1))
        
      }
      
      # Gets inputs for new lineup if user wants
      
      if (new_lineup) {
        
        cat("Leadoff hitter: ")
        bat1 <- readLines(con = stdin(), 1)
        
        # Checks to make sure a real player was imputed
        
        while (is.null(sing$GetPlayers(name = bat1))) {
          
          cat("Leadoff hitter: ")
          bat1 <- readLines(con = stdin(), 1)
          
        }
        
        cat("Second hitter: ")
        bat2 <- readLines(con = stdin(), 1)
        
        # Same check
        
        while (is.null(sing$GetPlayers(name = bat2))) {
          
          cat("Second hitter: ")
          bat2 <- readLines(con = stdin(), 1)
          
        }
        
        cat("Third hitter: ")
        bat3 <- readLines(con = stdin(), 1)
        
        while (is.null(sing$GetPlayers(name = bat3))) {
          
          cat("Third hitter: ")
          bat3 <- readLines(con = stdin(), 1)
          
        }
        
        cat("Cleanup hitter: ")
        bat4 <- readLines(con = stdin(), 1)
        
        while (is.null(sing$GetPlayers(name = bat4))) {
          
          cat("Cleanup hitter: ")
          bat4 <- readLines(con = stdin(), 1)
          
        }
        
        cat("Fifth hitter: ")
        bat5 <- readLines(con = stdin(), 1)
        
        while (is.null(sing$GetPlayers(name = bat5))) {
          
          cat("Fifth hitter: ")
          bat5 <- readLines(con = stdin(), 1)
          
        }
        
        cat("Sixth hitter: ")
        bat6 <- readLines(con = stdin(), 1)
        
        while (is.null(sing$GetPlayers(name = bat6))) {
          
          cat("Sixth hitter: ")
          bat6 <- readLines(con = stdin(), 1)
          
        }
        
        cat("Seventh hitter: ")
        bat7 <- readLines(con = stdin(), 1)
        
        while (is.null(sing$GetPlayers(name = bat7))) {
          
          cat("Seventh hitter: ")
          bat7 <- readLines(con = stdin(), 1)
          
        }
        
        cat("Eighth hitter: ")
        bat8 <- readLines(con = stdin(), 1)
        
        while (is.null(sing$GetPlayers(name = bat8))) {
          
          cat("Eighth hitter: ")
          bat8 <- readLines(con = stdin(), 1)
          
        }
        
        cat("Ninth hitter: ")
        bat9 <- readLines(con = stdin(), 1)
        
        while (is.null(sing$GetPlayers(name = bat9))) {
          
          cat("Ninth hitter: ")
          bat9 <- readLines(con = stdin(), 1)
          
        }
        
        # Assigns to already-created variable with lineup
        
        batters <- c(bat1, bat2, bat3, bat4, bat5, bat6, bat7, bat8, bat9)
        
      }
      
      # Asks if user wants a new pitcher
      
      cat("New pitcher? (TRUE or FALSE) " )
      new_pitcher <- as.logical(readLines(con = stdin(), 1))
      
      # Checks to make sure a logical value was actually imputed
      
      while (is.na(new_pitcher)) {
        
        cat("New pitcher? (TRUE or FALSE) ")
        new_pitcher <- as.logical(readLines(con = stdin(), 1))
        
      }
      
      # Gets a new pitcher if the user said so
      
      if (new_pitcher) {
        
        cat("Pitcher: ")
        pitcher <- readLines(con = stdin(), 1)
        
        # Does the same check as above to make sure the input is correct
        
        while (is.null(sing$GetPlayers(name = pitcher))) {
          
          cat("Pitcher: ")
          pitcher <- readLines(con = stdin(), 1)
          
        }
        
      }
      
      # Asks if user wants a new environment
      
      cat("New environment? (TRUE or FALSE) ")
      new_envmt <- as.logical(readLines(con = stdin(), 1))
      
      # Same logical check as above
      
      while (is.na(new_envmt)) {
        
        cat("New environment? (TRUE or FALSE) ")
        new_envmt <- as.logical(readLines(con = stdin(), 1))
        
      }
      
      # Creates a new environment (stadium, home team, temperature) if requested
      
      if (new_envmt) {
        
        cat("Stadium: ")
        stadium <- readLines(con = stdin(), 1)
        
        # Null check for stadium input
        
        while (is.null(sing$GetVenues(stadium.name = stadium)[[1]])) {
          
          cat("Stadium: ")
          stadium <- readLines(con = stdin(), 1)
          
        }
        
        cat("Home Team: ")
        home <- readLines(con = stdin(), 1)
        
        # Null check for stadium input
        
        while (is.null(sing$GetTeams(name = home)[[1]])) {
          
          cat("Stadium: ")
          stadium <- readLines(con = stdin(), 1)
          
        }
        
        cat("Temperature: ")
        temp <- as.numeric(readLines(con = stdin(), 1))
        
        # NA check for temperature input
        
        while (is.na(temp)) {
          
          cat("Temperature: ")
          temp <- as.numeric(readLines(con = stdin(), 1))
          
        }
        
      }
      
      # Asks if user wants a new state
      
      cat("New state? (TRUE or FALSE) ")
      new_state <- as.logical(readLines(con = stdin(), 1))
      
      # Repeated logical check
      
      while (is.na(new_state)) {
        
        cat("New state? (TRUE or FALSE) ")
        new_state <- as.logical(readLines(con = stdin(), 1))
        
      }
      
      # Gets inputs for new state
      
      if (new_state) {
        
        # Asks if the user wants to change the inning
        
        cat("New inning? (TRUE or FALSE) ")
        new_inning <- as.logical(readLines(con = stdin(), 1))
        
        while (is.na(new_inning)) {
          
          cat("New inning? (TRUE or FALSE) ")
          new_inning <- as.logical(readLines(con = stdin(), 1))
          
        }
        
        if (new_inning) {
          
          cat("Inning: ")
          state$inning <- as.integer(readLines(con = stdin(), 1))
          
          while (is.na(state$inning) | state$inning < 1) {
            
            cat("Inning: ")
            state$inning <- as.numeric(readLines(con = stdin(), 1))
            
          }
          
        }
        
        # Asks if the user wants to change the half of the inning
        
        cat("Top of the inning? (TRUE or FALSE) ")
        state$top <- as.logical(readLines(con = stdin(), 1))
        
        while (is.na(state$top)) {
          
          cat("Top of the inning? (TRUE or FALSE) ")
          state$top <- as.logical(readLines(con = stdin(), 1))
          
        }
        
        # Asks if the user wants to change the number of outs
        
        cat("Change outs? (TRUE or FALSE) ")
        new_outs <- as.logical(readLines(con = stdin(), 1))
        
        while (is.na(new_outs)) {
          
          cat("Change outs? (TRUE or FALSE) ")
          new_outs <- as.logical(readLines(con = stdin(), 1))
          
        }
        
        if (new_outs) {
          
          cat("Outs: ")
          state$outs <- as.integer(readLines(con = stdin(), 1))
          
          while (is.na(state$outs) | (state$outs < 0 | state$outs > 2)) {
            
            cat("Inning: ")
            state$outs <- as.integer(readLines(con = stdin(), 1))
            
          }
          
        }
        
        # Asks if the user wants to change the baserunner state
        
        cat("New baserunner state? (TRUE or FALSE) ")
        new_br <- as.logical(readLines(con = stdin(), 1))
        
        while (is.na(new_br)) {
          
          cat("New baserunner state? (TRUE or FALSE) ")
          new_br <- as.logical(readLines(con = stdin(), 1))
          
        }
        
        # Changes baserunner state
        
        if (new_br) {
          
          # Goes through each base to ask whether there's a runner
          
          cat("Runner on first? (TRUE or FALSE) ")
          state$on_1b <- as.logical(readLines(con = stdin(), 1))
          
          while (is.na(state$on_1b)) {
            
            cat("Runner on first? (TRUE or FALSE) ")
            state$on_1b <- as.logical(readLines(con = stdin(), 1))
            
          }
          
          cat("Runner on second? (TRUE or FALSE) ")
          state$on_2b <- as.logical(readLines(con = stdin(), 1))
          
          while (is.na(state$on_2b)) {
            
            cat("Runner on second? (TRUE or FALSE) ")
            state$on_2b <- as.logical(readLines(con = stdin(), 1))
            
          }
          
          cat("Runner on third? (TRUE or FALSE) ")
          state$on_3b <- as.logical(readLines(con = stdin(), 1))
          
          while (is.na(state$on_3b)) {
            
            cat("Runner on third? (TRUE or FALSE) ")
            state$on_3b <- as.logical(readLines(con = stdin(), 1))
            
          }
          
        }
        
      }
      
      # Asks where to start in the batting order
      
      cat("Where to start in the order? ")
      order_start <- as.integer(readLines(con = stdin(), 1))
      
      # More checks
      
      while (is.na(order_start) | (order_start < 1 | order_start > 9)) {
        
        cat("Where to start in the order? ")
        order_start <- as.integer(readLines(con = stdin(), 1))
        
      }
      
      info <- list(batters, pitcher, stadium, home, temp)
      
      markov_half_inning(order_start, info, state)
      
    }
    
  }
  
  # When not run in the R console
  
  else {
    
    # Asks for user input on matchup type
    
    cat("Run default matchup of Dodgers batting vs. Chris Paddack in the 
        bottom of the first inning? (TRUE or FALSE) ")
    default <- as.logical(readLines(file("stdin"), 1))
    
    # Checks to make sure a logical value was actually imputed
    
    while (is.na(default)) {
      
      cat("Run default matchup of Dodgers batting vs. Chris Paddack in the 
          bottom of the first inning? (TRUE or FALSE) ")
      default <- as.logical(readLines(file("stdin"), 1))
      
    }
    
    # Runs default matchup, starting at the top of the order
    
    if (default) {
      
      markov_half_inning(1, info)
      
    }
    
    else {
      
      # Asks whether user wants to input a new lineup
      
      cat("New lineup? (TRUE or FALSE) ")
      new_lineup <- as.logical(readLines(file("stdin"), 1))
      
      # Checks to make sure a logical value was actually imputed
      
      while (is.na(new_lineup)) {
        
        cat("New lineup? (TRUE or FALSE) ")
        new_lineup <- as.logical(readLines(file("stdin"), 1))
        
      }
      
      # Gets inputs for new lineup if user wants
      
      if (new_lineup) {
        
        cat("Leadoff hitter: ")
        bat1 <- readLines(file("stdin"), 1)
        
        # Checks to make sure a real player was imputed
        
        while (is.null(sing$GetPlayers(name = bat1))) {
          
          cat("Leadoff hitter: ")
          bat1 <- readLines(file("stdin"), 1)
          
        }
        
        cat("Second hitter: ")
        bat2 <- readLines(file("stdin"), 1)
        
        # Same check
        
        while (is.null(sing$GetPlayers(name = bat2))) {
          
          cat("Second hitter: ")
          bat2 <- readLines(file("stdin"), 1)
          
        }
        
        cat("Third hitter: ")
        bat3 <- readLines(file("stdin"), 1)
        
        while (is.null(sing$GetPlayers(name = bat3))) {
          
          cat("Third hitter: ")
          bat3 <- readLines(file("stdin"), 1)
          
        }
        
        cat("Cleanup hitter: ")
        bat4 <- readLines(file("stdin"), 1)
        
        while (is.null(sing$GetPlayers(name = bat4))) {
          
          cat("Cleanup hitter: ")
          bat4 <- readLines(file("stdin"), 1)
          
        }
        
        cat("Fifth hitter: ")
        bat5 <- readLines(file("stdin"), 1)
        
        while (is.null(sing$GetPlayers(name = bat5))) {
          
          cat("Fifth hitter: ")
          bat5 <- readLines(file("stdin"), 1)
          
        }
        
        cat("Sixth hitter: ")
        bat6 <- readLines(file("stdin"), 1)
        
        while (is.null(sing$GetPlayers(name = bat6))) {
          
          cat("Sixth hitter: ")
          bat6 <- readLines(file("stdin"), 1)
          
        }
        
        cat("Seventh hitter: ")
        bat7 <- readLines(file("stdin"), 1)
        
        while (is.null(sing$GetPlayers(name = bat7))) {
          
          cat("Seventh hitter: ")
          bat7 <- readLines(file("stdin"), 1)
          
        }
        
        cat("Eighth hitter: ")
        bat8 <- readLines(file("stdin"), 1)
        
        while (is.null(sing$GetPlayers(name = bat8))) {
          
          cat("Eighth hitter: ")
          bat8 <- readLines(file("stdin"), 1)
          
        }
        
        cat("Ninth hitter: ")
        bat9 <- readLines(file("stdin"), 1)
        
        while (is.null(sing$GetPlayers(name = bat9))) {
          
          cat("Ninth hitter: ")
          bat9 <- readLines(file("stdin"), 1)
          
        }
        
        # Assigns to already-created variable with lineup
        
        batters <- c(bat1, bat2, bat3, bat4, bat5, bat6, bat7, bat8, bat9)
        
      }
      
      # Asks if user wants a new pitcher
      
      cat("New pitcher? (TRUE or FALSE) " )
      new_pitcher <- as.logical(readLines(file("stdin"), 1))
      
      # Checks to make sure a logical value was actually imputed
      
      while (is.na(new_pitcher)) {
        
        cat("New pitcher? (TRUE or FALSE) ")
        new_pitcher <- as.logical(readLines(file("stdin"), 1))
        
      }
      
      # Gets a new pitcher if the user said so
      
      if (new_pitcher) {
        
        cat("Pitcher: ")
        pitcher <- readLines(file("stdin"), 1)
        
        # Does the same check as above to make sure the input is correct
        
        while (is.null(sing$GetPlayers(name = pitcher))) {
          
          cat("Pitcher: ")
          pitcher <- readLines(file("stdin"), 1)
          
        }
        
      }
      
      # Asks if user wants a new environment
      
      cat("New environment? (TRUE or FALSE) ")
      new_envmt <- as.logical(readLines(file("stdin"), 1))
      
      # Same logical check as above
      
      while (is.na(new_envmt)) {
        
        cat("New environment? (TRUE or FALSE) ")
        new_envmt <- as.logical(readLines(file("stdin"), 1))
        
      }
      
      # Creates a new environment (stadium, home team, temperature) if requested
      
      if (new_envmt) {
        
        cat("Stadium: ")
        stadium <- readLines(file("stdin"), 1)
        
        # Null check for stadium input
        
        while (is.null(sing$GetVenues(stadium.name = stadium)[[1]])) {
          
          cat("Stadium: ")
          stadium <- readLines(file("stdin"), 1)
          
        }
        
        cat("Home Team: ")
        home <- readLines(file("stdin"), 1)
        
        # Same check for home input
        
        while (is.null(sing$GetTeams(name = home)[[1]])) {
          
          cat("Home Team: ")
          home <- readLines(file("stdin"), 1)
          
        }
        
        cat("Temperature: ")
        temp <- as.numeric(readLines(file("stdin"), 1))
        
        # NA check for temperature input
        
        while (is.na(temp)) {
          
          cat("Temperature: ")
          temp <- as.numeric(readLines(file("stdin"), 1))
          
        }
        
      }
      
      # Asks if user wants a new state
      
      cat("New state? (TRUE or FALSE) ")
      new_state <- as.logical(readLines(file("stdin"), 1))
      
      # Repeated logical check
      
      while (is.na(new_state)) {
        
        cat("New state? (TRUE or FALSE) ")
        new_state <- as.logical(readLines(file("stdin"), 1))
        
      }
      
      # Gets inputs for new state
      
      if (new_state) {
        
        # Asks if the user wants to change the inning
        
        cat("New inning? (TRUE or FALSE) ")
        new_inning <- as.logical(readLines(file("stdin"), 1))
        
        while (is.na(new_inning)) {
          
          cat("New inning? (TRUE or FALSE) ")
          new_inning <- as.logical(readLines(file("stdin"), 1))
          
        }
        
        if (new_inning) {
          
          cat("Inning: ")
          state$inning <- as.integer(readLines(file("stdin"), 1))
          
          while (is.na(state$inning) | state$inning < 1) {
            
            cat("Inning: ")
            state$inning <- as.numeric(readLines(file("stdin"), 1))
            
          }
          
        }
        
        # Asks if the user wants to change the half of the inning
        
        cat("Top of the inning? (TRUE or FALSE) ")
        state$top <- as.logical(readLines(file("stdin"), 1))
        
        while (is.na(state$top)) {
          
          cat("Top of the inning? (TRUE or FALSE) ")
          state$top <- as.logical(readLines(file("stdin"), 1))
          
        }
        
        # Asks if the user wants to change the number of outs
        
        cat("Change outs? (TRUE or FALSE) ")
        new_outs <- as.logical(readLines(file("stdin"), 1))
        
        while (is.na(new_outs)) {
          
          cat("Change outs? (TRUE or FALSE) ")
          new_outs <- as.logical(readLines(file("stdin"), 1))
          
        }
        
        if (new_outs) {
          
          cat("Outs: ")
          state$outs <- as.integer(readLines(file("stdin"), 1))
          
          while (is.na(state$outs) | (state$outs < 0 | state$outs > 2)) {
            
            cat("Outs: ")
            state$outs <- as.integer(readLines(file("stdin"), 1))
            
          }
          
        }
        
        # Asks if the user wants to change the baserunner state
        
        cat("New baserunner state? (TRUE or FALSE) ")
        new_br <- as.logical(readLines(file("stdin"), 1))
        
        while (is.na(new_br)) {
          
          cat("New baserunner state? (TRUE or FALSE) ")
          new_br <- as.logical(readLines(file("stdin"), 1))
          
        }
        
        # Changes baserunner state
        
        if (new_br) {
          
          # Goes through each base to ask whether there's a runner
          
          cat("Runner on first? (TRUE or FALSE) ")
          state$on_1b <- as.logical(readLines(file("stdin"), 1))
          
          while (is.na(state$on_1b)) {
            
            cat("Runner on first? (TRUE or FALSE) ")
            state$on_1b <- as.logical(readLines(file("stdin"), 1))
            
          }
          
          cat("Runner on second? (TRUE or FALSE) ")
          state$on_2b <- as.logical(readLines(file("stdin"), 1))
          
          while (is.na(state$on_2b)) {
            
            cat("Runner on second? (TRUE or FALSE) ")
            state$on_2b <- as.logical(readLines(file("stdin"), 1))
            
          }
          
          cat("Runner on third? (TRUE or FALSE) ")
          state$on_3b <- as.logical(readLines(file("stdin"), 1))
          
          while (is.na(state$on_3b)) {
            
            cat("Runner on third? (TRUE or FALSE) ")
            state$on_3b <- as.logical(readLines(file("stdin"), 1))
            
          }
          
        }
        
      }
      
      # Asks where to start in the batting order
      
      cat("Where to start in the order? ")
      order_start <- as.integer(readLines(file("stdin"), 1))
      
      # More checks
      
      while (is.na(order_start) | (order_start < 1 | order_start > 9)) {
        
        cat("Where to start in the order? ")
        order_start <- as.integer(readLines(file("stdin"), 1))
        
      }
      
      info <- list(batters, pitcher, stadium, home, temp)
      
      markov_half_inning(order_start, info, state)
      
    }
    
  }
  
}

main()
