# Functions (in separate file) to get transition matrix

source("examples/common.R")

# Default probabilities for certain events
# Probability of a one-base error

p_error_1b <- 0.85
p_error_2b <- 1 - p_error_1b

# Probability of scoring from first on a double

p_2b_score_from_1b <- 0.38
p_2b_go_to_3b <- 1 - p_2b_score_from_1b

# Probability of going first to third on a single

p_1b_first_to_third <- 0.3
p_1b_first_to_second <- 1 - p_1b_first_to_third

# Probability of going second to third on a field out

p_fo_second_to_third <- 0.15
p_fo_second_stay <- 1 - p_fo_second_to_third

# Function to fill in specific element of the transition matrix
# The ellipses are whatever values are entered after the matrix/row/col info

elmt_fill <- function(matrix, row, col, ...) {
  
  matrix[row, col] <- sum(...)
  return(matrix)
  
}

# Standard function (passing in values explicitly)
# Inputs:
# 21 different event probabilities
# Defaults are overall league averages for the 2020 season, as of 9/24/20
# Probs. are generally just # occurrences / # situations
# Situation-dependent ones are restricted to states where it's actually possible
# i.e., the PA denominator is not the same for TP as for SO
# For errors, the denominator is number of chances

tmatrix_std <- function(bb_exp = 5697 / 62087,
                        ci_exp = 33 / 62087,
                        double_exp = 2635 / 62087,
                        dp_exp = 137 / 62087,
                        e_exp = 955 / 62087,
                        f_out_exp = 23875 / 62087,
                        fc_exp = 135 / 62087,
                        fc_o_exp = 110 / 62087,
                        fo_exp = 1195 / 62087,
                        gdp_exp = 1146 / 62087,
                        hbp_exp = 772 / 62087,
                        hr_exp = 2156 / 62087,
                        ibb_exp = 187 / 62087,
                        sf_exp = 370 / 62087,
                        sf_dp_exp = 2 / 62087,
                        sh_exp = 112 / 62087,
                        single_exp = 8439 / 62087,
                        so_exp = 14540 / 62087,
                        so_dp_exp = 42 / 62087,
                        tp_exp = 1 / 62087,
                        triple_exp = 225 / 62087) {
  
  # Creating transition matrix (25x25, all zeroes as default)
  
  tmatrix <- matrix(0, 25, 25)
  
  # Probability of --- to ---, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 1, 1, hr_exp)

  # Probability of --- to 1--, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 1, 2, single_exp, hbp_exp, ci_exp, 
                       bb_exp, ibb_exp, p_error_1b * e_exp)
  
  # Probability of --- to -2-, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 1, 3, double_exp, p_error_2b * e_exp)
  
  # Probability of --- to --3, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 1, 4, triple_exp)
  
  # Probability of --- to ---, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 1, 9, f_out_exp, so_exp)
  
  # Probability of 1-- to ---, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 2, 1, hr_exp)
  
  # Probability of 1-- to -2-, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 2, 3, p_2b_score_from_1b * double_exp)

  # Probability of 1-- to --3, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 2, 4, triple_exp)

  # Probability of 1-- to 12-, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 2, 5, hbp_exp, ci_exp, bb_exp, ibb_exp, fc_exp,
                       p_1b_first_to_second * single_exp, p_error_1b * e_exp)

  # Probability of 1-- to 1-3, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 2, 6, p_1b_first_to_third * single_exp)

  # Probability of 1-- to -23, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 2, 7, p_2b_go_to_3b * double_exp, p_error_2b * e_exp)

  # Probability of 1-- to 1--, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 2, 10, fc_o_exp, fo_exp, so_exp, f_out_exp)

  # Probability of 1-- to -2-, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 2, 11, sh_exp)

  # Probability of 1-- to ---, starting with no outs and two-out increase
  
  tmatrix <- elmt_fill(tmatrix, 2, 17, dp_exp, gdp_exp, sf_dp_exp, so_dp_exp)

  # Probability of -2- to ---, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 3, 1, hr_exp)

  # Probability of -2- to 1--, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 3, 2, single_exp)

  # Probability of -2- to -2-, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 3, 3, double_exp, p_error_2b * e_exp)

  # Probability of -2- to --3, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 3, 4, triple_exp)

  # Probability of -2- to 12-, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 3, 5, bb_exp, ibb_exp, hbp_exp, ci_exp)

  # Probability of -2- to 1-3, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 3, 6, fc_exp, p_error_1b * e_exp)

  # Probability of -2- to 1--, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 3, 10, fc_o_exp)

  # Probability of -2- to -2-, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 3, 11, so_exp, p_fo_second_stay * f_out_exp)

  # Probability of -2- to --3, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 3, 12, sh_exp, p_fo_second_to_third * f_out_exp)

  # Probability of -2- to ---, starting with no outs and two-out increase
  
  tmatrix <- elmt_fill(tmatrix, 3, 17, dp_exp, gdp_exp, sf_dp_exp, so_dp_exp)

  # Probability of --3 to ---, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 4, 1, hr_exp)

  # Probability of --3 to 1--, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 4, 2, single_exp, fc_exp, p_error_1b * e_exp)

  # Probability of --3 to -2-, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 4, 3, double_exp, p_error_2b * e_exp)

  # Probability of --3 to --3, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 4, 4, triple_exp)

  # Probability of --3 to 1-3, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 4, 6, bb_exp, ibb_exp, hbp_exp, ci_exp)

  # Probability of --3 to ---, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 4, 9, sf_exp, f_out_exp, fc_o_exp, sh_exp)

  # Probability of --3 to --3, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 4, 12, so_exp)

  # Probability of --3 to ---, starting with no outs and two-out increase
  
  tmatrix <- elmt_fill(tmatrix, 4, 17, dp_exp, gdp_exp, sf_dp_exp, so_dp_exp)

  # Probability of 12- to ---, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 5, 1, hr_exp)

  # Probability of 12- to -2-, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 5, 3, p_2b_score_from_1b * double_exp)

  # Probability of 12- to --3, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 5, 4, triple_exp)

  # Probability of 12- to 12-, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 5, 5, p_1b_first_to_second * single_exp)

  # Probability of 12- to 1-3, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 5, 6, p_1b_first_to_third * single_exp)

  # Probability of 12- to -23, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 5, 7, p_2b_go_to_3b * double_exp, p_error_2b * e_exp)

  # Probability of 12- to 123, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 5, 8, bb_exp, ibb_exp, hbp_exp, ci_exp, fc_exp,
                       p_error_1b * e_exp)

  # Probability of 12- to 12-, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 5, 13, f_out_exp, so_exp)

  # Probability of 12- to 1-3, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 5, 14, fo_exp, fc_o_exp)

  # Probability of 12- to -23, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 5, 15, sh_exp)

  # Probability of 12- to -2-, starting with no outs and two-out increase
  
  tmatrix <- elmt_fill(tmatrix, 5, 19, so_dp_exp, dp_exp)

  # Probability of 12- to --3, starting with no outs and two-out increase
  
  tmatrix <- elmt_fill(tmatrix, 5, 20, gdp_exp)

  # Probability of 12- to end of inning, starting with no outs
  
  tmatrix <- elmt_fill(tmatrix, 5, 25, tp_exp)

  # Probability of 1-3 to ---, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 6, 1, hr_exp)

  # Probability of 1-3 to -2-, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 6, 3, p_2b_score_from_1b * double_exp)

  # Probability of 1-3 to --3, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 6, 4, triple_exp)

  # Probability of 1-3 to 12-, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 6, 5, fc_exp, p_1b_first_to_second * single_exp,
                       p_error_1b * e_exp)

  # Probability of 1-3 to 1-3, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 6, 6, p_1b_first_to_third * single_exp)

  # Probability of 1-3 to -23, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 6, 7, p_2b_go_to_3b * double_exp, p_error_2b * e_exp)

  # Probability of 1-3 to 123, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 6, 8, bb_exp, ibb_exp, hbp_exp, ci_exp)

  # Probability to 1-3 to 1--, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 6, 10, sf_exp, fc_o_exp, fo_exp)

  # Probability of 1-3 to -2-, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 6, 11, sh_exp)

  # Probability of 1-3 to 1-3, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 6, 14, so_exp, f_out_exp)

  # Probability of 1-3 to ---, starting with no outs and two-out increase
  
  tmatrix <- elmt_fill(tmatrix, 6, 17, dp_exp, gdp_exp)

  # Probability of 1-3 to -2-, starting with no outs and two-out increase
  
  tmatrix <- elmt_fill(tmatrix, 6, 19, sf_dp_exp)

  # Probability of 1-3 to --3, starting with no outs and two-out increase
  
  tmatrix <- elmt_fill(tmatrix, 6, 20, so_dp_exp)

  # Probability of 1-3 to end of inning, starting with no outs
  
  tmatrix <- elmt_fill(tmatrix, 6, 25, tp_exp)

  # Probability of -23 to ---, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 7, 1, hr_exp)

  # Probability of -23 to 1--, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 7, 2, single_exp)

  # Probability of -23 to -2-, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 7, 3, double_exp, p_error_2b * e_exp)

  # Probability of -23 to --3, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 7, 4, triple_exp)

  # Probability of -23 to 1-3, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 7, 6, fc_exp, p_error_1b * e_exp)

  # Probability of -23 to 123, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 7, 8, bb_exp, ibb_exp, hbp_exp, ci_exp)

  # Probability of -23 to -2-, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 7, 11, sf_exp, p_fo_second_stay * f_out_exp)

  # Probability of -23 to --3, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 7, 12, sh_exp, p_fo_second_to_third * f_out_exp)

  # Probability of -23 to 1-3, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 7, 14, fc_o_exp)

  # Probability of -23 to -23, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 7, 15, so_exp)

  # Probability of -23 to --3, starting with no outs and two-out increase
  
  tmatrix <- elmt_fill(tmatrix, 7, 20, sf_dp_exp, so_dp_exp, dp_exp, gdp_exp)

  # Probability of -23 to end of inning, starting with no outs
  
  tmatrix <- elmt_fill(tmatrix, 7, 25, tp_exp)

  # Probability of 123 to ---, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 8, 1, hr_exp)

  # Probability of 123 to -2-, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 8, 3, p_2b_score_from_1b * double_exp)

  # Probability of 123 to --3, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 8, 4, triple_exp)

  # Probability of 123 to 12-, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 8, 5, p_1b_first_to_second * single_exp)

  # Probability of 123 to 1-3, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 8, 6, p_1b_first_to_third * single_exp)

  # Probability of 123 to -23, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 8, 7, p_2b_go_to_3b * double_exp, p_error_2b * e_exp)

  # Probability of 123 to 123, starting with no outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 8, 8, bb_exp, ibb_exp, hbp_exp, ci_exp, fc_exp,
                       p_error_1b * e_exp)

  # Probability of 123 to 12-, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 8, 13, sf_exp, 0.5 * p_fo_second_stay * f_out_exp)

  # Probability of 123 to 1-3, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 8, 14, fc_o_exp, p_fo_second_to_third * f_out_exp)

  # Probability of 123 to -23, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 8, 15, sh_exp)

  # Probability of 123 to 123, starting with no outs and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 8, 16, so_exp, fo_exp, 0.5 * p_fo_second_stay * f_out_exp)

  # Probability of 123 to --3, starting with no outs and two-out increase
  
  tmatrix <- elmt_fill(tmatrix, 8, 20, gdp_exp)

  # Probability of 123 to 12-, starting with no outs and two-out increase
  
  tmatrix <- elmt_fill(tmatrix, 8, 21, so_dp_exp, dp_exp)

  # Probability of 123 to 1-3, starting with no outs and two-out increase
  
  tmatrix <- elmt_fill(tmatrix, 8, 22, sf_dp_exp)

  # Probability of 123 to end of inning, starting with no outs
  
  tmatrix <- elmt_fill(tmatrix, 8, 25, tp_exp)

  # Probability of --- to ---, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 9, 9, hr_exp)
  
  # Probability of --- to 1--, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 9, 10, single_exp, hbp_exp, ci_exp,
                       bb_exp, ibb_exp, p_error_1b * e_exp)

  # Probability of --- to -2-, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 9, 11, double_exp, p_error_2b * e_exp)

  # Probability of --- to --3, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 9, 12, triple_exp)

  # Probability of --- to ---, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 9, 17, f_out_exp, so_exp)

  # Probability of 1-- to ---, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 10, 9, hr_exp)

  # Probability of 1-- to -2-, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 10, 11, p_2b_score_from_1b * double_exp)

  # Probability of 1-- to --3, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 10, 12, triple_exp)

  # Probability of 1-- to 12-, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 10, 13, bb_exp, ibb_exp, hbp_exp, ci_exp, fc_exp,
                       p_1b_first_to_second * single_exp, p_error_1b * e_exp)

  # Probability of 1-- to 1-3, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 10, 14, p_1b_first_to_third * single_exp)

  # Probability of 1-- to -23, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 10, 15, p_2b_go_to_3b * double_exp,
                       p_error_2b * e_exp)

  # Probability of 1-- to 1--, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 10, 18, fc_o_exp, fo_exp, so_exp, f_out_exp)

  # Probability of 1-- to -2-, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 10, 19, sh_exp)

  # Probability of 1-- to end of inning, starting with one out
  
  tmatrix <- elmt_fill(tmatrix, 10, 25, dp_exp, gdp_exp, sf_dp_exp, so_dp_exp)

  # Probability of -2- to ---, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 11, 9, hr_exp)

  # Probability of -2- to 1--, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 11, 10, single_exp)

  # Probability of -2- to -2-, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 11, 11, double_exp, p_error_2b * e_exp)

  # Probability of -2- to --3, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 11, 12, triple_exp)

  # Probability of -2- to 12-, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 11, 13, bb_exp, ibb_exp, hbp_exp, ci_exp)

  # Probability of -2- to 1-3, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 11, 14, fc_exp, p_error_1b * e_exp)

  # Probability of -2- to 1--, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 11, 18, fc_o_exp)

  # Probability of -2- to -2-, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 11, 19, so_exp, p_fo_second_stay * f_out_exp)

  # Probability of -2- to --3, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 11, 20, p_fo_second_to_third * f_out_exp,
                       sh_exp)

  # Probability of -2- to end of inning, starting with one out
  
  tmatrix <- elmt_fill(tmatrix, 11, 25, dp_exp, gdp_exp, sf_dp_exp, so_dp_exp)

  # Probability of --3 to ---, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 12, 9, hr_exp)

  # Probability of --3 to 1--, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 12, 10, single_exp, fc_exp,
                       p_error_1b * e_exp)

  # Probability of --3 to -2-, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 12, 11, double_exp, p_error_2b * e_exp)

  # Probability of --3 to --3, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 12, 12, triple_exp)

  # Probability of --3 to 1-3, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 12, 14, bb_exp, ibb_exp, hbp_exp, ci_exp)

  # Probability of --3 to ---, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 12, 17, sf_exp, f_out_exp, sh_exp)

  # Probability of --3 to 1--, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 12, 18, fc_o_exp)

  # Probability of --3 to --3, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 12, 20, so_exp)

  # Probability of --3 to end of inning, starting with one out
  
  tmatrix <- elmt_fill(tmatrix, 12, 25, dp_exp, gdp_exp, sf_dp_exp, so_dp_exp)

  # Probability of 12- to ---, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 13, 9, hr_exp)

  # Probability of 12- to -2-, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 13, 11, p_2b_score_from_1b * double_exp)

  # Probability of 12- to --3, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 13, 12, triple_exp)

  # Probability of 12- to 12-, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 13, 13, p_1b_first_to_second * single_exp)

  # Probability of 12- to 1-3, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 13, 14, p_1b_first_to_third * single_exp)

  # Probability of 12- to -23, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 13, 15, p_2b_go_to_3b * double_exp,
                       p_error_2b * e_exp)

  # Probability of 12- to 123, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 13, 16, bb_exp, ibb_exp, hbp_exp, ci_exp,
                       fc_exp, p_error_1b * e_exp)

  # Probability of 12- to 12-, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 13, 21, f_out_exp, so_exp)

  # Probability of 12- to 1-3, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 13, 22, fo_exp, fc_o_exp)

  # Probability of 12- to -23, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 13, 23, sh_exp)

  # Probability of 12- to end of inning, starting with one out
  
  tmatrix <- elmt_fill(tmatrix, 13, 25, dp_exp, gdp_exp, sf_dp_exp, so_dp_exp)

  # Probability of 1-3 to ---, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 14, 9, hr_exp)

  # Probability of 1-3 to -2-, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 14, 11, p_2b_score_from_1b * double_exp)

  # Probability of 1-3 to --3, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 14, 12, triple_exp)

  # Probability of 1-3 to 12-, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 14, 13, fc_exp, p_error_1b * e_exp,
                       p_1b_first_to_second * single_exp)

  # Probability of 1-3 to 1-3, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 14, 14, p_1b_first_to_third * single_exp)

  # Probability of 1-3 to -23, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 14, 15, p_2b_go_to_3b * double_exp,
                       p_error_2b * e_exp)

  # Probability of 1-3 to 123, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 14, 16, bb_exp, ibb_exp, hbp_exp, ci_exp)

  # Probability to 1-3 to 1--, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 14, 18, sf_exp, fc_o_exp, fo_exp)
  
  # Probability of 1-3 to -2-, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 14, 19, sh_exp)

  # Probability of 1-3 to 1-3, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 14, 22, so_exp, f_out_exp)

  # Probability of 1-3 to end of inning, starting with one out
  
  tmatrix <- elmt_fill(tmatrix, 14, 25, dp_exp, gdp_exp, sf_dp_exp, so_dp_exp)

  # Probability of -23 to ---, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 15, 9, hr_exp)

  # Probability of -23 to 1--, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 15, 10, single_exp)

  # Probability of -23 to -2-, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 15, 11, double_exp, p_error_2b * e_exp)

  # Probability of -23 to --3, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 15, 12, triple_exp)

  # Probability of -23 to 1-3, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 15, 14, fc_exp, p_error_1b * e_exp)

  # Probability of -23 to 123, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 15, 16, bb_exp, ibb_exp, hbp_exp, ci_exp)

  # Probability of -23 to -2-, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 15, 19, sf_exp, p_fo_second_stay * f_out_exp)

  # Probability of -23 to --3, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 15, 20, sh_exp, p_fo_second_to_third * f_out_exp)

  # Probability of -23 to 1-3, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 15, 22, fc_o_exp)

  # Probability of -23 to -23, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 15, 23, so_exp)

  # Probability of -23 to end of inning, starting with one out
  
  tmatrix <- elmt_fill(tmatrix, 15, 25, dp_exp, gdp_exp, sf_dp_exp, so_dp_exp)

    # Probability of 123 to ---, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 16, 9, hr_exp)

  # Probability of 123 to -2-, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 16, 11, p_2b_score_from_1b * double_exp)

  # Probability of 123 to --3, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 16, 12, triple_exp)

  # Probability of 123 to 12-, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 16, 13, p_1b_first_to_second * single_exp)

  # Probability of 123 to 1-3, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 16, 14, p_1b_first_to_third * single_exp)

  # Probability of 123 to -23, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 16, 15, p_2b_go_to_3b * double_exp,
                       p_error_2b * e_exp)

  # Probability of 123 to 123, starting with one out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 16, 16, bb_exp, ibb_exp, hbp_exp, ci_exp,
                       fc_exp, p_error_1b * e_exp)

  # Probability of 123 to 12-, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 16, 21, sf_exp, 0.5 * p_fo_second_stay * f_out_exp)

  # Probability of 123 to 1-3, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 16, 22, fc_o_exp, p_fo_second_to_third * f_out_exp)
  
  # Probability of 123 to -23, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 16, 23, sh_exp)

  # Probability of 123 to 123, starting with one out and one-out increase
  
  tmatrix <- elmt_fill(tmatrix, 16, 24, so_exp, fo_exp,
                       0.5 * p_fo_second_stay * f_out_exp)

  # Probability of 123 to end of inning, starting with one out
  
  tmatrix <- elmt_fill(tmatrix, 16, 25, dp_exp, gdp_exp, sf_dp_exp, so_dp_exp)

  # Probability of --- to ---, starting with two outs and no increase
  
  tmatrix <- elmt_fill(tmatrix, 17, 17, hr_exp)

  # Probability of --- to 1--, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 17, 18, single_exp, bb_exp, ibb_exp, hbp_exp,
                       ci_exp, p_error_1b * e_exp)

  # Probability of --- to -2-, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 17, 19, double_exp, p_error_2b * e_exp)

  # Probability of --- to --3, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 17, 20, triple_exp)

  # Probability of --- to end of inning, starting with two out
  
  tmatrix <- elmt_fill(tmatrix, 17, 25, f_out_exp, so_exp)

  # Probability of 1-- to ---, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 18, 17, hr_exp)

  # Probability of 1-- to -2-, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 18, 19, p_2b_score_from_1b * double_exp)

  # Probability of 1-- to --3, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 18, 20, triple_exp)

  # Probability of 1-- to 12-, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 18, 21, bb_exp, ibb_exp, hbp_exp, ci_exp,
                       fc_exp, p_1b_first_to_second * single_exp,
                       p_error_1b * e_exp)

  # Probability of 1-- to 1-3, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 18, 22, p_1b_first_to_third * single_exp)

  # Probability of 1-- to -23, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 18, 23, p_2b_go_to_3b * double_exp,
                       p_error_2b * e_exp)

  # Probability of 1-- to end of inning, starting with two out
  
  tmatrix <- elmt_fill(tmatrix, 18, 25, f_out_exp, fc_o_exp, fo_exp, so_exp)

  # Probability of -2- to ---, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 19, 17, hr_exp)

  # Probability of -2- to 1--, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 19, 18, single_exp)

  # Probability of -2- to -2-, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 19, 19, double_exp, p_error_2b * e_exp)

  # Probability of -2- to --3, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 19, 20, triple_exp)

  # Probability of -2- to 12-, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 19, 21, bb_exp, ibb_exp, hbp_exp, ci_exp)

  # Probability of -2- to 1-3, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 19, 22, fc_exp, p_error_1b * e_exp)

  # Probability of -2- to end of inning, starting with two out
  
  tmatrix <- elmt_fill(tmatrix, 19, 25, fc_o_exp, f_out_exp, so_exp)

  # Probability of --3 to ---, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 20, 17, hr_exp)

  # Probability of --3 to 1--, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 20, 18, single_exp, p_error_1b * e_exp)

  # Probability of --3 to -2-, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 20, 19, double_exp, p_error_2b * e_exp)

  # Probability of --3 to --3, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 20, 20, triple_exp)

  # Probability of --3 to 1-3, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 20, 22, bb_exp, ibb_exp, hbp_exp, ci_exp)

  # Probability of --3 to end of inning, starting with two out
  
  tmatrix <- elmt_fill(tmatrix, 20, 25, f_out_exp, fc_o_exp, so_exp)

  # Probability of 12- to ---, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 21, 17, hr_exp)

  # Probability of 12- to -2-, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 21, 19, p_2b_score_from_1b * double_exp)

  # Probability of 12- to --3, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 21, 20, triple_exp)

  # Probability of 12- to 12-, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 21, 21, p_1b_first_to_second * single_exp)

  # Probability of 12- to 1-3, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 21, 22, p_1b_first_to_third * single_exp)

  # Probability of 12- to -23, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 21, 23, p_2b_go_to_3b * double_exp,
                       p_error_2b * e_exp)

  # Probability of 12- to 123, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 21, 24, bb_exp, ibb_exp, hbp_exp, ci_exp,
                       fc_exp, p_error_1b * e_exp)

  # Probability of 12- to end of inning, starting with two out
  
  tmatrix <- elmt_fill(tmatrix, 21, 25, f_out_exp, fc_o_exp, fo_exp, so_exp)

  # Probability of 1-3 to ---, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 22, 17, hr_exp)

  # Probability of 1-3 to -2-, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 22, 19, p_2b_score_from_1b * double_exp)

  # Probability of 1-3 to --3, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 22, 20, triple_exp)

  # Probability of 1-3 to 12-, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 22, 21, fc_exp, p_error_1b * e_exp,
                       p_1b_first_to_second * single_exp)

  # Probability of 1-3 to 1-3, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 22, 22, p_1b_first_to_third * single_exp)

  # Probability of 1-3 to -23, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 22, 23, p_2b_go_to_3b * double_exp,
                       p_error_2b * e_exp)

  # Probability of 1-3 to 123, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 22, 24, bb_exp, ibb_exp, hbp_exp, ci_exp)

  # Probability of 1-3 to end of inning, starting with two out
  
  tmatrix <- elmt_fill(tmatrix, 22, 25, f_out_exp, fc_o_exp, fo_exp, so_exp)

  # Probability of -23 to ---, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 23, 17, hr_exp)

  # Probability of -23 to 1--, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 23, 18, single_exp)

  # Probability of -23 to -2-, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 23, 19, double_exp, p_error_2b * e_exp)

  # Probability of -23 to --3, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 23, 20, triple_exp)

  # Probability of -23 to 1-3, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 23, 22, fc_exp, p_error_1b * e_exp)

  # Probability of -23 to 123, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 23, 24, bb_exp, ibb_exp, hbp_exp, ci_exp)

  # Probability of -23 to end of inning, starting with two out
  
  tmatrix <- elmt_fill(tmatrix, 23, 25, f_out_exp, fc_o_exp, so_exp)

  # Probability of 123 to ---, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 24, 17, hr_exp)

  # Probability of 123 to -2-, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 24, 19, p_2b_score_from_1b * double_exp)

  # Probability of 123 to --3, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 24, 20, triple_exp)

  # Probability of 123 to 12-, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 24, 21, p_1b_first_to_second * single_exp)

  # Probability of 123 to 1-3, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 24, 22, p_1b_first_to_third * single_exp)

  # Probability of 123 to -23, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 24, 23, p_2b_go_to_3b * double_exp,
                       p_error_2b * e_exp)

  # Probability of 123 to 123, starting with two out and no increase
  
  tmatrix <- elmt_fill(tmatrix, 24, 24, bb_exp, ibb_exp, hbp_exp, ci_exp,
                       fc_exp, p_error_1b * e_exp)

  # Probability of 123 to end of inning, starting with two out
  
  tmatrix <- elmt_fill(tmatrix, 24, 25, f_out_exp, fc_o_exp, fo_exp, so_exp)

  # Filling in the last element of the matrix
  
  tmatrix[25,25] <- 1
  
  # Normalizes the rows of the matrix
  # This may sound counter-intuitive: shouldn't the probabilities sum to 1?
  # This is because multiple events can occur on the same play
  # For example, there can be a single and an error on the same batted ball
  # So the league-wide averages have a bit of overcounting, thus we normalize
  # The actual effect is minimal - the row sums are within .05 of 1
  
  for (i in 1:25) {
    rowsum <- sum(tmatrices[i,])
    tmatrices[i,] <- tmatrices[i,] / rowsum
  }
  
  return(tmatrix)
  
}
