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

# League-wide default event probabilities, as of 9/24/20

bb_exp_lg <- 5697 / 62087
ci_exp_lg <- 33 / 62087
double_exp_lg <- 2635 / 62087
dp_exp_lg <- 137 / 62087
e_exp_lg <- 955 / 62087
f_out_exp_lg <- 23875 / 62087
fc_exp_lg <- 135 / 62087
fc_o_exp_lg <- 110 / 62087
fo_exp_lg <- 1195 / 62087
gdp_exp_lg <- 1146 / 62087
hbp_exp_lg <- 772 / 62087
hr_exp_lg <- 2156 / 62087
ibb_exp_lg <- 187 / 62087
sf_exp_lg <- 370 / 62087
sf_dp_exp_lg <- 2 / 62087
sh_exp_lg <- 112 / 62087
single_exp_lg <- 8439 / 62087
so_exp_lg <- 14540 / 62087
so_dp_exp_lg <- 42 / 62087
tp_exp_lg <- 1 / 62087
triple_exp_lg <- 225 / 62087

# Function to fill in specific element of the transition matrix
# The ellipses are whatever values are entered after the matrix/row/col info

elmt_fill <- function(matrix, row, col, ...) {
  
  matrix[row, col] <- sum(...)
  return(matrix)
  
}

# Standard function (passing in values explicitly)
# Inputs:
# 21 different event probabilities

tmatrix_std <- function(bb_exp = bb_exp_lg,
                        ci_exp = ci_exp_lg,
                        double_exp = double_exp_lg,
                        dp_exp = dp_exp_lg,
                        e_exp = e_exp_lg,
                        f_out_exp = f_out_exp_lg,
                        fc_exp = fc_exp_lg,
                        fc_o_exp = fc_o_exp_lg,
                        fo_exp = fo_exp_lg,
                        gdp_exp = gdp_exp_lg,
                        hbp_exp = hbp_exp_lg,
                        hr_exp = hr_exp_lg,
                        ibb_exp = ibb_exp_lg,
                        sf_exp = sf_exp_lg,
                        sf_dp_exp = sf_dp_exp_lg,
                        sh_exp = sh_exp_lg,
                        single_exp = single_exp_lg,
                        so_exp = so_exp_lg,
                        so_dp_exp = so_dp_exp_lg,
                        tp_exp = tp_exp_lg,
                        triple_exp = triple_exp_lg) {
  
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


# Function to get results of a plate appearance
# This is essentially copied/pasted from pa_pred_simple.R

get_results <- function(bat, pitch, stad, home, temp,
                        away, inning = 1, pitch_ct = 0) {
  
  # Create list of states to iterate over
  states <- list(State$new(top = away, inning = inning, pitch_number = pitch_ct), 
                 State$new(on_1b = T, top = away, inning = inning, 
                           pitch_number = pitch_ct), 
                 State$new(on_2b = T, top = away, inning = inning, 
                           pitch_number = pitch_ct), 
                 State$new(on_3b = T, top = away, inning = inning, 
                           pitch_number = pitch_ct),
                 State$new(on_1b = T, on_2b = T, top = away, 
                           inning = inning, pitch_number = pitch_ct), 
                 State$new(on_1b = T, on_3b = T, top = away,
                           inning = inning, pitch_number = pitch_ct),
                 State$new(on_2b = T, on_3b = T, top = away,
                           inning = inning, pitch_number = pitch_ct), 
                 State$new(on_1b = T, on_2b = T, on_3b = T, top = away,
                           inning = inning, pitch_number = pitch_ct),
                 State$new(outs = 1, top = away, inning = inning, 
                           pitch_number = pitch_ct), 
                 State$new(on_1b = T, outs = 1, top = away,
                           inning = inning, pitch_number = pitch_ct), 
                 State$new(on_2b = T, outs = 1, top = away,
                           inning = inning, pitch_number = pitch_ct), 
                 State$new(on_3b = T, outs = 1, top = away,
                           inning = inning, pitch_number = pitch_ct),
                 State$new(on_1b = T, on_2b = T, outs = 1, top = away,
                           inning = inning, pitch_number = pitch_ct), 
                 State$new(on_1b = T, on_3b = T, outs = 1, top = away,
                           inning = inning, pitch_number = pitch_ct),
                 State$new(on_2b = T, on_3b = T, outs = 1, top = away,
                           inning = inning, pitch_number = pitch_ct), 
                 State$new(on_1b = T, on_2b = T, on_3b = T, outs = 1, top = away,
                           inning = inning, pitch_number = pitch_ct),
                 State$new(outs = 2, top = away, inning = inning, 
                           pitch_number = pitch_ct), 
                 State$new(on_1b = T, outs = 2, top = away,
                           inning = inning, pitch_number = pitch_ct), 
                 State$new(on_2b = T, outs = 2, top = away,
                           inning = inning, pitch_number = pitch_ct), 
                 State$new(on_3b = T, outs = 2, top = away,
                           inning = inning, pitch_number = pitch_ct),
                 State$new(on_1b = T, on_2b = T, outs = 2, top = away,
                           inning = inning, pitch_number = pitch_ct), 
                 State$new(on_1b = T, on_3b = T, outs = 2, top = away,
                           inning = inning, pitch_number = pitch_ct),
                 State$new(on_2b = T, on_3b = T, outs = 2, top = away,
                           inning = inning, pitch_number = pitch_ct), 
                 State$new(on_1b = T, on_2b = T, on_3b = T, outs = 2, top = away,
                           inning = inning, pitch_number = pitch_ct))
  
  #initialize empty lists/vectors
  candidate_batters <- list()
  candidate_pitchers <- list()
  last_names <- vector()
  
  for (batter in bat)
  {
    candidate_batters <- append(candidate_batters, sing$GetPlayers(name=batter))
    last_names <- append(last_names, word(batter, -1))
  }
  
  for (pitcher in pitch)
  {
    candidate_pitchers <- append(candidate_pitchers, sing$GetPlayers(name=pitcher))
  }
  
  venue <- sing$GetVenues(stadium.name = stad)[[1]]
  atmosphere <- Atmosphere$new(venue = venue, 
                               temperature = temp, 
                               home_team = sing$GetTeams(name = home)[[1]])
  d <- date
  
  matchups <- list()
  
  for (b in candidate_batters) 
  {
    for (p in candidate_pitchers)
    {
      for (s in states) 
      {
        matchups <- append(matchups, Matchup$new(batter = b, pitcher = p,
                                                 atmosphere = atmosphere,
                                                 state = s))
      }
    }
  }
  
  results <- sing$GetPaSim(matchup = matchups) %>% 
    dplyr::mutate(num_on = reduce(dplyr::select(., starts_with("on_")), `+`),
                      lineup_spot = match(batter_name, unique(batter_name))) %>% 
    dplyr::arrange(match(word(batter_name, -1), last_names), outs, num_on,
                       desc(on_1b), desc(on_2b)) %>% 
    dplyr::select(-c(num_on))
  
  return(results)
  
}

# Singlearity-based function

tmatrix_sing <- function(batters, pitcher, stadium, home, temp,
                         away, inning = 1, pitch_ct = 0) {
  
  # Uses predicted results to get the transition matrix
  # Initializes nine matrices
  
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
  
  tmatrices <- list(tmatrix_1, tmatrix_2, tmatrix_3, tmatrix_4, tmatrix_5,
                    tmatrix_6, tmatrix_7, tmatrix_8, tmatrix_9)

  # Gets results of plate appearance simulations
  
  results <- get_results(batters, pitcher, stadium, home, temp,
                         away, inning, pitch_ct)
  
  for (i in 1:9) {
    
    # Probability of --- to ---, starting with no outs and no increase
    
    tmatrices[[i]][1,1] <- results[1 + 24 * (i - 1),]$hr_exp
    
    # Probability of --- to 1--, starting with no outs and no increase
    
    tmatrices[[i]][1,2] <- results[1 + 24 * (i - 1),]$single_exp + 
      results[1 + 24 * (i - 1),]$hbp_exp + results[1 + 24 * (i - 1),]$ci_exp + 
      results[1 + 24 * (i - 1),]$bb_exp + results[1 + 24 * (i - 1),]$ibb_exp +
      p_error_1b * results[1 + 24 * (i - 1),]$e_exp
    
    # Probability of --- to -2-, starting with no outs and no increase

    tmatrices[[i]][1,3] <- results[1 + 24 * (i - 1),]$double_exp + 
      p_error_2b * results[1 + 24 * (i - 1),]$e_exp
    
    # Probability of --- to --3, starting with no outs and no increase

    tmatrices[[i]][1,4] <- results[1 + 24 * (i - 1),]$triple_exp

    # Probability of --- to ---, starting with no outs and one-out increase

    tmatrices[[i]][1,9] <- results[1 + 24 * (i - 1),]$f_out_exp + 
      results[1 + 24 * (i - 1),]$so_exp

    # Probability of 1-- to ---, starting with no outs and no increase
    
    tmatrices[[i]][2,1] <- results[2 + 24 * (i - 1),]$hr_exp

    # Probability of 1-- to -2-, starting with no outs and no increase
    
    tmatrices[[i]][2,3] <- p_2b_score_from_1b * results[2 + 24 * (i - 1),]$double_exp

    # Probability of 1-- to --3, starting with no outs and no increase

    tmatrices[[i]][2,4] <- results[2 + 24 * (i - 1),]$triple_exp
    
    # Probability of 1-- to 12-, starting with no outs and no increase
    
    tmatrices[[i]][2,5] <- p_1b_first_to_second * results[2 + 24 * (i - 1),]$single_exp +
      results[2 + 24 * (i - 1),]$hbp_exp + results[2 + 24 * (i - 1),]$ci_exp + 
      results[2 + 24 * (i - 1),]$bb_exp + results[2 + 24 * (i - 1),]$ibb_exp +
      p_error_1b * results[2 + 24 * (i - 1),]$e_exp + results[2 + 24 * (i - 1),]$fc_exp
    
    # Probability of 1-- to 1-3, starting with no outs and no increase

    tmatrices[[i]][2,6] <- p_1b_first_to_third * results[2 + 24 * (i - 1),]$single_exp
    
    # Probability of 1-- to -23, starting with no outs and no increase

    tmatrices[[i]][2,7] <- p_2b_go_to_3b * results[2 + 24 * (i - 1),]$double_exp +
      p_error_2b * results[2 + 24 * (i - 1),]$e_exp
    
    # Probability of 1-- to 1--, starting with no outs and one-out increase

    tmatrices[[i]][2,10] <- results[2 + 24 * (i - 1),]$fc_o_exp + 
      results[2 + 24 * (i - 1),]$fo_exp + results[2 + 24 * (i - 1),]$so_exp + 
      results[2 + 24 * (i - 1),]$f_out_exp
    
    # Probability of 1-- to -2-, starting with no outs and one-out increase

    tmatrices[[i]][2,11] <- results[2 + 24 * (i - 1),]$sh_exp +
      results[2 + 24 * (i - 1),]$sf_exp
    
    # Probability of 1-- to ---, starting with no outs and two-out increase

    tmatrices[[i]][2,17] <- results[2 + 24 * (i - 1),]$dp_exp + 
      results[2 + 24 * (i - 1),]$gdp_exp + results[2 + 24 * (i - 1),]$sf_dp_exp + 
      results[2 + 24 * (i - 1),]$so_dp_exp

    # Probability of -2- to ---, starting with no outs and no increase

    tmatrices[[i]][3,1] <- results[3 + 24 * (i - 1),]$hr_exp
    
    # Probability of -2- to 1--, starting with no outs and no increase

    tmatrices[[i]][3,2] <- results[3 + 24 * (i - 1),]$single_exp
    
    # Probability of -2- to -2-, starting with no outs and no increase

    tmatrices[[i]][3,3] <- results[3 + 24 * (i - 1),]$double_exp + 
      p_error_2b * results[3 + 24 * (i - 1),]$e_exp
    
    # Probability of -2- to --3, starting with no outs and no increase

    tmatrices[[i]][3,4] <- results[3 + 24 * (i - 1),]$triple_exp
    
    # Probability of -2- to 12-, starting with no outs and no increase

    tmatrices[[i]][3,5] <- results[3 + 24 * (i - 1),]$bb_exp + 
      results[3 + 24 * (i - 1),]$ibb_exp + results[3 + 24 * (i - 1),]$hbp_exp + 
      results[3 + 24 * (i - 1),]$ci_exp

    # Probability of -2- to 1-3, starting with no outs and no increase

    tmatrices[[i]][3,6] <- p_error_1b * results[3 + 24 * (i - 1),]$e_exp + 
      results[3 + 24 * (i - 1),]$fc_exp
    
    # Probability of -2- to 1--, starting with no outs and one-out increase

    tmatrices[[i]][3,10] <- results[3 + 24 * (i - 1),]$fc_o_exp
    
    # Probability of -2- to -2-, starting with no outs and one-out increase

    tmatrices[[i]][3,11] <- results[3 + 24 * (i - 1),]$so_exp + 
      p_fo_second_stay * results[3 + 24 * (i - 1),]$f_out_exp
    
    # Probability of -2- to --3, starting with no outs and one-out increase

    tmatrices[[i]][3,12] <- p_fo_second_to_third * results[3 + 24 * (i - 1),]$f_out_exp + 
      results[3 + 24 * (i - 1),]$sh_exp + results[3 + 24 * (i - 1),]$sf_exp
    
    # Probability of -2- to ---, starting with no outs and two-out increase

    tmatrices[[i]][3,17] <- results[3 + 24 * (i - 1),]$dp_exp + 
      results[3 + 24 * (i - 1),]$gdp_exp + results[3 + 24 * (i - 1),]$sf_dp_exp + 
      results[3 + 24 * (i - 1),]$so_dp_exp

    # Probability of --3 to ---, starting with no outs and no increase

    tmatrices[[i]][4,1] <- results[4 + 24 * (i - 1),]$hr_exp
    
    # Probability of --3 to 1--, starting with no outs and no increase

    tmatrices[[i]][4,2] <- results[4 + 24 * (i - 1),]$single_exp + 
      p_error_1b * results[4 + 24 * (i - 1),]$e_exp + results[4 + 24 * (i - 1),]$fc_exp

    # Probability of --3 to -2-, starting with no outs and no increase

    tmatrices[[i]][4,3] <- results[4 + 24 * (i - 1),]$double_exp + 
      p_error_2b * results[4 + 24 * (i - 1),]$e_exp
    
    # Probability of --3 to --3, starting with no outs and no increase

    tmatrices[[i]][4,4] <- results[4 + 24 * (i - 1),]$triple_exp
    
    # Probability of --3 to 1-3, starting with no outs and no increase
    
    tmatrices[[i]][4,6] <- results[4 + 24 * (i - 1),]$bb_exp + 
      results[4 + 24 * (i - 1),]$ibb_exp + results[4 + 24 * (i - 1),]$hbp_exp + 
      results[4 + 24 * (i - 1),]$ci_exp
    
    # Probability of --3 to ---, starting with no outs and one-out increase

    tmatrices[[i]][4,9] <- results[4 + 24 * (i - 1),]$sf_exp + 
      results[4 + 24 * (i - 1),]$f_out_exp + results[4 + 24 * (i - 1),]$fc_o_exp + 
      results[4 + 24 * (i - 1),]$sh_exp + results[4 + 24 * (i - 1),]$fo_exp
    
    # Probability of --3 to --3, starting with no outs and one-out increase
    
    tmatrices[[i]][4,12] <- results[4 + 24 * (i - 1),]$so_exp

    # Probability of --3 to ---, starting with no outs and two-out increase
    
    tmatrices[[i]][4,17] <- results[4 + 24 * (i - 1),]$dp_exp + 
      results[4 + 24 * (i - 1),]$gdp_exp + results[4 + 24 * (i - 1),]$sf_dp_exp + 
      results[4 + 24 * (i - 1),]$so_dp_exp

    # Probability of 12- to ---, starting with no outs and no increase
    
    tmatrices[[i]][5,1] <- results[5 + 24 * (i - 1),]$hr_exp

    # Probability of 12- to -2-, starting with no outs and no increase

    tmatrices[[i]][5,3] <- p_2b_score_from_1b * results[5 + 24 * (i - 1),]$double_exp
    
    # Probability of 12- to --3, starting with no outs and no increase

    tmatrices[[i]][5,4] <- results[5 + 24 * (i - 1),]$triple_exp

    # Probability of 12- to 12-, starting with no outs and no increase
    
    tmatrices[[i]][5,5] <- p_1b_first_to_second * results[5 + 24 * (i - 1),]$single_exp

    # Probability of 12- to 1-3, starting with no outs and no increase

    tmatrices[[i]][5,6] <- p_1b_first_to_third * results[5 + 24 * (i - 1),]$single_exp

    # Probability of 12- to -23, starting with no outs and no increase

    tmatrices[[i]][5,7] <- p_2b_go_to_3b * results[5 + 24 * (i - 1),]$double_exp +
      p_error_2b * results[5 + 24 * (i - 1),]$e_exp

    # Probability of 12- to 123, starting with no outs and no increase
    
    tmatrices[[i]][5,8] <- results[5 + 24 * (i - 1),]$bb_exp + 
      results[5 + 24 * (i - 1),]$ibb_exp + results[5 + 24 * (i - 1),]$hbp_exp + 
      results[5 + 24 * (i - 1),]$ci_exp + p_error_1b * results[5 + 24 * (i - 1),]$e_exp +
      results[5 + 24 * (i - 1),]$fc_exp
    
    # Probability of 12- to 12-, starting with no outs and one-out increase

    tmatrices[[i]][5,13] <- results[5 + 24 * (i - 1),]$f_out_exp + 
      results[5 + 24 * (i - 1),]$so_exp
    
    # Probability of 12- to 1-3, starting with no outs and one-out increase
    
    tmatrices[[i]][5,14] <- results[5 + 24 * (i - 1),]$fo_exp + 
      results[5 + 24 * (i - 1),]$fc_o_exp + results[5 + 24 * (i - 1),]$sf_exp

    # Probability of 12- to -23, starting with no outs and one-out increase

    tmatrices[[i]][5,15] <- results[5 + 24 * (i - 1),]$sh_exp

    # Probability of 12- to -2-, starting with no outs and two-out increase
    
    tmatrices[[i]][5,19] <- results[5 + 24 * (i - 1),]$so_dp_exp + 
      results[5 + 24 * (i - 1),]$dp_exp

    # Probability of 12- to --3, starting with no outs and two-out increase

    tmatrices[[i]][5,20] <- results[5 + 24 * (i - 1),]$gdp_exp

    # Probability of 12- to end of inning, starting with no outs

    tmatrices[[i]][5,25] <- results[5 + 24 * (i - 1),]$tp_exp

    # Probability of 1-3 to ---, starting with no outs and no increase
    
    tmatrices[[i]][6,1] <- results[6 + 24 * (i - 1),]$hr_exp

    # Probability of 1-3 to -2-, starting with no outs and no increase
    
    tmatrices[[i]][6,3] <- p_2b_score_from_1b * results[6 + 24 * (i - 1),]$double_exp
    
    # Probability of 1-3 to --3, starting with no outs and no increase

    tmatrices[[i]][6,4] <- results[6 + 24 * (i - 1),]$triple_exp

    # Probability of 1-3 to 12-, starting with no outs and no increase

    tmatrices[[i]][6,5] <- p_1b_first_to_second * results[6 + 24 * (i - 1),]$single_exp +
      p_error_1b * results[6 + 24 * (i - 1),]$e_exp + results[6 + 24 * (i - 1),]$fc_exp
    
    # Probability of 1-3 to 1-3, starting with no outs and no increase
    
    tmatrices[[i]][6,6] <- p_1b_first_to_third * results[6 + 24 * (i - 1),]$single_exp
    
    # Probability of 1-3 to -23, starting with no outs and no increase

    tmatrices[[i]][6,7] <- p_2b_go_to_3b * results[6 + 24 * (i - 1),]$double_exp +
      p_error_2b * results[6 + 24 * (i - 1),]$e_exp
    
    # Probability of 1-3 to 123, starting with no outs and no increase
    
    tmatrices[[i]][6,8] <- results[6 + 24 * (i - 1),]$bb_exp + 
      results[6 + 24 * (i - 1),]$ibb_exp + results[6 + 24 * (i - 1),]$hbp_exp + 
      results[6 + 24 * (i - 1),]$ci_exp

    # Probability to 1-3 to 1--, starting with no outs and one-out increase
    
    tmatrices[[i]][6,10] <- results[6 + 24 * (i - 1),]$sf_exp + 
      results[6 + 24 * (i - 1),]$fc_o_exp + results[6 + 24 * (i - 1),]$fo_exp

    # Probability of 1-3 to -2-, starting with no outs and one-out increase
    
    tmatrices[[i]][6,11] <- results[6 + 24 * (i - 1),]$sh_exp
    
    # Probability of 1-3 to 1-3, starting with no outs and one-out increase

    tmatrices[[i]][6,14] <- results[6 + 24 * (i - 1),]$so_exp + 
      results[6 + 24 * (i - 1),]$f_out_exp
    
    # Probability of 1-3 to ---, starting with no outs and two-out increase
    
    tmatrices[[i]][6,17] <- results[6 + 24 * (i - 1),]$dp_exp + 
      results[6 + 24 * (i - 1),]$gdp_exp

    # Probability of 1-3 to -2-, starting with no outs and two-out increase

    tmatrices[[i]][6,19] <- results[6 + 24 * (i - 1),]$sf_dp_exp

    # Probability of 1-3 to --3, starting with no outs and two-out increase
    
    tmatrices[[i]][6,20] <- results[6 + 24 * (i - 1),]$so_dp_exp

    # Probability of 1-3 to end of inning, starting with no outs

    tmatrices[[i]][6,25] <- results[6 + 24 * (i - 1),]$tp_exp
    
    # Probability of -23 to ---, starting with no outs and no increase

    tmatrices[[i]][7,1] <- results[7 + 24 * (i - 1),]$hr_exp
    
    # Probability of -23 to 1--, starting with no outs and no increase
    
    tmatrices[[i]][7,2] <- results[7 + 24 * (i - 1),]$single_exp
    
    # Probability of -23 to -2-, starting with no outs and no increase
    
    tmatrices[[i]][7,3] <- results[7 + 24 * (i - 1),]$double_exp + 
      p_error_2b * results[7 + 24 * (i - 1),]$e_exp

    # Probability of -23 to --3, starting with no outs and no increase

    tmatrices[[i]][7,4] <- results[7 + 24 * (i - 1),]$triple_exp
    
    # Probability of -23 to 1-3, starting with no outs and no increase
    
    tmatrices[[i]][7,6] <- p_error_1b * results[7 + 24 * (i - 1),]$e_exp + 
      results[7 + 24 * (i - 1),]$fc_exp
    
    # Probability of -23 to 123, starting with no outs and no increase

    tmatrices[[i]][7,8] <- results[7 + 24 * (i - 1),]$bb_exp + 
      results[7 + 24 * (i - 1),]$ibb_exp + results[7 + 24 * (i - 1),]$hbp_exp + 
      results[7 + 24 * (i - 1),]$ci_exp
    
    # Probability of -23 to -2-, starting with no outs and one-out increase
    
    tmatrices[[i]][7,11] <- results[7 + 24 * (i - 1),]$sf_exp + 
      p_fo_second_stay * results[7 + 24 * (i - 1),]$f_out_exp
    
    # Probability of -23 to --3, starting with no outs and one-out increase
    
    tmatrices[[i]][7,12] <- results[7 + 24 * (i - 1),]$sh_exp + 
      p_fo_second_to_third * results[7 + 24 * (i - 1),]$f_out_exp
    
    # Probability of -23 to 1-3, starting with no outs and one-out increase
    
    tmatrices[[i]][7,14] <- results[7 + 24 * (i - 1),]$fc_o_exp

    # Probability of -23 to -23, starting with no outs and one-out increase
    
    tmatrices[[i]][7,15] <- results[7 + 24 * (i - 1),]$so_exp
    
    # Probability of -23 to --3, starting with no outs and two-out increase

    tmatrices[[i]][7,20] <- results[7 + 24 * (i - 1),]$sf_dp_exp + 
      results[7 + 24 * (i - 1),]$so_dp_exp + results[7 + 24 * (i - 1),]$dp_exp + 
      results[7 + 24 * (i - 1),]$gdp_exp

    # Probability of -23 to end of inning, starting with no outs

    tmatrices[[i]][7,25] <- results[7 + 24 * (i - 1),]$tp_exp
    
    # Probability of 123 to ---, starting with no outs and no increase
    
    tmatrices[[i]][8,1] <- results[8 + 24 * (i - 1),]$hr_exp

    # Probability of 123 to -2-, starting with no outs and no increase

    tmatrices[[i]][8,3] <- p_2b_score_from_1b * results[8 + 24 * (i - 1),]$double_exp
    
    # Probability of 123 to --3, starting with no outs and no increase

    tmatrices[[i]][8,4] <- results[8 + 24 * (i - 1),]$triple_exp

    # Probability of 123 to 12-, starting with no outs and no increase
    
    tmatrices[[i]][8,5] <- p_1b_first_to_second * results[8 + 24 * (i - 1),]$single_exp
    
    # Probability of 123 to 1-3, starting with no outs and no increase
    
    tmatrices[[i]][8,6] <- p_1b_first_to_third * results[8 + 24 * (i - 1),]$single_exp
    
    # Probability of 123 to -23, starting with no outs and no increase
    
    tmatrices[[i]][8,7] <- p_2b_go_to_3b * results[8 + 24 * (i - 1),]$double_exp +
      p_error_2b * results[8 + 24 * (i - 1),]$e_exp
    
    # Probability of 123 to 123, starting with no outs and no increase
    
    tmatrices[[i]][8,8] <- results[8 + 24 * (i - 1),]$bb_exp + 
      results[8 + 24 * (i - 1),]$ibb_exp + results[8 + 24 * (i - 1),]$hbp_exp + 
      results[8 + 24 * (i - 1),]$ci_exp + p_error_1b * results[8 + 24 * (i - 1),]$e_exp +
      results[8 + 24 * (i - 1),]$fc_exp
    
    # Probability of 123 to 12-, starting with no outs and one-out increase

    tmatrices[[i]][8,13] <- results[8 + 24 * (i - 1),]$sf_exp +
      0.5 * p_fo_second_stay * results[8 + 24 * (i - 1),]$f_out_exp
    
    # Probability of 123 to 1-3, starting with no outs and one-out increase

    tmatrices[[i]][8,14] <- p_fo_second_to_third * results[8 + 24 * (i - 1),]$f_out_exp +
      results[8 + 24 * (i - 1),]$fc_o_exp

    # Probability of 123 to -23, starting with no outs and one-out increase

    tmatrices[[i]][8,15] <- results[8 + 24 * (i - 1),]$sh_exp
    
    # Probability of 123 to 123, starting with no outs and one-out increase

    tmatrices[[i]][8,16] <- 0.5 * p_fo_second_stay * results[8 + 24 * (i - 1),]$f_out_exp +
      results[8 + 24 * (i - 1),]$so_exp + results[8 + 24 * (i - 1),]$fo_exp

    # Probability of 123 to --3, starting with no outs and two-out increase
    
    tmatrices[[i]][8,20] <- results[8 + 24 * (i - 1),]$gdp_exp

    # Probability of 123 to 12-, starting with no outs and two-out increase
    
    tmatrices[[i]][8,21] <- results[8 + 24 * (i - 1),]$so_dp_exp + 
      results[8 + 24 * (i - 1),]$dp_exp

    # Probability of 123 to 1-3, starting with no outs and two-out increase
    
    tmatrices[[i]][8,22] <- results[8 + 24 * (i - 1),]$sf_dp_exp

    # Probability of 123 to end of inning, starting with no outs
    
    tmatrices[[i]][8,25] <- results[8 + 24 * (i - 1),]$tp_exp

    # Probability of --- to ---, starting with one out and no increase
    
    tmatrices[[i]][9,9] <- results[9 + 24 * (i - 1),]$hr_exp

    # Probability of --- to 1--, starting with one out and no increase

    tmatrices[[i]][9,10] <- results[9 + 24 * (i - 1),]$single_exp + 
      results[9 + 24 * (i - 1),]$hbp_exp + results[9 + 24 * (i - 1),]$ci_exp + 
      results[9 + 24 * (i - 1),]$bb_exp + results[9 + 24 * (i - 1),]$ibb_exp +
      p_error_1b * results[9 + 24 * (i - 1),]$e_exp

    # Probability of --- to -2-, starting with one out and no increase

    tmatrices[[i]][9,11] <- results[9 + 24 * (i - 1),]$double_exp + 
      p_error_2b * results[9 + 24 * (i - 1),]$e_exp

    # Probability of --- to --3, starting with one out and no increase

    tmatrices[[i]][9,12] <- results[9 + 24 * (i - 1),]$triple_exp

    # Probability of --- to ---, starting with one out and one-out increase

    tmatrices[[i]][9,17] <- results[9 + 24 * (i - 1),]$f_out_exp + 
      results[9 + 24 * (i - 1),]$so_exp

    # Probability of 1-- to ---, starting with one out and no increase
    
    tmatrices[[i]][10,9] <- results[10 + 24 * (i - 1),]$hr_exp

    # Probability of 1-- to -2-, starting with one out and no increase

    tmatrices[[i]][10,11] <- p_2b_score_from_1b * results[10 + 24 * (i - 1),]$double_exp
    
    # Probability of 1-- to --3, starting with one out and no increase

    tmatrices[[i]][10,12] <- results[10 + 24 * (i - 1),]$triple_exp

    # Probability of 1-- to 12-, starting with one out and no increase

    tmatrices[[i]][10,13] <- p_1b_first_to_second * results[10 + 24 * (i - 1),]$single_exp +
      results[10 + 24 * (i - 1),]$bb_exp + results[10 + 24 * (i - 1),]$ibb_exp + 
      results[10 + 24 * (i - 1),]$hbp_exp + results[10 + 24 * (i - 1),]$ci_exp + 
      p_error_1b * results[10 + 24 * (i - 1),]$e_exp + results[10 + 24 * (i - 1),]$fc_exp
    
    # Probability of 1-- to 1-3, starting with one out and no increase

    tmatrices[[i]][10,14] <- p_1b_first_to_third * results[10 + 24 * (i - 1),]$single_exp

    # Probability of 1-- to -23, starting with one out and no increase

    tmatrices[[i]][10,15] <- p_2b_go_to_3b * results[10 + 24 * (i - 1),]$double_exp +
      p_error_2b * results[10 + 24 * (i - 1),]$e_exp
    
    # Probability of 1-- to 1--, starting with one out and one-out increase
    
    tmatrices[[i]][10,18] <- results[10 + 24 * (i - 1),]$fc_o_exp + 
      results[10 + 24 * (i - 1),]$fo_exp + results[10 + 24 * (i - 1),]$so_exp + 
      results[10 + 24 * (i - 1),]$f_out_exp
    
    # Probability of 1-- to -2-, starting with one out and one-out increase

    tmatrices[[i]][10,19] <- results[10 + 24 * (i - 1),]$sh_exp

    # Probability of 1-- to end of inning, starting with one out

    tmatrices[[i]][10,25] <- results[10 + 24 * (i - 1),]$dp_exp + 
      results[10 + 24 * (i - 1),]$gdp_exp + results[10 + 24 * (i - 1),]$sf_dp_exp + 
      results[10 + 24 * (i - 1),]$so_dp_exp
    
    # Probability of -2- to ---, starting with one out and no increase
    
    tmatrices[[i]][11,9] <- results[11 + 24 * (i - 1),]$hr_exp
    
    # Probability of -2- to 1--, starting with one out and no increase

    tmatrices[[i]][11,10] <- results[11 + 24 * (i - 1),]$single_exp

    # Probability of -2- to -2-, starting with one out and no increase

    tmatrices[[i]][11,11] <- results[11 + 24 * (i - 1),]$double_exp + 
      p_error_2b * results[11 + 24 * (i - 1),]$e_exp

    # Probability of -2- to --3, starting with one out and no increase

    tmatrices[[i]][11,12] <- results[11 + 24 * (i - 1),]$triple_exp

    # Probability of -2- to 12-, starting with one out and no increase

    tmatrices[[i]][11,13] <- results[11 + 24 * (i - 1),]$bb_exp + 
      results[11 + 24 * (i - 1),]$ibb_exp + results[11 + 24 * (i - 1),]$hbp_exp + 
      results[11 + 24 * (i - 1),]$ci_exp
    
    # Probability of -2- to 1-3, starting with one out and no increase

    tmatrices[[i]][11,14] <- p_error_1b * results[11 + 24 * (i - 1),]$e_exp + 
      results[11 + 24 * (i - 1),]$fc_exp

    # Probability of -2- to 1--, starting with one out and one-out increase

    tmatrices[[i]][11,18] <- results[11 + 24 * (i - 1),]$fc_o_exp

    # Probability of -2- to -2-, starting with one out and one-out increase

    tmatrices[[i]][11,19] <- results[11 + 24 * (i - 1),]$so_exp + 
      p_fo_second_stay * results[11 + 24 * (i - 1),]$f_out_exp
    
    # Probability of -2- to --3, starting with one out and one-out increase
    
    tmatrices[[i]][11,20] <- p_fo_second_to_third * results[11 + 24 * (i - 1),]$f_out_exp +
      results[11 + 24 * (i - 1),]$sh_exp + results[11 + 24 * (i - 1),]$sf_exp

    # Probability of -2- to end of inning, starting with one out

    tmatrices[[i]][11,25] <- results[11 + 24 * (i - 1),]$dp_exp + 
      results[11 + 24 * (i - 1),]$gdp_exp + results[11 + 24 * (i - 1),]$sf_dp_exp + 
      results[11 + 24 * (i - 1),]$so_dp_exp

    # Probability of --3 to ---, starting with one out and no increase

    tmatrices[[i]][12,9] <- results[12 + 24 * (i - 1),]$hr_exp

    # Probability of --3 to 1--, starting with one out and no increase

    tmatrices[[i]][12,10] <- results[12 + 24 * (i - 1),]$single_exp + 
      p_error_1b * results[12 + 24 * (i - 1),]$e_exp + results[12 + 24 * (i - 1),]$fc_exp
    
    # Probability of --3 to -2-, starting with one out and no increase

    tmatrices[[i]][12,11] <- results[12 + 24 * (i - 1),]$double_exp + 
      p_error_2b * results[12 + 24 * (i - 1),]$e_exp
    
    # Probability of --3 to --3, starting with one out and no increase
    
    tmatrices[[i]][12,12] <- results[12 + 24 * (i - 1),]$triple_exp

    # Probability of --3 to 1-3, starting with one out and no increase
    
    tmatrices[[i]][12,14] <- results[12 + 24 * (i - 1),]$bb_exp + 
      results[12 + 24 * (i - 1),]$ibb_exp + results[12 + 24 * (i - 1),]$hbp_exp + 
      results[12 + 24 * (i - 1),]$ci_exp

    # Probability of --3 to ---, starting with one out and one-out increase
    
    tmatrices[[i]][12,17] <- results[12 + 24 * (i - 1),]$sf_exp + 
      results[12 + 24 * (i - 1),]$f_out_exp + results[12 + 24 * (i - 1),]$sh_exp
    
    # Probability of --3 to 1--, starting with one out and one-out increase

    tmatrices[[i]][12,18] <- results[12 + 24 * (i - 1),]$fc_o_exp +
      results[12 + 24 * (i - 1),]$fo_exp

    # Probability of --3 to --3, starting with one out and one-out increase

    tmatrices[[i]][12,20] <- results[12 + 24 * (i - 1),]$so_exp
    
    # Probability of --3 to end of inning, starting with one out

    tmatrices[[i]][12,25] <- results[12 + 24 * (i - 1),]$dp_exp + 
      results[12 + 24 * (i - 1),]$gdp_exp + results[12 + 24 * (i - 1),]$sf_dp_exp + 
      results[12 + 24 * (i - 1),]$so_dp_exp

    # Probability of 12- to ---, starting with one out and no increase
    
    tmatrices[[i]][13,9] <- results[13 + 24 * (i - 1),]$hr_exp
    
    # Probability of 12- to -2-, starting with one out and no increase
    
    tmatrices[[i]][13,11] <- p_2b_score_from_1b * results[13 + 24 * (i - 1),]$double_exp

    # Probability of 12- to --3, starting with one out and no increase

    tmatrices[[i]][13,12] <- results[13 + 24 * (i - 1),]$triple_exp

    # Probability of 12- to 12-, starting with one out and no increase
    
    tmatrices[[i]][13,13] <- p_1b_first_to_second * results[13 + 24 * (i - 1),]$single_exp

    # Probability of 12- to 1-3, starting with one out and no increase
    
    tmatrices[[i]][13,14] <- p_1b_first_to_third * results[13 + 24 * (i - 1),]$single_exp

    # Probability of 12- to -23, starting with one out and no increase

    tmatrices[[i]][13,15] <- p_2b_go_to_3b * results[13 + 24 * (i - 1),]$double_exp +
      p_error_2b * results[13 + 24 * (i - 1),]$e_exp

    # Probability of 12- to 123, starting with one out and no increase
    
    tmatrices[[i]][13,16] <- results[13 + 24 * (i - 1),]$bb_exp + 
      results[13 + 24 * (i - 1),]$ibb_exp + results[13 + 24 * (i - 1),]$hbp_exp + 
      results[13 + 24 * (i - 1),]$ci_exp + p_error_1b * results[13 + 24 * (i - 1),]$e_exp +
      results[13 + 24 * (i - 1),]$fc_exp
    
    # Probability of 12- to 12-, starting with one out and one-out increase

    tmatrices[[i]][13,21] <- results[13 + 24 * (i - 1),]$f_out_exp + 
      results[13 + 24 * (i - 1),]$so_exp

    # Probability of 12- to 1-3, starting with one out and one-out increase
    
    tmatrices[[i]][13,22] <- results[13 + 24 * (i - 1),]$fo_exp + 
      results[13 + 24 * (i - 1),]$fc_o_exp + results[13 + 24 * (i - 1),]$sf_exp

    # Probability of 12- to -23, starting with one out and one-out increase

    tmatrices[[i]][13,23] <- results[13 + 24 * (i - 1),]$sh_exp

    # Probability of 12- to end of inning, starting with one out
    
    tmatrices[[i]][13,25] <- results[13 + 24 * (i - 1),]$dp_exp + 
      results[13 + 24 * (i - 1),]$gdp_exp + results[13 + 24 * (i - 1),]$sf_dp_exp + 
      results[13 + 24 * (i - 1),]$so_dp_exp

    # Probability of 1-3 to ---, starting with one out and no increase
    
    tmatrices[[i]][14,9] <- results[14 + 24 * (i - 1),]$hr_exp

    # Probability of 1-3 to -2-, starting with one out and no increase

    tmatrices[[i]][14,11] <- p_2b_score_from_1b * results[14 + 24 * (i - 1),]$double_exp

    # Probability of 1-3 to --3, starting with one out and no increase

    tmatrices[[i]][14,12] <- results[14 + 24 * (i - 1),]$triple_exp
    
    # Probability of 1-3 to 12-, starting with one out and no increase
    
    tmatrices[[i]][14,13] <- p_1b_first_to_second * results[14 + 24 * (i - 1),]$single_exp +
      p_error_1b * results[14 + 24 * (i - 1),]$e_exp + results[14 + 24 * (i - 1),]$fc_exp
    
    # Probability of 1-3 to 1-3, starting with one out and no increase
    
    tmatrices[[i]][14,14] <- p_1b_first_to_third * results[14 + 24 * (i - 1),]$single_exp

    # Probability of 1-3 to -23, starting with one out and no increase

    tmatrices[[i]][14,15] <- p_2b_go_to_3b * results[14 + 24 * (i - 1),]$double_exp +
      p_error_2b * results[14 + 24 * (i - 1),]$e_exp

    # Probability of 1-3 to 123, starting with one out and no increase
    
    tmatrices[[i]][14,16] <- results[14 + 24 * (i - 1),]$bb_exp + 
      results[14 + 24 * (i - 1),]$ibb_exp + results[14 + 24 * (i - 1),]$hbp_exp + 
      results[14 + 24 * (i - 1),]$ci_exp
    
    # Probability to 1-3 to 1--, starting with one out and one-out increase

    tmatrices[[i]][14,18] <- results[14 + 24 * (i - 1),]$sf_exp + 
      results[14 + 24 * (i - 1),]$fc_o_exp + results[14 + 24 * (i - 1),]$fo_exp

    # Probability of 1-3 to -2-, starting with one out and one-out increase

    tmatrices[[i]][14,19] <- results[14 + 24 * (i - 1),]$sh_exp

    # Probability of 1-3 to 1-3, starting with one out and one-out increase
    
    tmatrices[[i]][14,22] <- results[14 + 24 * (i - 1),]$so_exp + 
      results[14 + 24 * (i - 1),]$f_out_exp
    
    # Probability of 1-3 to end of inning, starting with one out

    tmatrices[[i]][14,25] <- results[14 + 24 * (i - 1),]$dp_exp + 
      results[14 + 24 * (i - 1),]$gdp_exp + results[14 + 24 * (i - 1),]$sf_dp_exp + 
      results[14 + 24 * (i - 1),]$so_dp_exp

    # Probability of -23 to ---, starting with one out and no increase
    
    tmatrices[[i]][15,9] <- results[15 + 24 * (i - 1),]$hr_exp
    
    # Probability of -23 to 1--, starting with one out and no increase
    
    tmatrices[[i]][15,10] <- results[15 + 24 * (i - 1),]$single_exp
    
    # Probability of -23 to -2-, starting with one out and no increase
    
    tmatrices[[i]][15,11] <- results[15 + 24 * (i - 1),]$double_exp + 
      p_error_2b * results[15 + 24 * (i - 1),]$e_exp

    # Probability of -23 to --3, starting with one out and no increase

    tmatrices[[i]][15,12] <- results[15 + 24 * (i - 1),]$triple_exp

    # Probability of -23 to 1-3, starting with one out and no increase

    tmatrices[[i]][15,14] <- p_error_1b * results[15 + 24 * (i - 1),]$e_exp + 
      results[15 + 24 * (i - 1),]$fc_exp
    
    # Probability of -23 to 123, starting with one out and no increase

    tmatrices[[i]][15,16] <- results[15 + 24 * (i - 1),]$bb_exp + 
      results[15 + 24 * (i - 1),]$ibb_exp + results[15 + 24 * (i - 1),]$hbp_exp + 
      results[15 + 24 * (i - 1),]$ci_exp

    # Probability of -23 to -2-, starting with one out and one-out increase

    tmatrices[[i]][15,19] <- results[15 + 24 * (i - 1),]$sf_exp +
      p_fo_second_stay * results[15 + 24 * (i - 1),]$f_out_exp
    
    # Probability of -23 to --3, starting with one out and one-out increase

    tmatrices[[i]][15,20] <- results[15 + 24 * (i - 1),]$sh_exp +
      p_fo_second_to_third * results[15 + 24 * (i - 1),]$f_out_exp
    
    # Probability of -23 to 1-3, starting with one out and one-out increase

    tmatrices[[i]][15,22] <- results[15 + 24 * (i - 1),]$fc_o_exp +
      results[15 + 24 * (i - 1),]$fo_exp
    
    # Probability of -23 to -23, starting with one out and one-out increase

    tmatrices[[i]][15,23] <- results[15 + 24 * (i - 1),]$so_exp

    # Probability of -23 to end of inning, starting with one out

    tmatrices[[i]][15,25] <- results[15 + 24 * (i - 1),]$dp_exp + 
      results[15 + 24 * (i - 1),]$gdp_exp + results[15 + 24 * (i - 1),]$sf_dp_exp + 
      results[15 + 24 * (i - 1),]$so_dp_exp

    # Probability of 123 to ---, starting with one out and no increase

    tmatrices[[i]][16,9] <- results[16 + 24 * (i - 1),]$hr_exp

    # Probability of 123 to -2-, starting with one out and no increase

    tmatrices[[i]][16,11] <- p_2b_score_from_1b * results[16 + 24 * (i - 1),]$double_exp

    # Probability of 123 to --3, starting with one out and no increase

    tmatrices[[i]][16,12] <- results[16 + 24 * (i - 1),]$triple_exp

    # Probability of 123 to 12-, starting with one out and no increase
    
    tmatrices[[i]][16,13] <- p_1b_first_to_second * results[16 + 24 * (i - 1),]$single_exp

    # Probability of 123 to 1-3, starting with one out and no increase

    tmatrices[[i]][16,14] <- p_1b_first_to_third * results[16 + 24 * (i - 1),]$single_exp
    
    # Probability of 123 to -23, starting with one out and no increase
    
    tmatrices[[i]][16,15] <- p_2b_go_to_3b * results[16 + 24 * (i - 1),]$double_exp +
      p_error_2b * results[16 + 24 * (i - 1),]$e_exp

    # Probability of 123 to 123, starting with one out and no increase

    tmatrices[[i]][16,16] <- results[16 + 24 * (i - 1),]$bb_exp + 
      results[16 + 24 * (i - 1),]$ibb_exp + results[16 + 24 * (i - 1),]$hbp_exp + 
      results[16 + 24 * (i - 1),]$ci_exp + p_error_1b * results[16 + 24 * (i - 1),]$e_exp +
      results[16 + 24 * (i - 1),]$fc_exp

    # Probability of 123 to 12-, starting with one out and one-out increase

    tmatrices[[i]][16,21] <- results[16 + 24 * (i - 1),]$sf_exp +
      0.5 * p_fo_second_stay * results[16 + 24 * (i - 1),]$f_out_exp

    # Probability of 123 to 1-3, starting with one out and one-out increase

    tmatrices[[i]][16,22] <- p_fo_second_to_third * results[16 + 24 * (i - 1),]$f_out_exp +
      results[16 + 24 * (i - 1),]$fc_o_exp

    # Probability of 123 to -23, starting with one out and one-out increase
    
    tmatrices[[i]][16,23] <- results[16 + 24 * (i - 1),]$sh_exp
    
    # Probability of 123 to 123, starting with one out and one-out increase
    
    tmatrices[[i]][16,24] <- 0.5 * p_fo_second_stay * results[16 + 24 * (i - 1),]$f_out_exp +
      results[16 + 24 * (i - 1),]$so_exp + results[16 + 24 * (i - 1),]$fo_exp
    
    # Probability of 123 to end of inning, starting with one out
    
    tmatrices[[i]][16,25] <- results[16 + 24 * (i - 1),]$dp_exp + 
      results[16 + 24 * (i - 1),]$gdp_exp + results[16 + 24 * (i - 1),]$sf_dp_exp + 
      results[16 + 24 * (i - 1),]$so_dp_exp
    
    # Probability of --- to ---, starting with two outs and no increase
    
    tmatrices[[i]][17,17] <- results[17 + 24 * (i - 1),]$hr_exp

    # Probability of --- to 1--, starting with two out and no increase

    tmatrices[[i]][17,18] <- results[17 + 24 * (i - 1),]$single_exp + 
      results[17 + 24 * (i - 1),]$bb_exp + results[17 + 24 * (i - 1),]$ibb_exp + 
      results[17 + 24 * (i - 1),]$hbp_exp + results[17 + 24 * (i - 1),]$ci_exp + 
      p_error_1b * results[17 + 24 * (i - 1),]$e_exp

    # Probability of --- to -2-, starting with two out and no increase

    tmatrices[[i]][17,19] <- results[17 + 24 * (i - 1),]$double_exp + 
      p_error_2b * results[17 + 24 * (i - 1),]$e_exp

    # Probability of --- to --3, starting with two out and no increase

    tmatrices[[i]][17,20] <- results[17 + 24 * (i - 1),]$triple_exp

    # Probability of --- to end of inning, starting with two out

    tmatrices[[i]][17,25] <- results[17 + 24 * (i - 1),]$f_out_exp + 
      results[17 + 24 * (i - 1),]$so_exp

    # Probability of 1-- to ---, starting with two out and no increase
    
    tmatrices[[i]][18,17] <- results[18 + 24 * (i - 1),]$hr_exp

    # Probability of 1-- to -2-, starting with two out and no increase

    tmatrices[[i]][18,19] <- p_2b_score_from_1b * results[18 + 24 * (i - 1),]$double_exp

    # Probability of 1-- to --3, starting with two out and no increase

    tmatrices[[i]][18,20] <- results[18 + 24 * (i - 1),]$triple_exp

    # Probability of 1-- to 12-, starting with two out and no increase

    tmatrices[[i]][18,21] <- p_1b_first_to_second * results[18 + 24 * (i - 1),]$single_exp +
      results[18 + 24 * (i - 1),]$bb_exp + results[18 + 24 * (i - 1),]$ibb_exp + 
      results[18 + 24 * (i - 1),]$hbp_exp + results[18 + 24 * (i - 1),]$ci_exp +
      p_error_1b * results[18 + 24 * (i - 1),]$e_exp + results[18 + 24 * (i - 1),]$fc_exp
    
    # Probability of 1-- to 1-3, starting with two out and no increase

    tmatrices[[i]][18,22] <- p_1b_first_to_third * results[18 + 24 * (i - 1),]$single_exp

    # Probability of 1-- to -23, starting with two out and no increase

    tmatrices[[i]][18,23] <- p_2b_go_to_3b * results[18 + 24 * (i - 1),]$double_exp +
      p_error_2b * results[18 + 24 * (i - 1),]$e_exp

    # Probability of 1-- to end of inning, starting with two out

    tmatrices[[i]][18,25] <- results[18 + 24 * (i - 1),]$f_out_exp + 
      results[18 + 24 * (i - 1),]$fc_o_exp + results[18 + 24 * (i - 1),]$fo_exp + 
      results[18 + 24 * (i - 1),]$so_exp
    
    # Probability of -2- to ---, starting with two out and no increase

    tmatrices[[i]][19,17] <- results[19 + 24 * (i - 1),]$hr_exp
    
    # Probability of -2- to 1--, starting with two out and no increase

    tmatrices[[i]][19,18] <- results[19 + 24 * (i - 1),]$single_exp

    # Probability of -2- to -2-, starting with two out and no increase
    
    tmatrices[[i]][19,19] <- results[19 + 24 * (i - 1),]$double_exp + 
      p_error_2b * results[19 + 24 * (i - 1),]$e_exp

    # Probability of -2- to --3, starting with two out and no increase

    tmatrices[[i]][19,20] <- results[19 + 24 * (i - 1),]$triple_exp

    # Probability of -2- to 12-, starting with two out and no increase

    tmatrices[[i]][19,21] <- results[19 + 24 * (i - 1),]$bb_exp + 
      results[19 + 24 * (i - 1),]$ibb_exp + results[19 + 24 * (i - 1),]$hbp_exp + 
      results[19 + 24 * (i - 1),]$ci_exp

    # Probability of -2- to 1-3, starting with two out and no increase

    tmatrices[[i]][19,22] <- p_error_1b * results[19 + 24 * (i - 1),]$e_exp + 
      results[19 + 24 * (i - 1),]$fc_exp

    # Probability of -2- to end of inning, starting with two out

    tmatrices[[i]][19,25] <- results[19 + 24 * (i - 1),]$fc_o_exp + 
      results[19 + 24 * (i - 1),]$f_out_exp + results[19 + 24 * (i - 1),]$so_exp

    # Probability of --3 to ---, starting with two out and no increase
    
    tmatrices[[i]][20,17] <- results[20 + 24 * (i - 1),]$hr_exp

    # Probability of --3 to 1--, starting with two out and no increase

    tmatrices[[i]][20,18] <- results[20 + 24 * (i - 1),]$single_exp + 
      p_error_1b * results[20 + 24 * (i - 1),]$e_exp +
      results[20 + 24 * (i - 1),]$fc_exp

    # Probability of --3 to -2-, starting with two out and no increase

    tmatrices[[i]][20,19] <- results[20 + 24 * (i - 1),]$double_exp + 
      p_error_2b * results[20 + 24 * (i - 1),]$e_exp

    # Probability of --3 to --3, starting with two out and no increase

    tmatrices[[i]][20,20] <- results[20 + 24 * (i - 1),]$triple_exp

    # Probability of --3 to 1-3, starting with two out and no increase

    tmatrices[[i]][20,22] <- results[20 + 24 * (i - 1),]$bb_exp + 
      results[20 + 24 * (i - 1),]$ibb_exp + results[20 + 24 * (i - 1),]$hbp_exp + 
      results[20 + 24 * (i - 1),]$ci_exp

    # Probability of --3 to end of inning, starting with two out

    tmatrices[[i]][20,25] <- results[20 + 24 * (i - 1),]$f_out_exp + 
      results[20 + 24 * (i - 1),]$fc_o_exp + results[20 + 24 * (i - 1),]$so_exp +
      results[20 + 24 * (i - 1),]$fo_exp

    # Probability of 12- to ---, starting with two out and no increase

    tmatrices[[i]][21,17] <- results[21 + 24 * (i - 1),]$hr_exp

    # Probability of 12- to -2-, starting with two out and no increase

    tmatrices[[i]][21,19] <- p_2b_score_from_1b * results[21 + 24 * (i - 1),]$double_exp

    # Probability of 12- to --3, starting with two out and no increase

    tmatrices[[i]][21,20] <- results[21 + 24 * (i - 1),]$triple_exp

    # Probability of 12- to 12-, starting with two out and no increase
    
    tmatrices[[i]][21,21] <- p_1b_first_to_second * results[21 + 24 * (i - 1),]$single_exp

    # Probability of 12- to 1-3, starting with two out and no increase

    tmatrices[[i]][21,22] <- p_1b_first_to_third * results[21 + 24 * (i - 1),]$single_exp

    # Probability of 12- to -23, starting with two out and no increase
    
    tmatrices[[i]][21,23] <- p_2b_go_to_3b * results[21 + 24 * (i - 1),]$double_exp +
      p_error_2b * results[21 + 24 * (i - 1),]$e_exp

    # Probability of 12- to 123, starting with two out and no increase
    
    tmatrices[[i]][21,24] <- results[21 + 24 * (i - 1),]$bb_exp + 
      results[21 + 24 * (i - 1),]$ibb_exp + results[21 + 24 * (i - 1),]$hbp_exp + 
      results[21 + 24 * (i - 1),]$ci_exp + p_error_1b * results[21 + 24 * (i - 1),]$e_exp +
      results[21 + 24 * (i - 1),]$fc_exp

    # Probability of 12- to end of inning, starting with two out

    tmatrices[[i]][21,25] <- results[21 + 24 * (i - 1),]$f_out_exp + 
      results[21 + 24 * (i - 1),]$fc_o_exp + results[21 + 24 * (i - 1),]$fo_exp + 
      results[21 + 24 * (i - 1),]$so_exp
    
    # Probability of 1-3 to ---, starting with two out and no increase
    
    tmatrices[[i]][22,17] <- results[22 + 24 * (i - 1),]$hr_exp

    # Probability of 1-3 to -2-, starting with two out and no increase

    tmatrices[[i]][22,19] <- p_2b_score_from_1b * results[22 + 24 * (i - 1),]$double_exp

    # Probability of 1-3 to --3, starting with two out and no increase

    tmatrices[[i]][22,20] <- results[22 + 24 * (i - 1),]$triple_exp

    # Probability of 1-3 to 12-, starting with two out and no increase
    
    tmatrices[[i]][22,21] <- p_1b_first_to_second * results[22 + 24 * (i - 1),]$single_exp +
      p_error_1b * results[22 + 24 * (i - 1),]$e_exp + results[22 + 24 * (i - 1),]$fc_exp

    # Probability of 1-3 to 1-3, starting with two out and no increase
    
    tmatrices[[i]][22,22] <- p_1b_first_to_third * results[22 + 24 * (i - 1),]$single_exp

    # Probability of 1-3 to -23, starting with two out and no increase
    
    tmatrices[[i]][22,23] <- p_2b_go_to_3b * results[22 + 24 * (i - 1),]$double_exp +
      p_error_2b * results[22 + 24 * (i - 1),]$e_exp

    # Probability of 1-3 to 123, starting with two out and no increase
    
    
    tmatrices[[i]][22,24] <- results[22 + 24 * (i - 1),]$bb_exp + 
      results[22 + 24 * (i - 1),]$ibb_exp + results[22 + 24 * (i - 1),]$hbp_exp + 
      results[22 + 24 * (i - 1),]$ci_exp

    # Probability of 1-3 to end of inning, starting with two out

    tmatrices[[i]][22,25] <- results[22 + 24 * (i - 1),]$f_out_exp + 
      results[22 + 24 * (i - 1),]$fc_o_exp + results[22 + 24 * (i - 1),]$fo_exp + 
      results[22 + 24 * (i - 1),]$so_exp

    # Probability of -23 to ---, starting with two out and no increase
    
    tmatrices[[i]][23,17] <- results[23 + 24 * (i - 1),]$hr_exp

    # Probability of -23 to 1--, starting with two out and no increase

    tmatrices[[i]][23,18] <- results[23 + 24 * (i - 1),]$single_exp

    # Probability of -23 to -2-, starting with two out and no increase

    tmatrices[[i]][23,19] <- results[23 + 24 * (i - 1),]$double_exp + 
      p_error_2b * results[23 + 24 * (i - 1),]$e_exp

    # Probability of -23 to --3, starting with two out and no increase
    
    tmatrices[[i]][23,20] <- results[23 + 24 * (i - 1),]$triple_exp

    # Probability of -23 to 1-3, starting with two out and no increase
    
    tmatrices[[i]][23,22] <- p_error_1b * results[23 + 24 * (i - 1),]$e_exp + 
      results[23 + 24 * (i - 1),]$fc_exp

    # Probability of -23 to 123, starting with two out and no increase

    tmatrices[[i]][23,24] <- results[23 + 24 * (i - 1),]$bb_exp + 
      results[23 + 24 * (i - 1),]$ibb_exp + results[23 + 24 * (i - 1),]$hbp_exp + 
      results[23 + 24 * (i - 1),]$ci_exp

    # Probability of -23 to end of inning, starting with two out

    tmatrices[[i]][23,25] <- results[23 + 24 * (i - 1),]$f_out_exp + 
      results[23 + 24 * (i - 1),]$fc_o_exp + results[23 + 24 * (i - 1),]$so_exp +
      results[23 + 24 * (i - 1),]$fo_exp

    # Probability of 123 to ---, starting with two out and no increase

    tmatrices[[i]][24,17] <- results[24 + 24 * (i - 1),]$hr_exp

    # Probability of 123 to -2-, starting with two out and no increase

    tmatrices[[i]][24,19] <- p_2b_score_from_1b * results[24 + 24 * (i - 1),]$double_exp

    # Probability of 123 to --3, starting with two out and no increase

    tmatrices[[i]][24,20] <- results[24 + 24 * (i - 1),]$triple_exp

    # Probability of 123 to 12-, starting with two out and no increase

    tmatrices[[i]][24,21] <- p_1b_first_to_second * results[24 + 24 * (i - 1),]$single_exp

    # Probability of 123 to 1-3, starting with two out and no increase

    tmatrices[[i]][24,22] <- p_1b_first_to_third * results[24 + 24 * (i - 1),]$single_exp
    
    # Probability of 123 to -23, starting with two out and no increase

    tmatrices[[i]][24,23] <- p_2b_go_to_3b * results[24 + 24 * (i - 1),]$double_exp +
      p_error_2b * results[24 + 24 * (i - 1),]$e_exp

    # Probability of 123 to 123, starting with two out and no increase
    
    tmatrices[[i]][24,24] <- results[24 + 24 * (i - 1),]$bb_exp + 
      results[24 + 24 * (i - 1),]$ibb_exp + results[24 + 24 * (i - 1),]$hbp_exp + 
      results[24 + 24 * (i - 1),]$ci_exp + p_error_1b * results[24 + 24 * (i - 1),]$e_exp +
      results[24 + 24 * (i - 1),]$fc_exp

    # Probability of 123 to end of inning, starting with two out
    
    tmatrices[[i]][24,25] <- results[24 + 24 * (i - 1),]$f_out_exp + 
      results[24 + 24 * (i - 1),]$fc_o_exp + results[24 + 24 * (i - 1),]$fo_exp + 
      results[24 + 24 * (i - 1),]$so_exp

    # Filling in the last element of the matrix
    
    tmatrices[[i]][25,25] <- 1
    
  }

  # Normalizing the matrix for proper calculation
  # 
  # for (i in 1:9) {
  #   for (j in 1:25) {
  #     rowsum <- sum(tmatrices[[i]][j,])
  #     print(c(i, j, rowsum))
  #     tmatrices[[i]][j,] <- tmatrices[[i]][j,] / rowsum
  #   }
  # }
  
  return(tmatrices)
  
}
