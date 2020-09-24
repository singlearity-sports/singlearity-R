# Functions (in separate file) to get transition matrix

source("examples/common.R")

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
                        dp_exp = 137 / 27215,
                        e_exp = 955 / 58537,
                        f_out_exp = 23875 / 62087,
                        fc_exp = 135 / 27215,
                        fc_o_exp = 110 / 27215,
                        fo_exp = 1195 / 27215,
                        gdp_exp = 1146 / 27215,
                        hbp_exp = 772 / 62087,
                        hr_exp = 2156 / 62087,
                        ibb_exp = 187 / 62087,
                        sf_exp = 370 / 6356,
                        sf_dp_exp = 2 / 6356,
                        sh_exp = 112 / 27215,
                        single_exp = 8439 / 62087,
                        so_exp = 14540 / 62087,
                        so_dp_exp = 42 / 27215,
                        tp_exp = 1 / 9155,
                        triple_exp = 225 / 62087) {
  
}