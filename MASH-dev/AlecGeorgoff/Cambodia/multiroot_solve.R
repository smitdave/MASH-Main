###################################
# multiroot_solve.R
#
# Author: Alec Georgoff
#
# Purpose: Find prevalence values in the forest and village at equilibrium, given
#          all other parameters are held constant
###################################


rm(list = ls())

library(rootSolve, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")

###################################
#
# Define constant parameters
#
###################################

c <- 0.15
p <- 0.3

S_v <- 8.8
S_f <- 8.8

# number of humans:
H_v <- 5000
H_f <- 2000

# R_0 values:
R_0_v <- 4
R_0_f <- 4

# choose "starting point" for solver to look for roots:
chi_v_start <- 0.9
chi_f_start <- 0.1

# convert to number of humans:
X_v_start <- chi_v_start * H_v
X_f_start <- chi_f_start * H_f

###################################
#
# Set up the model as a function
#
###################################

model <- function(X) {
  # X[1] = X_f
  # X[2] = X_v
  
  chi_f = X[1] / H_f
  chi_v = ((1 - p) * X[1] + X[2]) / ((1 - p) * H_f + H_v)
  
  equation_village <- (R_0_v * (1 - p) * (chi_v / (1 + S_v * c * chi_v))) * (H_v - X[2]) - X[2]
  
  equation_forest <- (R_0_v * (1 - p) * chi_v / (1 + S_v * c * chi_v) + R_0_f * p * chi_f / (1 + S_f * c * chi_f)) * (H_f - X[1]) - X[1]
  
  return(c(equation_village, equation_forest))
}

###################################
#
# Solve for roots
#
###################################

ss <- multiroot(f = model, start = c(X_v_start, X_f_start))

chi_v_SS <- ss$root[1] / H_v
chi_f_SS <- ss$root[2] / H_f

print(paste0("Starting village prevalence = ", chi_v_start))
print(paste0("Equilibrium village prevalence = ", chi_v_SS))
print(paste0("Starting forest prevalence = ", chi_f_start))
print(paste0("Equilibrium forest prevalence = ", chi_f_SS))

# NEXT STEPS:
  # loop thru a bunch of R values and store results in a data frame
  # graph that shit
