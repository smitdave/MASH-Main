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
library(data.table)

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

model <- function(X, R_0_v, R_0_f, H_v, H_f, S_v, S_f, c_val, p_val) {
  # X[1] = X_f
  # X[2] = X_v
  
  chi_f = X[1] / H_f
  chi_v = ((1 - p_val) * X[1] + X[2]) / ((1 - p_val) * H_f + H_v)
  
  equation_village <- (R_0_v * (1 - p_val) * (chi_v / (1 + S_v * c_val * chi_v))) * (H_v - X[2]) - X[2]
  
  equation_forest <- (R_0_v * (1 - p_val) * chi_v / (1 + S_v * c_val * chi_v) + R_0_f * p_val * chi_f / 
                        (1 + S_f * c_val * chi_f)) * (H_f - X[1]) - X[1]
  
  return(c(equation_village, equation_forest))
}

###################################
#
# Solve for roots
#
###################################

# ss <- multiroot(f = model, start = c(X_v_start, X_f_start))
# 
# chi_v_SS <- ss$root[1] / H_v
# chi_f_SS <- ss$root[2] / H_f
# 
# print(paste0("Starting village prevalence = ", chi_v_start))
# print(paste0("Equilibrium village prevalence = ", chi_v_SS))
# print(paste0("Starting forest prevalence = ", chi_f_start))
# print(paste0("Equilibrium forest prevalence = ", chi_f_SS))

find_roots <- function(R_0_v, R_0_f,
                       H_v = 5000, H_f = 2000,
                       S_v = 8.8, S_f = 8.8,
                       c_val = 0.15, p_val = 0.3,
                       chi_v_start = 0.5, chi_f_start = 0.5) {
  
  X_v_start <- chi_v_start * H_v
  X_f_start <- chi_f_start * H_f
  
  ss <- multiroot(f = model, start = c(X_v_start, X_f_start),
                  R_0_v = R_0_v, R_0_f = R_0_f,
                  H_v = H_v, H_f = H_f,
                  S_v = S_v, S_f = S_f,
                  c_val = c_val, p_val = p_val)
  
  chi_v_SS <- ss$root[1] / H_v
  chi_f_SS <- ss$root[2] / H_f
  
  return(c(chi_v_SS, chi_f_SS))
}

# NEXT STEPS:
  # loop thru a bunch of R values and store results in a data frame
  # graph that shit
R_0_v_values <- seq(0.1, 10, 0.1)
R_0_f_values <- seq(0.1, 10, 0.1)

results <- data.table(R_0_v = rep(0, times = length(R_0_f_values) * length(R_0_v_values)),
                      R_0_f = 0, chi_v = 0, chi_f = 0)

i <- 1

for (v in R_0_v_values) {
  for (f in R_0_f_values) {
    results[i, R_0_v := v]
    results[i, R_0_f := f]
    results[i, chi_v := find_roots(v, f)[1]]
    results[i, chi_f := find_roots(v, f)[2]]
    
    i <- i + 1
  }
}

library(plotly, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")

p <- plot_ly(x = results$R_0_v,
             y = results$R_0_f,
             z = results$chi_v,
             type = "heatmap") %>%
  layout(title = "Equilibrium Prevalence in Village as a Function of R_0 in Village and Forest",
         xaxis = list(title = "R_0 Value, Village"),
         yaxis = list(title = "R_0 Value, Forest"))

p

p2 <- plot_ly(x = results$R_0_v,
              y = results$R_0_f,
              z = results$chi_v,
              type = "scatter3d") %>%
  layout(
    title = "Malaria Prevalence in the Village as a Function of R_0",
    scene = list(
      xaxis = list(title = "R_0, Village"),
      yaxis = list(title = "R_0, Forest"),
      zaxis = list(title = "Village Malaria Prevalence")
    )
  )

p2