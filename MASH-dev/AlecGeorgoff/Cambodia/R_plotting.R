rm(list = ls())

library(rootSolve, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")

p = 0.3
c = 0.15

R_v_fun <- function(chi_v, S_v = 8.8) {
  return((chi_v / ((1 - chi_v) * (1 - p)))*((1 + S_v * c * chi_v) / chi_v))
}

chi_v_values <- seq(0,0.8,0.01)

R_v_values <- R_v_fun(chi_v_values)

plot(R_v_values, chi_v_values)

R_f_fun <- function(chi_f, chi_v, R_0_v, S_f = 8.8, S_v = 8.8) {
  a_term = (1 + S_f * c * chi_f) / (p * chi_f)
  b_term = chi_f / (1 - chi_f)
  c_term = chi_v / (1 + S_v * c * chi_v)
  
  return(a_term * (b_term - R_0_v * (1 - p) * c_term))
}

# chi_f_values <- seq(0,0.8,0.1)
chi_f_values <- rep(x = 0.4, times = length(chi_v_values))

# is it screwing up because we're pairing our chi_v and chi_f values? they should be independent maybe...
  # each chi_v value should have a combination with every possible chi_f value

R_f_values <- R_f_fun(chi_f = chi_f_values,
                      chi_v = chi_v_values,
                      R_0_v = R_v_values)

plot(R_f_values, chi_f_values)

results <- data.table(chi_f = chi_f_values, chi_v = chi_v_values, R_f = R_f_values, R_v = R_v_values)
View(results)

# H_v = 5000
# R_v = 6
# S_v = 8.8
# chi_test <- (H_v * (p - 1) * R_v + 1) / ((p - 1) * R_v - c * S_v)