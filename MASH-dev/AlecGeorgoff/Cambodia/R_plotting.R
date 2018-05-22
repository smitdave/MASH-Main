rm(list = ls())

library(rootSolve, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")

p = 0.5
c = 0.15

R_v_fun <- function(chi_v, S_v = 8.8) {
  return((chi_v / ((1 - chi_v) * (1 - p)))*((1 + S_v * c * chi_v) / chi_v))
}

chi_v_values <- seq(0,0.8,0.01)

R_v_values <- R_v_fun(chi_v_values)

plot(R_v_values, chi_v_values)

R_f_fun <- function(chi_f, chi_v, R_0_v, S_f = 8.8, S_v = 8.8) {
  a = (1 + S_f * c * chi_f) / (p * chi_f)
  b = chi_f / (1 - chi_f)
  c = chi_v / (1 + S_v * c * chi_v)
  
  return(a * (b - R_0_v * (1 - p) * c))
}

chi_f_values <- seq(0,0.8,0.01)

R_f_values <- R_f_fun(chi_f = chi_f_values,
                      chi_v = chi_v_values,
                      R_0_v = R_v_values)

plot(R_f_values, chi_f_values)
