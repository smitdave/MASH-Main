###################################
# multiroot_solve.R
#
# Author: Alec Georgoff
#
# Purpose: Find prevalence values in the forest and village at equilibrium, given
#          all other parameters are held constant
###################################


rm(list = ls())

require(rootSolve, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")
require(data.table)
require(plotly, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")

###################################
#
# Define parameters
#
###################################

# Define general parameters
b = 0.55  # Proportion of bites by infectious mosquitoes that cause an infection
r = 1/200 # Rate that humans recover from an infection
c = 0.15   # Proportion of mosquitoes infected after biting infectious human
n = 12     # Time for sporogonic cycle

# Define forest-specific parameters
a_f = 0.88   # Human blood feeding rate in forest
g_f = 0.1   # Per capita death rate of mosquitoes in forest
H_f = 2000  # Human population (density?) in forest
X_f = 0     # Number of infected forest-goers
p = 0.1     # Proportion of time forest-goers spend in the forest
V_f = 500  # Vector population in forest
Y_f = 0     # Number of infected vectors in forest
Z_f = exp(-g_f*n)*Y_f # Number of infectious vectors in forest

# Define village-specific parameters
a_v = 0.88   # Human blood feeding rate in village
g_v = 0.1   # Per capita death rate of mosquitoes in village
H_v = 5000  # Human population (density?) in village
X_v = 0     # Number of infected villagers
# 1 - p     # Proportion of time forest-goers spend in the village
V_v = 100   # Vector population in village
Y_v = 0     # Number of infected vectors in village
Z_v = exp(-g_v*n)*Y_v # Number of infectious vectors in village
c <- 0.15
p <- 0.3

# set up function to calculate R values:
calculate_R <- function(V, a, b, c, g, n, H, r) {
  R = (V * a^2 * b * c * exp(-g * n)) / (H * g * r)
  return(R)
}

# R_0 values:
R_0_v <- calculate_R(V = V_v, a = a_v, b = b, c = c, g = g_v, n = n, H = H_v, r = r)
R_0_f <- calculate_R(V = V_f, a = a_f, b = b, c = c, g = g_f, n = n, H = H_f, r = r)

# S values:
S_v <- a_v / g_v
S_f <- a_f / g_f

# choose "starting point" for solver to look for roots:
chi_v_start <- 0.9
chi_f_start <- 0.1

# convert to number of humans:
X_v_start <- chi_v_start * H_v
X_f_start <- chi_f_start * H_f



###################################
#
# Set up the equations as a function
#
###################################

model <- function(X, R_0_v, R_0_f, H_v, H_f, S_v, S_f, c_val, p_val) {
  # X[1] = X_f
  # X[2] = X_v
  
  # convert to prevalence:
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

find_roots <- function(R_0_v, R_0_f,
                       H_v. = H_v, H_f. = H_f,
                       S_v. = S_v, S_f. = S_f,
                       c_val = c, p_val = p,
                       chi_v_start. = chi_v_start, chi_f_start. = chi_f_start) {
  
  # convert start point from prevalence to # of humans:
  X_v_start <- chi_v_start. * H_v
  X_f_start <- chi_f_start. * H_f
  
  # use multiroot solver to find roots:
  ss <- multiroot(f = model, start = c(X_v_start, X_f_start),
                  R_0_v = R_0_v, R_0_f = R_0_f,
                  H_v = H_v., H_f = H_f.,
                  S_v = S_v., S_f = S_f.,
                  c_val = c_val, p_val = p_val)
  
  # convert results to prevalence:
  chi_v_SS <- ss$root[1] / H_v
  chi_f_SS <- ss$root[2] / H_f
  
  return(c(chi_v_SS, chi_f_SS))
}

###################################
#
# Cycle through R values
#
###################################

# set R values to cycle through:
R_0_v_values <- seq(0, 10, 0.1)
R_0_f_values <- seq(0, 10, 0.1)

# create data table to store results:
results <- data.table(R_0_v = rep(0, times = length(R_0_f_values) * length(R_0_v_values)),
                      R_0_f = 0, chi_v = 0, chi_f = 0)

i <- 1

for (v in R_0_v_values) {
  for (f in R_0_f_values) {
    # record current R values:
    results[i, R_0_v := v]
    results[i, R_0_f := f]
    # solve for roots at those R values:
    results[i, chi_v := find_roots(v, f)[1]]
    results[i, chi_f := find_roots(v, f)[2]]
    
    # print progress:
    cat("R_0_v =", v, ", R_0_f =", f, " \r", file = "", sep = " ")
    flush.console()
    
    i <- i + 1
  }
}

p <- plot_ly(x = results$R_0_v,
             y = results$R_0_f,
             z = results$chi_v,
             type = "heatmap",
             height = 800, width = 960) %>%
  layout(title = "Equilibrium Prevalence in Village as a Function of R_0 in Village and Forest",
         titlefont = list(size = 16),
         xaxis = list(title = "R_0 Value, Village",
                      titlefont = list(size = 20)),
         yaxis = list(title = "R_0 Value, Forest",
                      titlefont = list(size = 20)))

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