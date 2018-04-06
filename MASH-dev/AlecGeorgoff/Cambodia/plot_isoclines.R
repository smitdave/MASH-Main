##########
# plot_isoclines.R
# 
# Author: Alec Georgoff
#
# Purpose: Solve system of differential equations for a simple two-system (forest and village) landscape of malaria transmission
##########

rm(list = ls())

library(ggplot2)

# Define general parameters
b = 0.55  # Proportion of bites by infectious mosquitoes that cause an infection
r = 1/200 # Rate that humans recover from an infection
c = 0.15   # Proportion of mosquitoes infected after biting infectious human
n = 12     # Time for sporogonic cycle

# Define forest-specific parameters
a_f = 0.88   # Human blood feeding rate in forest
g_f = 0.1   # Per capita death rate of mosquitoes in forest
H_f = 1000  # Human population (density?) in forest
X_f = 0     # Number of infected forest-goers
p = 0.7     # Proportion of time forest-goers spend in the forest
V_f = 1000  # Vector population in forest
Y_f = 0     # Number of infected vectors in forest
Z_f = exp(-g_f*n)*Y_f # Number of infectious vectors in forest

# Define village-specific parameters
a_v = 0.88   # Human blood feeding rate in village
g_v = 0.1   # Per capita death rate of mosquitoes in village
H_v = 5000  # Human population (density?) in village
X_v = 0     # Number of infected villagers
# 1 - p     # Proportion of time forest-goers spend in the village
V_v = 5000   # Vector population in village
Y_v = 0     # Number of infected vectors in village
Z_v = exp(-g_v*n)*Y_v # Number of infectious vectors in village

# Initial conditions
X_f0 = 50
X_v0 = 50
Y_f0 = 1000
Y_v0 = 500

X_f_equilibrium = function(X_f, X_v){
  result <- ((b*(1-p)*a_v*exp(-g_v*n)*a_v*c*V_v*((1-p)*X_f+X_v))/((H_f*(1-p)+H_v)*(H_f*(1-p)+H_v+a_v*c*((1-p)*X_f+X_v)))+(b*p*a_f*exp(-g_f*n)*a_f*c*V_f*X_f)/(H_f*H_f*(g_f+(a_f*c*X_f/H_f))))*(H_f-X_f) -
    r*X_f
  return(abs(result))
}

my_optim_function <- function(x){
  
  fit1 <- optim(
    par = 0,
    fn = X_f_equilibrium,
    method = "BFGS",
    X_f = x
  )
  
  out <- fit1$par
  return(out)
  
}

x_vals <- 910:930
y_vals <- sapply(x_vals, my_optim_function)

df <- as.data.frame(cbind(x_vals, y_vals))

