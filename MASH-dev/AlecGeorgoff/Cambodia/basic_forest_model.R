##########
# basic_forest_model.R
# 
# Author: Alec Georgoff
#
# Purpose: Solve system of differential equations for a simple two-system (forest and village) landscape of malaria transmission
##########

rm(list = ls())

library(deSolve, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs")
library(ggplot2)

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

# Initial conditions (SET AS PREVALENCE)
X_f0 = 0.1
X_v0 = 0.1
Y_f0 = 0.2
Y_v0 = 0.1

# Initial conditions (scale to population sizes)
X_f0 = X_f0 * H_f
X_v0 = X_v0 * H_v
Y_f0 = Y_f0 * V_f
Y_v0 = Y_v0 * V_v

# Set time scale
times = 0:10000

require(deSolve)
FM.ode = function(t,y,parms){with(as.list(c(y,parms)),{
  # Need to recalculate Z for every iteration
  Z_f = exp(-g_f*n)*Y_f
  Z_v = exp(-g_v*n)*Y_v
  
  # Change in number of infected forest goers:
  dX_f = ((b*(1-p)*a_v*Z_v)/(H_f*(1-p)+H_v)+(b*p*a_f*Z_f)/H_f)*(H_f-X_f) - r*X_f
  
  # Change in number of infected villagers:
  dX_v = ((b*a_v*Z_v)/(H_f*(1-p)+H_v))*(H_v-X_v) - r*X_v
  
  # Change in number of infected vectors in forest:
  dY_f = ((a_f*c*X_f)/H_f)*(V_f-Y_f) - g_f*Y_f
  
  # Change in number of infected vectors in village:
  dY_v = a_v*c*((X_f*(1-p)+X_v)/(H_f*(1-p)+H_v))*(V_v-Y_v) - g_v*Y_v
  
  list(c(dX_f, dX_v, dY_f, dY_v))
})}

parms = c(b=b, r=r, c=c, n=n,
          a_f=a_f, g_f=g_f, H_f=H_f, X_f=X_f, p=p, V_f=V_f, Y_f=Y_f, Z_f=Z_f,
          a_v=a_v, g_v=g_v, H_v=H_v, X_v=X_v, V_v=V_v, Y_v=Y_v, Z_v=Z_v)

inits = c(X_f=X_f0, X_v=X_v0, Y_f=Y_f0, Y_v=Y_v0)

out = data.frame(lsoda(inits, times, FM.ode, parms))

Y_f_plot <- ggplot(out,
                   aes(x = time, y = Y_f)) +
  geom_point() +
  scale_y_continuous(limits = c(0, NA))

Y_f_plot + ggtitle("Number of Infected Vectors in Forest") +
  labs(x = "Time Step", y = "Infected Vectors")

Y_v_plot <- ggplot(out,
                   aes(x = time, y = Y_v)) +
  geom_point() +
  scale_y_continuous(limits = c(0, NA))

Y_v_plot + ggtitle("Number of Infected Vectors in Village") +
  labs(x = "Time Step", y = "Infected Vectors")

X_f_plot <- ggplot(out,
                   aes(x = time, y = X_f)) +
  geom_point() +
  scale_y_continuous(limits = c(0, NA))

X_f_plot + ggtitle("Number of Infected Forest Goers") +
  labs(x = "Time Step", y = "Infected Forest Goers")

X_v_plot <- ggplot(out,
                   aes(x = time, y = X_v)) +
  geom_point() +
  scale_y_continuous(limits = c(0, NA))

X_v_plot + ggtitle("Number of Infected Villagers") +
  labs(x = "Time Step", y = "Infected Villagers")

X_v_SS <- out$X_v[out$time == max(times)]
X_f_SS <- out$X_f[out$time == max(times)]

S_v <- a_v/g_v
chi_v <- ((1 - p) * X_f_SS + X_v_SS) / ((1 - p) * H_f + H_v) # prevalence in the village

S_f <- a_f/g_f
chi_f <- X_f_SS / H_f # prevalence in the forest

R_0_v <- (X_v_SS * (1 + S_v * c * chi_v)) / ((H_v - X_v_SS) * chi_v * (1 - p))

R_0_f <- ((1 + S_f * c * chi_f) / (p * chi_f))*((chi_f / (1 - chi_f)) - R_0_v * (1 - p) * (chi_v / (1 + S_v * c * chi_v)))

print(paste0("R_0 value in village (at beginning of outbreak) = ", R_0_v))
print(paste0("R_0 value in forest (at beginning of outbreak) = ", R_0_f))
print(paste0("Prevalence in village = ", chi_v))
print(paste0("Prevalence in forest = ", chi_f))