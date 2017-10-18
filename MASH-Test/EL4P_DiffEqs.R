#################################################
## MASH EL4P MODEL ANALYTICS                   ##
## John Marshall (john.marshall@berkeley.edu)  ##
## 10/5/2017                                   ##
#################################################

#################################################
## VECTOR OF PARAMETERS:                       ##
#################################################

parameters <- c(
  M_eq = 10000, # Equilibrium number of adult female mosquitoes
  beta = 32, # Mean number of eggs laid per adult femal parameters["beta"]
  mu_E = 0.168, # Egg daily death rate
  mu_L = 0.168, # Larval daily death rate
  mu_P = 0.168, # Pupal daily death rate
  mu_M = 0.123, # Adult female daily death rate
  d_E = 1, # Mean duration of egg stage (days)
  d_L = 14, # Mean duration of larval stage (days)
  d_P = 1) # Mean duration of pupal stage (days)

numDays <- 3*30 # Simulation over 3 months

#################################################
## ELP DIFFERENTIAL EQUATIONS:                 ##
#################################################

library(deSolve)

# Derivatives function for Ross-Macdonald malaria model:
ELP_ODEs <- function(time, state, parameters) {

  # Parameters:
  M_eq <- parameters[["M_eq"]]
  beta <- parameters[["beta"]]
  mu_E <- parameters[["mu_E"]]
  mu_L <- parameters[["mu_L"]]
  mu_P <- parameters[["mu_P"]]
  mu_M <- parameters[["mu_M"]]
  d_E <- parameters[["d_E"]]
  d_L <- parameters[["d_L"]]
  d_P <- parameters[["d_P"]]

  # Equilibria & larval carrying capacity (derived):
  P_eq <- M_eq * 2 * d_P * mu_M
  E_eq <- beta * M_eq / (mu_E + (1/d_E))
  L_eq <- (mu_P + (1/d_P)) * d_L * P_eq
  K <- L_eq/(E_eq/(mu_L * d_E * L_eq) - 1/(mu_L * d_L) - 1)

  # States:
  E <- state["E"]
  L <- state["L"]
  P <- state["P"]
  M <- state["M"]

  # Derivatives:
  dE <- beta*M - mu_E*E - E/d_E
  dL <- E/d_E - mu_L*(1 + L/K)*L - L/d_L
  dP <- L/d_L - mu_P*P - P/d_P
  dM <- (1/2)*(P/d_P) - mu_M*M

  return(list(c(dE, dL, dP, dM)))
}

# Equilibria & larval carrying capacity (derived):
M_eq <- parameters[["M_eq"]]
beta <- parameters[["beta"]]
mu_E <- parameters[["mu_E"]]
mu_L <- parameters[["mu_L"]]
mu_P <- parameters[["mu_P"]]
mu_M <- parameters[["mu_M"]]
d_E <- parameters[["d_E"]]
d_L <- parameters[["d_L"]]
d_P <- parameters[["d_P"]]

P_eq <- M_eq * 2 * d_P * mu_M
E_eq <- beta * M_eq / (mu_E + (1/d_E))
L_eq <- (mu_P + (1/d_P)) * d_L * P_eq

## Trajectory for ELP immature form model:
trajectory <- ode(y = c(E = E_eq, L = L_eq, P = P_eq, M = M_eq),
                  times = seq(from = 0, to = numDays, by = 0.01),
                  parms = parameters, func = ELP_ODEs)

## The first few entries of the trajectory matrix:
head(trajectory)

## Convert the trajectory matrix into a data frame:
trajectory_df <- data.frame(trajectory)

## Plot the number of people in each state over time using "ggplot()":
library(ggplot2)

ggplot(trajectory_df, aes(x=time, y=trajectory_df, color=State)) +
  geom_line(aes(y = E, col = "E"), size = 1.2) +
  geom_line(aes(y = L, col = "L"), size = 1.2) +
  geom_line(aes(y = P, col = "P"), size = 1.2) +
  geom_line(aes(y = M, col = "M"), size = 1.2) +
  labs(x = "Time (days)", y = "Number of individuals in each life stage")

#################################################
## ELP DIFFERENCE EQUATIONS:                   ##
#################################################

timeStepSize <- 0.01 # Time-step size of 0.01 days

# Calculate trajectory predicted by a system of difference equations:

ELP_diffEqs <- function(parameters, numDays, timeStepSize) {

  # Extract parameters from parameter vector:
  M_eq <- parameters[["M_eq"]]
  beta <- parameters[["beta"]]
  mu_E <- parameters[["mu_E"]]
  mu_L <- parameters[["mu_L"]]
  mu_P <- parameters[["mu_P"]]
  mu_M <- parameters[["mu_M"]]
  d_E <- parameters[["d_E"]]
  d_L <- parameters[["d_L"]]
  d_P <- parameters[["d_P"]]

  # Equilibria & larval carrying capacity (derived):
  P_eq <- M_eq * 2 * d_P * mu_M
  E_eq <- beta * M_eq / (mu_E + (1/d_E))
  L_eq <- (mu_P + (1/d_P)) * d_L * P_eq
  K <- L_eq/(E_eq/(mu_L * d_E * L_eq) - 1/(mu_L * d_L) - 1)

  # Initial state values (equilibrium solution):
  E <- E_eq
  L <- L_eq
  P <- P_eq
  M <- M_eq
  time <- 0

  numTimeSteps <- (numDays / timeStepSize) + 1

  for (i in 2:numTimeSteps) {
    E[i] <- (beta*timeStepSize) * M[i-1] + exp(-mu_E*timeStepSize) * (1-(1-exp(-timeStepSize/d_E))) * E[i-1]
    L[i] <- exp(-mu_E*timeStepSize) * (1-exp(-timeStepSize/d_E)) * E[i-1] + exp(-mu_L*timeStepSize*(1+(L[i-1]/K))) * (1-(1-exp(-timeStepSize/d_L))) * L[i-1]
    P[i] <- exp(-mu_L*timeStepSize*(1+(L[i-1]/K))) * (1-exp(-timeStepSize/d_L)) * L[i-1] + exp(-mu_P*timeStepSize) * (1-(1-exp(-timeStepSize/d_P))) * P[i-1]
    M[i] <- exp(-mu_P*timeStepSize) * (1-exp(-timeStepSize/(2*d_P))) * P[i-1] + exp(-mu_M*timeStepSize) * M[i-1]
    time[i] <- time[i-1] + timeStepSize
  }

  ## Return the results as a data frame:
  results <- data.frame(E, L, P, M, time)
  return(results)
}

trajectory <- ELP_diffEqs(parameters, numDays, timeStepSize)

## The first few entries of the trajectory matrix:
head(trajectory)

## Convert the trajectory matrix into a data frame:
trajectory_df <- data.frame(trajectory)
time <- trajectory_df$time

## Plot the number of people in each state over time using "ggplot()":
library(ggplot2)

ggplot(trajectory_df, aes(x=time, y=trajectory_df, color=State)) +
  geom_line(aes(y = E, col = "E"), size = 1.2) +
  geom_line(aes(y = L, col = "L"), size = 1.2) +
  geom_line(aes(y = P, col = "P"), size = 1.2) +
  geom_line(aes(y = M, col = "M"), size = 1.2) +
  labs(x = "Time (days)", y = "Number of individuals in each life stage")

#################################################
## EL4P DIFFERENTIAL EQUATIONS:                ##
#################################################

library(deSolve)

# Derivatives function for Ross-Macdonald malaria model:
EL4P_ODEs <- function(time, state, parameters) {

  # Parameters:
  M_eq <- parameters[["M_eq"]]
  beta <- parameters[["beta"]]
  mu_E <- parameters[["mu_E"]]
  mu_L <- parameters[["mu_L"]]
  mu_P <- parameters[["mu_P"]]
  mu_M <- parameters[["mu_M"]]
  d_E <- parameters[["d_E"]]
  d_L <- parameters[["d_L"]]
  d_P <- parameters[["d_P"]]

  # Equilibria & larval carrying capacity (derived):
  P_eq <- M_eq * 2 * d_P * mu_M
  E_eq <- beta * M_eq / (mu_E + (1/d_E))
  L1_eq <- (beta^(3/4) * d_L * M_eq * (1 + d_P * mu_P)^(1/4) * mu_M^(1/4)) /
    (2^(7/4) * (1 + d_E * mu_E)^(3/4))
  L2_eq <- (beta^(2/4) * d_L * M_eq * (1 + d_P * mu_P)^(2/4) * mu_M^(2/4)) /
    (2^(6/4) * (1 + d_E * mu_E)^(2/4))
  L3_eq <- (beta^(1/4) * d_L * M_eq * (1 + d_P * mu_P)^(3/4) * mu_M^(3/4)) /
    (2^(5/4) * (1 + d_E * mu_E)^(1/4))
  L4_eq <- (d_L * M_eq * (1 + d_P * mu_P) * mu_M) / 2
  L_eq <- L1_eq + L2_eq + L3_eq + L4_eq
  K <- L_eq / (E_eq/(mu_L * d_E * L1_eq) - 4/(mu_L * d_L) - 1)

  # States:
  E <- state["E"]
  L1 <- state["L1"]
  L2 <- state["L2"]
  L3 <- state["L3"]
  L4 <- state["L4"]
  P <- state["P"]
  M <- state["M"]

  L <- L1 + L2 + L3 + L4

  # Derivatives:
  dE <- beta*M - mu_E*E - E/d_E
  dL1 <- E/d_E - mu_L*(1 + L/K)*L1 - 4*L1/d_L
  dL2 <- 4*L1/d_L - mu_L*(1 + L/K)*L2 - 4*L2/d_L
  dL3 <- 4*L2/d_L - mu_L*(1 + L/K)*L3 - 4*L3/d_L
  dL4 <- 4*L3/d_L - mu_L*(1 + L/K)*L4 - 4*L4/d_L
  dP <- 4*L4/d_L - mu_P*P - P/d_P
  dM <- (1/2)*(P/d_P) - mu_M*M

  return(list(c(dE, dL1, dL2, dL3, dL4, dP, dM)))
}

# Equilibria & larval carrying capacity (derived):
M_eq <- parameters[["M_eq"]]
beta <- parameters[["beta"]]
mu_E <- parameters[["mu_E"]]
mu_L <- parameters[["mu_L"]]
mu_P <- parameters[["mu_P"]]
mu_M <- parameters[["mu_M"]]
d_E <- parameters[["d_E"]]
d_L <- parameters[["d_L"]]
d_P <- parameters[["d_P"]]

P_eq <- M_eq * 2 * d_P * mu_M
E_eq <- beta * M_eq / (mu_E + (1/d_E))
L1_eq <- (beta^(3/4) * d_L * M_eq * (1 + d_P * mu_P)^(1/4) * mu_M^(1/4)) /
  (2^(7/4) * (1 + d_E * mu_E)^(3/4))
L2_eq <- (beta^(2/4) * d_L * M_eq * (1 + d_P * mu_P)^(2/4) * mu_M^(2/4)) /
  (2^(6/4) * (1 + d_E * mu_E)^(2/4))
L3_eq <- (beta^(1/4) * d_L * M_eq * (1 + d_P * mu_P)^(3/4) * mu_M^(3/4)) /
  (2^(5/4) * (1 + d_E * mu_E)^(1/4))
L4_eq <- (d_L * M_eq * (1 + d_P * mu_P) * mu_M) / 2
L_eq <- L1_eq + L2_eq + L3_eq + L4_eq

## Trajectory for ELP immature form model:
trajectory <- ode(y = c(E = E_eq, L1 = L1_eq, L2 = L2_eq, L3 = L3_eq,
                        L4 = L4_eq, P = P_eq, M = M_eq),
                  times = seq(from = 0, to = numDays, by = 0.01),
                  parms = parameters, func = EL4P_ODEs)

## The first few entries of the trajectory matrix:
head(trajectory)

## Convert the trajectory matrix into a data frame:
trajectory_df <- data.frame(trajectory)

trajectory_df$L <- trajectory_df$L1 + trajectory_df$L2 +
                   trajectory_df$L3 + trajectory_df$L4

## Plot the number of people in each state over time using "ggplot()":
library(ggplot2)

ggplot(trajectory_df, aes(x=time, y=trajectory_df, color=State)) +
  geom_line(aes(y = E, col = "E"), size = 1.2) +
  geom_line(aes(y = L, col = "L"), size = 1.2) +
  geom_line(aes(y = P, col = "P"), size = 1.2) +
  geom_line(aes(y = M, col = "M"), size = 1.2) +
  labs(x = "Time (days)", y = "Number of individuals in each life stage")

#################################################
## EL4P DIFFERENCE EQUATIONS:                  ##
#################################################

timeStepSize <- 0.01 # Time-step size of 0.01 days

# Calculate trajectory predicted by a system of difference equations:

EL4P_diffEqs <- function(parameters, numDays, timeStepSize) {

  # Extract parameters from parameter vector:
  M_eq <- parameters[["M_eq"]]
  beta <- parameters[["beta"]]
  mu_E <- parameters[["mu_E"]]
  mu_L <- parameters[["mu_L"]]
  mu_P <- parameters[["mu_P"]]
  mu_M <- parameters[["mu_M"]]
  d_E <- parameters[["d_E"]]
  d_L <- parameters[["d_L"]]
  d_P <- parameters[["d_P"]]

  # Equilibria & larval carrying capacity (derived):
  P_eq <- M_eq * 2 * d_P * mu_M
  E_eq <- beta * M_eq / (mu_E + (1/d_E))
  L1_eq <- (beta^(3/4) * d_L * M_eq * (1 + d_P * mu_P)^(1/4) * mu_M^(1/4)) /
    (2^(7/4) * (1 + d_E * mu_E)^(3/4))
  L2_eq <- (beta^(2/4) * d_L * M_eq * (1 + d_P * mu_P)^(2/4) * mu_M^(2/4)) /
    (2^(6/4) * (1 + d_E * mu_E)^(2/4))
  L3_eq <- (beta^(1/4) * d_L * M_eq * (1 + d_P * mu_P)^(3/4) * mu_M^(3/4)) /
    (2^(5/4) * (1 + d_E * mu_E)^(1/4))
  L4_eq <- (d_L * M_eq * (1 + d_P * mu_P) * mu_M) / 2
  L_eq <- L1_eq + L2_eq + L3_eq + L4_eq
  K <- L_eq / (E_eq/(mu_L * d_E * L1_eq) - 4/(mu_L * d_L) - 1)

  # Initial state values (equilibrium solution):
  E <- E_eq
  L1 <- L1_eq
  L2 <- L2_eq
  L3 <- L3_eq
  L4 <- L4_eq
  L <- L_eq
  P <- P_eq
  M <- M_eq
  time <- 0

  numTimeSteps <- (numDays / timeStepSize) + 1

  for (i in 2:numTimeSteps) {
    E[i] <- (beta*timeStepSize) * M[i-1] + exp(-mu_E*timeStepSize) * (1-(1-exp(-timeStepSize/d_E))) * E[i-1]
    L1[i] <- exp(-mu_E*timeStepSize) * (1-exp(-timeStepSize/d_E)) * E[i-1] + exp(-mu_L*timeStepSize*(1+(L[i-1]/K))) * (1-(1-exp(-4*timeStepSize/d_L))) * L1[i-1]
    L2[i] <- exp(-mu_L*timeStepSize*(1+(L[i-1]/K))) * ((1-exp(-4*timeStepSize/d_L)) * L1[i-1] + (1-(1-exp(-4*timeStepSize/d_L))) * L2[i-1])
    L3[i] <- exp(-mu_L*timeStepSize*(1+(L[i-1]/K))) * ((1-exp(-4*timeStepSize/d_L)) * L2[i-1] + (1-(1-exp(-4*timeStepSize/d_L))) * L3[i-1])
    L4[i] <- exp(-mu_L*timeStepSize*(1+(L[i-1]/K))) * ((1-exp(-4*timeStepSize/d_L)) * L3[i-1] + (1-(1-exp(-4*timeStepSize/d_L))) * L4[i-1])
    P[i] <- exp(-mu_L*timeStepSize*(1+(L[i-1]/K))) * (1-exp(-4*timeStepSize/d_L)) * L4[i-1] + exp(-mu_P*timeStepSize) * (1-(1-exp(-timeStepSize/d_P))) * P[i-1]
    M[i] <- exp(-mu_P*timeStepSize) * (1-exp(-timeStepSize/(2*d_P))) * P[i-1] + exp(-mu_M*timeStepSize) * M[i-1]
    time[i] <- time[i-1] + timeStepSize
    L[i] <- L1[i] + L2[i] + L3[i] + L4[i]
  }

  ## Return the results as a data frame:
  results <- data.frame(E, L1, L2, L3, L4, L, P, M, time)
  return(results)
}

trajectory <- EL4P_diffEqs(parameters, numDays, timeStepSize)

## The first few entries of the trajectory matrix:
head(trajectory)

## Convert the trajectory matrix into a data frame:
trajectory_df <- data.frame(trajectory)
time <- trajectory_df$time

## Plot the number of people in each state over time using "ggplot()":
library(ggplot2)

ggplot(trajectory_df, aes(x=time, y=trajectory_df, color=State)) +
  geom_line(aes(y = E, col = "E"), size = 1.2) +
  geom_line(aes(y = L, col = "L"), size = 1.2) +
  geom_line(aes(y = P, col = "P"), size = 1.2) +
  geom_line(aes(y = M, col = "M"), size = 1.2) +
  labs(x = "Time (days)", y = "Number of individuals in each life stage")


#################################################
## EL4P EQUILIBRIUM SOLUTIONS:                 ##
#################################################

# given a lambda, we need to get K
EL4P_fitK <- function(lambda, beta = 32, mu_E = 0.168, mu_L = 0.168, mu_P = 0.168, mu_M = 0.123, d_E = 1, d_L = 14, d_P = 1){

  out = list()

  out$P_eq <- lambda / (2 * d_P)
  out$M_eq <- out$P_eq / (2 * d_P * mu_M)
  out$E_eq <- beta * out$M_eq / (mu_E + (1/d_E))
  out$L1_eq <- (beta^(3/4) * d_L * out$M_eq * (1 + d_P * mu_P)^(1/4) * mu_M^(1/4)) /
    (2^(7/4) * (1 + d_E * mu_E)^(3/4))
  out$L2_eq <- (beta^(2/4) * d_L * out$M_eq * (1 + d_P * mu_P)^(2/4) * mu_M^(2/4)) /
    (2^(6/4) * (1 + d_E * mu_E)^(2/4))
  out$L3_eq <- (beta^(1/4) * d_L * out$M_eq * (1 + d_P * mu_P)^(3/4) * mu_M^(3/4)) /
    (2^(5/4) * (1 + d_E * mu_E)^(1/4))
  out$L4_eq <- (d_L * out$M_eq * (1 + d_P * mu_P) * mu_M) / 2
  out$L_eq <- out$L1_eq + out$L2_eq + out$L3_eq + out$L4_eq
  out$K <- out$L_eq / (out$E_eq/(mu_L * d_E * out$L1_eq) - 4/(mu_L * d_L) - 1)

  return(out)
}
