###############################################################################
#
#      ___   ____  __  _____
#     /   | / __ \/ / / /   |
#    / /| |/ / / / / / / /| |
#   / ___ / /_/ / /_/ / ___ |
#  /_/  |_\___\_\____/_/  |_|
#
#   MASH-MICRO
#   AQUATIC ECOLOGY: EL4P Methods
#   MASH-MICRO Team
#   September 13, 2017
#
###############################################################################


###############################################################################
# Difference Equations & Specialized Methods
###############################################################################

#' EL4P Daily Difference Equations
#'
#' Run daily EL4P difference equations.
#'  * This method is bound to \code{EL4P$oneDay_EL4P}
#'
oneDay_EL4P <- function(){

  private$L     = sum(private$L1,private$L2,private$L3,private$L4)

  private$E_lag     = private$E
  private$L1_lag    = private$L1
  private$L2_lag    = private$L2
  private$L3_lag    = private$L3
  private$L4_lag    = private$L4
  private$P_lag     = private$P

  private$E         = exp(-private$mu_E*private$deltaT) * (1-(1-exp(-private$deltaT/private$d_E))) * E_lag
  private$L1        = exp(-private$mu_E*private$deltaT) * (1-exp(-private$deltaT/private$d_E)) * E_lag + exp(-private$mu_L*private$deltaT*(1+(L/private$K))) * (1-(1-exp(-4*private$deltaT/private$d_L))) * L1_lag
  private$L2        = exp(-private$mu_L*private$deltaT*(1+(L/private$K))) * ((1-exp(-4*private$deltaT/private$d_L)) * L1_lag + (1-(1-exp(-4*private$deltaT/private$d_L))) * L2_lag)
  private$L3        = exp(-private$mu_L*private$deltaT*(1+(L/private$K))) * ((1-exp(-4*private$deltaT/private$d_L)) * L2_lag + (1-(1-exp(-4*private$deltaT/private$d_L))) * L3_lag)
  private$L4        = exp(-private$mu_L*private$deltaT*(1+(L/private$K))) * ((1-exp(-4*private$deltaT/private$d_L)) * L3_lag + (1-(1-exp(-4*private$deltaT/private$d_L))) * L4_lag)
  private$P         = exp(-private$mu_L*private$deltaT*(1+(L/private$K))) * (1-exp(-4*private$deltaT/private$d_L)) * L4_lag + exp(-private$mu_P*private$deltaT) * (1-(1-exp(-private$deltaT/private$d_P))) * P_lag
  private$lambda    = exp(-private$mu_P*private$deltaT) * (1-exp(-private$deltaT/(2*private$d_P))) * P_lag

}

EL4P$set(which = "public",name = "oneDay_EL4P",
          value = oneDay_EL4P, overwrite = TRUE
)

# L1o = pop$L1; L2o=pop$L2; L3o=pop$L3; L4o=pop$L4
# D   = sum(L1o+L2o+L3o+L4o)
# s1  = exp(-alpha[ix])
# s2  = exp(-(alpha[ix]+ psi*D))
# pop$lambda = s1*pop$P
# pop$P  = s2*p*L4o
# pop$L4 = s2*(p*L3o + (1-p)*L4o)
# pop$L3 = s2*(p*L2o + (1-p)*L3o)
# pop$L2 = s2*(p*L1o + (1-p)*L2o)
# pop$L1 = pop$eggs + s2*(1-p)*L1o
# pop$eggs = M[ix]*aquaEq[ix]*(G/lifespan)
# return(pop)

# #' EL4P Run to Equilibrium
# #'
# #' Run a single aquatic population's dynamics using simulated adult dynamics and egg laying from Ross-MacDonald parameters.
# #' Adult dynamics follow the simple equation \eqn{( e^{-1/lifespan} * M) + \lambda}
# #'
# #'  * This method is bound to \code{EL4P$run2Eq}
# #'
# #' @param M equilibrium adult female density (from \code{\link{EL4P_fitK}})
# #' @param beta daily rate of oviposition of eggs (units in 1/day * 1/female mosquito)
# #' @param mu_M adult female daily mortality rate
# #' @param tMax maximum number of time steps
# #' @param tol target minimum variance in lambda
# #'
# #'
# run2Eq_EL4P <- function(M, beta, mu_M, tMax = 800, tol = 0.1){
#
#   # burn in adult population
#   for(i in 1:100){
#     M = (exp(-mu_M)*M) + private$lambda
#   }
#
#   #
#
# }
#
checkVar_EL4P <- function(M, beta, mu_M, tMax = 800, tol = 0.1){

  M = private$lambda # init adult population

  lambdaH = vector(mode="numeric",length=tMax+1)
  lambdaH[1] = private$lambda

  for(i in 1:tMax){ # run full simulation
    pop = oneDay_GEL4P(ix = ix, psi = psi, pop = pop, PAR = PAR)
    PAR$M[ix] = ((exp(-1/PAR$lifespan))*PAR$M[ix]) + pop$lambda # simulate adult population dynamics
    lambdaH[i+1] = pop$lambda
  }
  return(lambdaH)

}


#' EL4P Add Egg Batch
#'
#' Add an egg batch to EL4P
#'  * This method is bound to \code{EL4P$addBatch_EL4P}
#'
#' @param batch numeric vector, size of vector must equal the number of genotypes in the EL4P population
#'
addBatch_EL4P <- function(batch){
  if(length(batch) != private$N_genotypes){
    stop(cat("number of elements in egg batch: ",batch," must be equal to number of genotypes in EL4P\n",sep=""))
  }
  private$E = private$E + batch
}

EL4P$set(which = "public",name = "addBatch_EL4P",
          value = addBatch_EL4P, overwrite = TRUE
)


###############################################################################
# Getters & Setters
###############################################################################

# get EL4P from outside AquaticSite

get_EL4P_AquaticSite <- function(){
  return(private$EL4P)
}

# get all

#' EL4P: Get All Life Stages
#'
#' Return all life stages
#'  * This method is bound to \code{EL4P$get_all}
#'
#' @param lag logical; return lag stages as well?
#'
get_all_EL4P <- function(lag = FALSE){
  if(lag){
    return(
      list(
        E = private$E,
        E_lag = private$E_lag,
        L1 = private$L1,
        L1_lag = private$L1_lag,
        L2 = private$L2,
        L2_lag = private$L2_lag,
        L3 = private$L3,
        L3_lag = private$L3_lag,
        L4 = private$L4,
        L4_lag = private$L4_lag,
        P = private$P,
        P_lag = private$P_lag,
        lambda = private$lambda
      )
    )
  } else {
    return(
      list(
        E = private$E,
        L1 = private$L1,
        L2 = private$L2,
        L3 = private$L3,
        L4 = private$L4,
        P = private$P,
        lambda = private$lambda
      )
    )
  }
}

EL4P$set(which = "public",name = "get_all",
          value = get_all_EL4P, overwrite = TRUE
)


# lambda

#' EL4P: Get Lambda
#'
#' Return lambda
#'  * This method is bound to \code{EL4P$get_lambda}
#'
get_lambda_EL4P <- function(){
  return(private$lambda)
}

EL4P$set(which = "public",name = "get_lambda",
          value = get_lambda_EL4P, overwrite = TRUE
)

#' EL4P: Set Lambda
#'
#' Set lambda
#'  * This method is bound to \code{EL4P$set_lambda}
#'
#' @param lambda numeric
#'
set_lambda_EL4P <- function(lambda){
  private$lambda = lambda
}

EL4P$set(which = "public",name = "set_lambda",
          value = set_lambda_EL4P, overwrite = TRUE
)

#' EL4P: Clear Lambda
#'
#' Clear lambda
#'  * This method is bound to \code{EL4P$clear_lambda}
#'
clear_lambda_EL4P <- function(){
  private$lambda = private$lambda*0
}

EL4P$set(which = "public",name = "clear_lambda",
          value = clear_lambda_EL4P, overwrite = TRUE
)

# E

#' EL4P: Get E
#'
#' Return E
#'  * This method is bound to \code{EL4P$get_E}
#'
get_E_EL4P <- function(){
  return(private$E)
}

EL4P$set(which = "public",name = "get_E",
          value = get_E_EL4P, overwrite = TRUE
)

#' EL4P: Set E
#'
#' Set E
#'  * This method is bound to \code{EL4P$set_E}
#'
#' @param E numeric
#'
set_E_EL4P <- function(E){
  private$E = E
}

EL4P$set(which = "public",name = "set_E",
          value = set_E_EL4P, overwrite = TRUE
)

# L1

#' EL4P: Get L1
#'
#' Return L1
#'  * This method is bound to \code{EL4P$get_L1}
#'
get_L1_EL4P <- function(){
  return(private$L1)
}

EL4P$set(which = "public",name = "get_L1",
          value = get_L1_EL4P, overwrite = TRUE
)

#' EL4P: Set L1
#'
#' Set L1
#'  * This method is bound to \code{EL4P$set_L1}
#'
#' @param L1 numeric
#'
set_L1_EL4P <- function(L1){
  private$L1 = L1
}

EL4P$set(which = "public",name = "set_L1",
          value = set_L1_EL4P, overwrite = TRUE
)


# L2

#' EL4P: Get L2
#'
#' Return L2
#'  * This method is bound to \code{EL4P$get_L2}
#'
get_L2_EL4P <- function(){
  return(private$L2)
}

EL4P$set(which = "public",name = "get_L2",
          value = get_L2_EL4P, overwrite = TRUE
)

#' EL4P: Set L2
#'
#' Set L2
#'  * This method is bound to \code{EL4P$set_L2}
#'
#' @param L2 numeric
#'
set_L2_EL4P <- function(L2){
  private$L2 = L2
}

EL4P$set(which = "public",name = "set_L2",
          value = set_L2_EL4P, overwrite = TRUE
)

# L3

#' EL4P: Get L3
#'
#' Return L3
#'  * This method is bound to \code{EL4P$get_L3}
#'
get_L3_EL4P <- function(){
  return(private$L3)
}

EL4P$set(which = "public",name = "get_L3",
          value = get_L3_EL4P, overwrite = TRUE
)

#' EL4P: Set L3
#'
#' Set L3
#'  * This method is bound to \code{EL4P$set_L3}
#'
#' @param L3 numeric
#'
set_L3_EL4P <- function(L3){
  private$L3 = L3
}

EL4P$set(which = "public",name = "set_L3",
          value = set_L3_EL4P, overwrite = TRUE
)

# L4

#' EL4P: Get L4
#'
#' Return L4
#'  * This method is bound to \code{EL4P$get_L4}
#'
get_L4_EL4P <- function(){
  return(private$L4)
}

EL4P$set(which = "public",name = "get_L4",
          value = get_L4_EL4P, overwrite = TRUE
)

#' EL4P: Set L4
#'
#' Set L4
#'  * This method is bound to \code{EL4P$set_L4}
#'
#' @param L4 numeric
#'
set_L4_EL4P <- function(L4){
  private$L4 = L4
}

EL4P$set(which = "public",name = "set_L4",
          value = set_L4_EL4P, overwrite = TRUE
)

# P

#' EL4P: Get P
#'
#' Return P
#'  * This method is bound to \code{EL4P$get_P}
#'
get_P_EL4P <- function(){
  return(private$P)
}

EL4P$set(which = "public",name = "get_P",
          value = get_P_EL4P, overwrite = TRUE
)

#' EL4P: Set P
#'
#' Set P
#'  * This method is bound to \code{EL4P$set_P}
#'
#' @param P numeric
#'
set_P_EL4P <- function(P){
  private$P = P
}

EL4P$set(which = "public",name = "set_P",
          value = set_P_EL4P, overwrite = TRUE
)

# Parameters

#' EL4P: Get K
#'
#' Return K
#'  * This method is bound to \code{EL4P$get_K}
#'
get_K_EL4P <- function(){
  return(private$P)
}

EL4P$set(which = "public",name = "get_K",
          value = get_K_EL4P, overwrite = TRUE
)

#' EL4P: Set K
#'
#' Set K
#'  * This method is bound to \code{EL4P$set_K}
#'
#' @param K numeric
#'
set_K_EL4P <- function(K){
  private$K = K
}

EL4P$set(which = "public",name = "set_K",
          value = set_K_EL4P, overwrite = TRUE
)

#' EL4P: Get d_E
#'
#' Return d_E
#'  * This method is bound to \code{EL4P$get_d_E}
#'
get_d_E_EL4P <- function(){
  return(private$d_E)
}

EL4P$set(which = "public",name = "get_d_E",
          value = get_d_E_EL4P, overwrite = TRUE
)

#' EL4P: Set d_E
#'
#' Set d_E
#'  * This method is bound to \code{EL4P$set_d_E}
#'
#' @param d_E numeric
#'
set_d_E_EL4P <- function(d_E){
  private$d_E = d_E
}

EL4P$set(which = "public",name = "set_d_E",
          value = set_d_E_EL4P, overwrite = TRUE
)

#' EL4P: Get d_L
#'
#' Return d_L
#'  * This method is bound to \code{EL4P$get_d_L}
#'
get_d_L_EL4P <- function(){
  return(private$d_L)
}

EL4P$set(which = "public",name = "get_d_L",
          value = get_d_L_EL4P, overwrite = TRUE
)

#' EL4P: Set d_L
#'
#' Set d_L
#'  * This method is bound to \code{EL4P$set_d_L}
#'
#' @param d_L numeric
#'
set_d_L_EL4P <- function(d_L){
  private$d_L = d_L
}

EL4P$set(which = "public",name = "set_d_L",
          value = set_d_L_EL4P, overwrite = TRUE
)

#' EL4P: Get d_P
#'
#' Return d_P
#'  * This method is bound to \code{EL4P$get_d_P}
#'
get_d_P_EL4P <- function(){
  return(private$d_P)
}

EL4P$set(which = "public",name = "get_d_P",
          value = get_d_P_EL4P, overwrite = TRUE
)

#' EL4P: Set d_P
#'
#' Set d_P
#'  * This method is bound to \code{EL4P$set_d_P}
#'
#' @param d_P numeric
#'
set_d_P_EL4P <- function(d_P){
  private$d_P = d_P
}

EL4P$set(which = "public",name = "set_d_P",
          value = set_d_P_EL4P, overwrite = TRUE
)

#' EL4P: Get mu_E
#'
#' Return mu_E
#'  * This method is bound to \code{EL4P$get_mu_E}
#'
get_mu_E_EL4P <- function(){
  return(private$mu_E)
}

EL4P$set(which = "public",name = "get_mu_E",
          value = get_mu_E_EL4P, overwrite = TRUE
)

#' EL4P: Set mu_E
#'
#' Set mu_E
#'  * This method is bound to \code{EL4P$set_mu_E}
#'
#' @param mu_E numeric
#'
set_mu_E_EL4P <- function(mu_E){
  private$mu_E = mu_E
}

EL4P$set(which = "public",name = "set_mu_E",
          value = set_mu_E_EL4P, overwrite = TRUE
)

#' EL4P: Get mu_L
#'
#' Return mu_L
#'  * This method is bound to \code{EL4P$get_mu_L}
#'
get_mu_L_EL4P <- function(){
  return(private$mu_L)
}

EL4P$set(which = "public",name = "get_mu_L",
          value = get_mu_L_EL4P, overwrite = TRUE
)

#' EL4P: Set mu_L
#'
#' Set mu_L
#'  * This method is bound to \code{EL4P$set_mu_L}
#'
#' @param mu_L numeric
#'
set_mu_L_EL4P <- function(mu_L){
  private$mu_L = mu_L
}

EL4P$set(which = "public",name = "set_mu_L",
          value = set_mu_L_EL4P, overwrite = TRUE
)

#' EL4P: Get mu_P
#'
#' Return mu_P
#'  * This method is bound to \code{EL4P$get_mu_P}
#'
get_mu_P_EL4P <- function(){
  return(private$mu_P)
}

EL4P$set(which = "public",name = "get_mu_P",
          value = get_mu_P_EL4P, overwrite = TRUE
)

#' EL4P: Set mu_P
#'
#' Set mu_P
#'  * This method is bound to \code{EL4P$set_mu_P}
#'
#' @param mu_P numeric
#'
set_mu_P_EL4P <- function(mu_P){
  private$mu_P = mu_P
}

EL4P$set(which = "public",name = "set_mu_P",
          value = set_mu_P_EL4P, overwrite = TRUE
)
