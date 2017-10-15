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

  private$E         = exp(-private$muE*private$deltaT) * (1-(1-exp(-private$deltaT/private$dE))) * E_lag
  private$L1        = exp(-private$muE*private$deltaT) * (1-exp(-private$deltaT/private$dE)) * E_lag + exp(-private$muL*private$deltaT*(1+(L/private$K))) * (1-(1-exp(-4*private$deltaT/private$dL))) * L1_lag
  private$L2        = exp(-private$muL*private$deltaT*(1+(L/private$K))) * ((1-exp(-4*private$deltaT/private$dL)) * L1_lag + (1-(1-exp(-4*private$deltaT/private$dL))) * L2_lag)
  private$L3        = exp(-private$muL*private$deltaT*(1+(L/private$K))) * ((1-exp(-4*private$deltaT/private$dL)) * L2_lag + (1-(1-exp(-4*private$deltaT/private$dL))) * L3_lag)
  private$L4        = exp(-private$muL*private$deltaT*(1+(L/private$K))) * ((1-exp(-4*private$deltaT/private$dL)) * L3_lag + (1-(1-exp(-4*private$deltaT/private$dL))) * L4_lag)
  private$P         = exp(-private$muL*private$deltaT*(1+(L/private$K))) * (1-exp(-4*private$deltaT/private$dL)) * L4_lag + exp(-private$muP*private$deltaT) * (1-(1-exp(-private$deltaT/private$dP))) * P_lag
  private$lambda    = exp(-private$muP*private$deltaT) * (1-exp(-private$deltaT/(2*private$dP))) * P_lag

}

EL4P$set(which = "public",name = "oneDay_EL4P",
          value = oneDay_EL4P, overwrite = TRUE
)


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

#' EL4P: Get dE
#'
#' Return dE
#'  * This method is bound to \code{EL4P$get_dE}
#'
get_dE_EL4P <- function(){
  return(private$dE)
}

EL4P$set(which = "public",name = "get_dE",
          value = get_dE_EL4P, overwrite = TRUE
)

#' EL4P: Set dE
#'
#' Set dE
#'  * This method is bound to \code{EL4P$set_dE}
#'
#' @param dE numeric
#'
set_dE_EL4P <- function(dE){
  private$dE = dE
}

EL4P$set(which = "public",name = "set_dE",
          value = set_dE_EL4P, overwrite = TRUE
)

#' EL4P: Get dL
#'
#' Return dL
#'  * This method is bound to \code{EL4P$get_dL}
#'
get_dL_EL4P <- function(){
  return(private$dL)
}

EL4P$set(which = "public",name = "get_dL",
          value = get_dL_EL4P, overwrite = TRUE
)

#' EL4P: Set dL
#'
#' Set dL
#'  * This method is bound to \code{EL4P$set_dL}
#'
#' @param dL numeric
#'
set_dL_EL4P <- function(dL){
  private$dL = dL
}

EL4P$set(which = "public",name = "set_dL",
          value = set_dL_EL4P, overwrite = TRUE
)

#' EL4P: Get dP
#'
#' Return dP
#'  * This method is bound to \code{EL4P$get_dP}
#'
get_dP_EL4P <- function(){
  return(private$dP)
}

EL4P$set(which = "public",name = "get_dP",
          value = get_dP_EL4P, overwrite = TRUE
)

#' EL4P: Set dP
#'
#' Set dP
#'  * This method is bound to \code{EL4P$set_dP}
#'
#' @param dP numeric
#'
set_dP_EL4P <- function(dP){
  private$dP = dP
}

EL4P$set(which = "public",name = "set_dP",
          value = set_dP_EL4P, overwrite = TRUE
)

#' EL4P: Get muE
#'
#' Return muE
#'  * This method is bound to \code{EL4P$get_muE}
#'
get_muE_EL4P <- function(){
  return(private$muE)
}

EL4P$set(which = "public",name = "get_muE",
          value = get_muE_EL4P, overwrite = TRUE
)

#' EL4P: Set muE
#'
#' Set muE
#'  * This method is bound to \code{EL4P$set_muE}
#'
#' @param muE numeric
#'
set_muE_EL4P <- function(muE){
  private$muE = muE
}

EL4P$set(which = "public",name = "set_muE",
          value = set_muE_EL4P, overwrite = TRUE
)

#' EL4P: Get muL
#'
#' Return muL
#'  * This method is bound to \code{EL4P$get_muL}
#'
get_muL_EL4P <- function(){
  return(private$muL)
}

EL4P$set(which = "public",name = "get_muL",
          value = get_muL_EL4P, overwrite = TRUE
)

#' EL4P: Set muL
#'
#' Set muL
#'  * This method is bound to \code{EL4P$set_muL}
#'
#' @param muL numeric
#'
set_muL_EL4P <- function(muL){
  private$muL = muL
}

EL4P$set(which = "public",name = "set_muL",
          value = set_muL_EL4P, overwrite = TRUE
)

#' EL4P: Get muP
#'
#' Return muP
#'  * This method is bound to \code{EL4P$get_muP}
#'
get_muP_EL4P <- function(){
  return(private$muP)
}

EL4P$set(which = "public",name = "get_muP",
          value = get_muP_EL4P, overwrite = TRUE
)

#' EL4P: Set muP
#'
#' Set muP
#'  * This method is bound to \code{EL4P$set_muP}
#'
#' @param muP numeric
#'
set_muP_EL4P <- function(muP){
  private$muP = muP
}

EL4P$set(which = "public",name = "set_muP",
          value = set_muP_EL4P, overwrite = TRUE
)
