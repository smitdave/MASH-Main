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
# Difference Equations
###############################################################################


#' EL4P Daily Difference Equations
#'
#' Run daily EL4P difference equations.
#'  * This method is bound to \code{EL4P$oneDay_EL4P}
#'
oneDay_EL4P <- function(){

  private$L_tot     = sum(private$L1,private$L2,private$L3,private$L4)

  private$E_lag     = private$E
  private$L1_lag    = private$L1
  private$L2_lag    = private$L2
  private$L3_lag    = private$L3
  private$L4_lag    = private$L4
  private$P_lag     = private$P

  private$E         = exp(-private$muEgg)*(1-(1-exp(-1/private$dEgg)))*private$E_lag
  private$L1        = exp(-private$muEgg)*(1-exp(-1/private$dEgg))*private$E_lag + exp(-private$muLarva*(1+(L/private$K)))*(1-(1-exp(-4/private$dLarva)))*private$L1_lag
  private$L2        = exp(-private$muLarva*(1+(L/private$K)))*((1-exp(-4/private$dLarva))*private$L1_lag + (1-(1-exp(-4/private$dLarva)))*private$L2_lag)
  private$L3        = exp(-private$muLarva*(1+(L/private$K)))*((1-exp(-4/private$dLarva))*private$L2_lag + (1-(1-exp(-4/private$dLarva)))*private$L3_lag)
  private$L4        = exp(-private$muLarva*(1+(L/private$K)))*((1-exp(-4/private$dLarva))*private$L3_lag + (1-(1-exp(-4/private$dLarva)))*private$L4_lag)
  private$P         = exp(-private$muLarva*(1+(L/private$K)))*(1-exp(-4/private$dLarva))*private$L4_lag + exp(-private$muPupae)*(1-(1-exp(-1/private$dPupae)))*private$P_lag
  private$lambda    = exp(-private$muPupae)*(1-exp(-1/(2*private$dPupae)))*private$P_lag

}

EL4P$set(which = "public",name = "oneDay_EL4P",
          value = oneDay_EL4P, overwrite = TRUE
)


###############################################################################
# Getters & Setters
###############################################################################

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

#' EL4P: Get dEgg
#'
#' Return dEgg
#'  * This method is bound to \code{EL4P$get_dEgg}
#'
get_dEgg_EL4P <- function(){
  return(private$dEgg)
}

EL4P$set(which = "public",name = "get_dEgg",
          value = get_dEgg_EL4P, overwrite = TRUE
)

#' EL4P: Set dEgg
#'
#' Set dEgg
#'  * This method is bound to \code{EL4P$set_dEgg}
#'
#' @param dEgg numeric
#'
set_dEgg_EL4P <- function(dEgg){
  private$dEgg = dEgg
}

EL4P$set(which = "public",name = "set_dEgg",
          value = set_dEgg_EL4P, overwrite = TRUE
)

#' EL4P: Get dLarva
#'
#' Return dLarva
#'  * This method is bound to \code{EL4P$get_dLarva}
#'
get_dLarva_EL4P <- function(){
  return(private$dLarva)
}

EL4P$set(which = "public",name = "get_dLarva",
          value = get_dLarva_EL4P, overwrite = TRUE
)

#' EL4P: Set dLarva
#'
#' Set dLarva
#'  * This method is bound to \code{EL4P$set_dLarva}
#'
#' @param dLarva numeric
#'
set_dLarva_EL4P <- function(dLarva){
  private$dLarva = dLarva
}

EL4P$set(which = "public",name = "set_dLarva",
          value = set_dLarva_EL4P, overwrite = TRUE
)

#' EL4P: Get dPupae
#'
#' Return dPupae
#'  * This method is bound to \code{EL4P$get_dPupae}
#'
get_dPupae_EL4P <- function(){
  return(private$dPupae)
}

EL4P$set(which = "public",name = "get_dPupae",
          value = get_dPupae_EL4P, overwrite = TRUE
)

#' EL4P: Set dPupae
#'
#' Set dPupae
#'  * This method is bound to \code{EL4P$set_dPupae}
#'
#' @param dPupae numeric
#'
set_dPupae_EL4P <- function(dPupae){
  private$dPupae = dPupae
}

EL4P$set(which = "public",name = "set_dPupae",
          value = set_dPupae_EL4P, overwrite = TRUE
)

#' EL4P: Get muEgg
#'
#' Return muEgg
#'  * This method is bound to \code{EL4P$get_muEgg}
#'
get_muEgg_EL4P <- function(){
  return(private$muEgg)
}

EL4P$set(which = "public",name = "get_muEgg",
          value = get_muEgg_EL4P, overwrite = TRUE
)

#' EL4P: Set muEgg
#'
#' Set muEgg
#'  * This method is bound to \code{EL4P$set_muEgg}
#'
#' @param muEgg numeric
#'
set_muEgg_EL4P <- function(muEgg){
  private$muEgg = muEgg
}

EL4P$set(which = "public",name = "set_muEgg",
          value = set_muEgg_EL4P, overwrite = TRUE
)

#' EL4P: Get muLarva
#'
#' Return muLarva
#'  * This method is bound to \code{EL4P$get_muLarva}
#'
get_muLarva_EL4P <- function(){
  return(private$muLarva)
}

EL4P$set(which = "public",name = "get_muLarva",
          value = get_muLarva_EL4P, overwrite = TRUE
)

#' EL4P: Set muLarva
#'
#' Set muLarva
#'  * This method is bound to \code{EL4P$set_muLarva}
#'
#' @param muLarva numeric
#'
set_muLarva_EL4P <- function(muLarva){
  private$muLarva = muLarva
}

EL4P$set(which = "public",name = "set_muLarva",
          value = set_muLarva_EL4P, overwrite = TRUE
)

#' EL4P: Get muPupae
#'
#' Return muPupae
#'  * This method is bound to \code{EL4P$get_muPupae}
#'
get_muPupae_EL4P <- function(){
  return(private$muPupae)
}

EL4P$set(which = "public",name = "get_muPupae",
          value = get_muPupae_EL4P, overwrite = TRUE
)

#' EL4P: Set muPupae
#'
#' Set muPupae
#'  * This method is bound to \code{EL4P$set_muPupae}
#'
#' @param muPupae numeric
#'
set_muPupae_EL4P <- function(muPupae){
  private$muPupae = muPupae
}

EL4P$set(which = "public",name = "set_muPupae",
          value = set_muPupae_EL4P, overwrite = TRUE
)
