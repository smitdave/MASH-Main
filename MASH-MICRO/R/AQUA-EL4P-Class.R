###############################################################################
#
#      ___   ____  __  _____
#     /   | / __ \/ / / /   |
#    / /| |/ / / / / / / /| |
#   / ___ / /_/ / /_/ / ___ |
#  /_/  |_\___\_\____/_/  |_|
#
#   MASH-MICRO
#   AQUATIC ECOLOGY: EL4P Class
#   MASH-MICRO Team
#   September 13, 2017
#
###############################################################################


#' EL4P Class Definition
#'
#' im a class!
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: im an agument!
#'
#' @section **Methods**:
#'  * method: im a method!
#'
#' @section **Fields**:
#'  * field: im a field!
#'
#' @export
EL4P <- R6::R6Class(classname = "EL4P",
                    portable = TRUE,
                    cloneable = FALSE,
                    lock_class = FALSE,
                    lock_objects = FALSE,

                    # public members
                    public = list(

                      #################################################
                      # Constructor
                      #################################################

                      initialize = function(K, dEgg, dLarva, dPupae, muEgg, muLarva, muPupae, N_genotypes = 1){

                        private$N_genotypes = N_genotypes

                        private$K = K
                        private$dEgg = dEgg
                        private$dLarva = dLarva
                        private$dPupae = dPupae
                        private$muEgg = muEgg
                        private$muLarva = muLarva
                        private$muPupae = muPupae

                        private$E = rep(0,times=N_genotypes)
                        private$L1 = rep(0,times=N_genotypes)
                        private$L2 = rep(0,times=N_genotypes)
                        private$L3 = rep(0,times=N_genotypes)
                        private$L4 = rep(0,times=N_genotypes)
                        private$P = rep(0,times=N_genotypes)
                        private$lambda = rep(0,times=N_genotypes)

                        private$E_lag = rep(0,times=N_genotypes)
                        private$L1_lag = rep(0,times=N_genotypes)
                        private$L2_lag = rep(0,times=N_genotypes)
                        private$L3_lag = rep(0,times=N_genotypes)
                        private$L4_lag = rep(0,times=N_genotypes)
                        private$P_lag = rep(0,times=N_genotypes)

                        private$L_tot = 0

                      }

                    ),

                    # private members
                    private = list(

                      # fields
                      N_genotypes = integer(1),

                      E = NULL,
                      L1 = NULL,
                      L2 = NULL,
                      L3 = NULL,
                      L4 = NULL,
                      P = NULL,
                      lambda = NULL,

                      E_lag = NULL,
                      L1_lag = NULL,
                      L2_lag = NULL,
                      L3_lag = NULL,
                      L4_lag = NULL,
                      P_lag = NULL,

                      K = numeric(1),
                      dEgg = numeric(1),
                      dLarva = numeric(1),
                      dPupae = numeric(1),
                      muEgg = numeric(1),
                      muLarva = numeric(1),
                      muPupae = numeric(1),

                      L_tot = numeric(1)

                    )
)


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

# L3

# L4

# P
