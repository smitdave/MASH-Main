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

                      initialize = function(K, dE, dL, dP, muE, muL, muP, N_genotypes = 1, deltaT = 0.01){

                        if(deltaT > 1){
                          stop("time step deltaT cannot currently be greater than 1 day; time steps greater than 0.1 may be numerically unstable")
                        }

                        private$N_genotypes = N_genotypes

                        private$K = K
                        private$dE = dE
                        private$dL = dL
                        private$dP = dP
                        private$muE = muE
                        private$muL = muL
                        private$muP = muP

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

                        private$L = 0

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
                      dE = numeric(1),
                      dL = numeric(1),
                      dP = numeric(1),
                      muE = numeric(1),
                      muL = numeric(1),
                      muP = numeric(1),

                      L = numeric(1)

                    )
)
