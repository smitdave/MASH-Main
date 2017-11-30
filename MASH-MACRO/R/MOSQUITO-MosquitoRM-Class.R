###############################################################################
#       __  _______  _____ ____  __  ________________
#      /  |/  / __ \/ ___// __ \/ / / /  _/_  __/ __ \
#     / /|_/ / / / /\__ \/ / / / / / // /  / / / / / /
#    / /  / / /_/ /___/ / /_/ / /_/ // /  / / / /_/ /
#   /_/  /_/\____//____/\___\_\____/___/ /_/  \____/
#
#   MASH-MACRO
#   Mosquito_RM Class Definition
#   MASH Team
#   November 2017
#
###############################################################################

#' MosquitoRM Class Definition
#'
#' Generate a mosquito population living in a \code{\link{MacroTile}}, following Ross-MacDonald stype daily difference equations
#' and simple diffusion based movement given a transition matrix.
#' This class inherits form \code{\link{Mosquito_Base}}, this documentation only records
#' overridden or new methods & fields, please see the base class documentation for more details.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * M: numeric vector of adult female mosquito density of length equal to number of patches
#'  * EIP: numeric vector of daily EIP of length 365
#'  * p = 0.9: daily probability of survival
#'  * f = 0.3: daily feeding rate
#'  * Q = 0.9: human blood index
#'  * v = 20: egg batch size
#'  * psi = \code{NULL}: diffusion matrix, must have rows and columns equal to number of patches
#'
#' @section **Methods**:
#'  * oneDay_popDynamics: see \code{\link{oneDay_popDynamics_Mosquito_RM}}
#'  * oneDay_oviposition: see \code{\link{oneDay_oviposition_Mosquito_RM}}
#'  * get_emergingAdults: see \code{\link{get_emergingAdults_Mosquito_RM}}
#'
#' @section **Fields**:
#'  * i'm a field: write me
#'
#' @md
#' @export
Mosquito_RM <- R6::R6Class(classname="Mosquito_RM",
                     inherit = Mosquito_Base,
                     portable = TRUE,
                     cloneable = FALSE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #################################################
                       # Constructor
                       #################################################

                       initialize = function(M, EIP, p=0.9, f=0.3, Q=0.9, v=20, psi = NULL){

                         N = nrow(psi)
                         if(length(EIP)!=365){stop("Mosquito_RM must be initialized with length 365 EIP vector")}
                         if(length(M)!=N){stop("Mosquito_RM must be initialized with M vector equal to rows of psi matrix")}

                         private$p             = p
                         private$f             = f
                         private$Q             = Q
                         private$v             = v
                         private$EIP           = EIP
                         private$maxEIP        = max(EIP)+1L

                         private$M             = M
                         private$Y             = rep(0L, N) # infected (incubating)
                         private$Z             = rep(0L, N) # infectious
                         private$ZZ            = matrix(data=0L,nrow=private$maxEIP,ncol=N) # each row is the number that will be added to the infectious state on that day

                         private$psi           = psi
                         private$P             = p^c(1:private$maxEIP) # survival over EIP

                       }

                     ),

                     #private members
                     private = list(

                       # RM parameters
                       p              = NULL,
                       f              = NULL,
                       Q              = NULL,
                       v              = NULL,
                       EIP            = NULL,
                       maxEIP         = NULL,

                       # Life stages
                       M              = NULL, # mosquito density
                       Y              = NULL, # infected (incubating)
                       Z              = NULL, # infectious
                       ZZ             = NULL, # each row is the number that will be added to the infectious state on that day

                       # Survival & Dispersion
                       psi            = NULL, # rough diffusion matrix
                       P              = NULL

                     )

) #end class definition


###############################################################################
# Getters & Setters
###############################################################################

get_M_Mosquito_RM <- function(){
  return(private$M)
}

Mosquito_RM$set(which = "public",name = "get_M",
          value = get_M_Mosquito_RM, overwrite = TRUE
)

get_Y_Mosquito_RM <- function(){
  return(private$Y)
}

get_Z_Mosquito_RM <- function(){
  return(private$Z)
}

get_ZZ_Mosquito_RM <- function(){
  return(private$ZZ)
}
