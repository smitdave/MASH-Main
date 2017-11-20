###############################################################################
#
#       ___   ____  __  _____
#     /   | / __ \/ / / /   |
#    / /| |/ / / / / / / /| |
#   / ___ / /_/ / /_/ / ___ |
#  /_/  |_\___\_\____/_/  |_|
#
#   MASH-MACRO
#   AQUATIC ECOLOGY: ELP Methods
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   August 21, 2017
#
###############################################################################

###############################################################################
#   Aquatic Dynamics
###############################################################################

# this is a stand-in for a generic
addCohort_ELP_MacroPatch <- function(){
  lambda = private$ELP$oneDay_Emergence()
  private$ImagoQ$add_ImagoQ(N_new = lambda,tEmerge_new = private$tNow,-1,"-1","-1")
}

# this is a stand-in for a generic
aquaticDynamics_ELP_MacroPatch <- function(){
  private$ELP$oneDay_aquaticDynamics()
}



###############################################################################
#   Getters & Setters
###############################################################################

#' Aquatic Eceology ELP: Getter for \code{\link{ELP}} in \code{\link{MacroPatch}}
#'
#' Return \code{\link{ELP}}
#'  * This method is bound to \code{MacroPatch$get_ELP}
#'
get_ELP_MacroPatch <- function(){
  return(private$ELP)
}

#' Aquatic Eceology ELP: Setter for \code{\link{ELP}} in \code{\link{MacroPatch}}
#'
#' Set \code{\link{ELP}}
#'  * This method is bound to \code{MacroPatch$set_ELP}
#'
#' @param ELP new object of class \code{\link{ELP}}
set_ELP_MacroPatch <- function(ELP){
  private$ELP = NULL
  private$ELP = ELP
}

####WRITE ELP STUFF FOR A SINGLE PATCH, AFTER THAT STRESS TEST MODEL ON A SINGLE PATCH###
