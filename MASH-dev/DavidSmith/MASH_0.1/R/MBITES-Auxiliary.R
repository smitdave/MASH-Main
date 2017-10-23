#################################################################
#
#   MASH
#   R6-ified
#   MBITES General Method Definitions
#   David Smith, Hector Sanchez, Sean Wu
#   May 2, 2017
#
#################################################################


##############################################################
# Checks of Life Status
##############################################################

#' MBITES-Generic: Alive Check for \code{\link{MicroMosquitoFemale}} and \code{\link{MicroMosquitoMale}}
#'
#' Check if this mosquito is alive and return a logical value.
#'  * This method is bound to \code{MicroMosquitoFemale$isAlive()} or \code{MicroMosquitoMale$isAlive()}.
#' @md
mbitesGeneric_isAlive <- function(){
  if(private$stateNew == "D" || private$state == "D"){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' MBITES-Generic: Active Check for \code{\link{MicroMosquitoFemale}} and \code{\link{MicroMosquitoMale}}
#'
#' Check if this mosquito is active and return a logical value.
#'  * This method is bound to \code{MicroMosquitoFemale$isActive()} or \code{MicroMosquitoMale$isActive()}.
#' @md
mbitesGeneric_isActive <- function(){
  if(private$state == "E"){
    return(FALSE)
  } else {
    return(self$isAlive())
  }
}
