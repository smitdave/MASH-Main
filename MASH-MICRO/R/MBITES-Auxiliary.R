###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES-Auxiliary
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################

##############################################################
# Sample
##############################################################

#' Sample Indices
#'
#' Wrapper for \code{\link{sample}} that will not lead to unexpected behavior when \code{x} is of length 1.
#'  * This function is used in \code{\link{mbitesGeneric_chooseHost}}
#'
#' @export
sampleIx_utility <- function(x, ...){
  return(
    x[sample.int(length(x), ...)]
  )
}


##############################################################
# Checks of Life Status
##############################################################

#' MBITES-Generic: Alive Check for \code{\link{MosquitoFemale}} and \code{\link{MosquitoMale}}
#'
#' Check if this mosquito is alive and return a logical value.
#'  * This method is bound to \code{MosquitoFemale$isAlive()} or \code{MosquitoMale$isAlive()}.
#' @md
mbitesGeneric_isAlive <- function(){
  if(private$stateNew == "D" || private$state == "D"){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' MBITES-Generic: Active Check for \code{\link{MosquitoFemale}} and \code{\link{MosquitoMale}}
#'
#' Check if this mosquito is active and return a logical value.
#'  * This method is bound to \code{MosquitoFemale$isActive()} or \code{MosquitoMale$isActive()}.
#' @md
mbitesGeneric_isActive <- function(){
  if(private$state == "E"){
    return(FALSE)
  } else {
    return(self$isAlive())
  }
}
