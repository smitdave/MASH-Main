###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES: chooseMate
#   MASH-MICRO Team
#   September 18, 2017
#
###############################################################################

#' MBITES-Generic: Mate Choosing for \code{\link{MosquitoFemale}}
#'
#' Choose a \code{\link{MosquitoMale}}
#'  * This method is bound to \code{MosquitoFemale$chooseMate()}.
#'
mbitesGeneric_chooseMate <- function(){

  if(private$inPointSet != "m"){
    stop(cat("mosquito is calling chooseMate from outside a mating site: ",private$ix," ",private$inPointSet,"\n",sep=""))
  }

  if(private$LandscapePointer$get_MatingSites(private$ix)$get_MatingQ()$get_N() == 0){
    private$lspot = "l"
  } else {
    mates = private$LandscapePointer$get_MatingSites(private$ix)$get_MatingQ()$get_MatingQ()
    private$mate = sampleIx_utility(x = mates$maleID, size = 1, prob = mates$mateFitness)
  }

}
