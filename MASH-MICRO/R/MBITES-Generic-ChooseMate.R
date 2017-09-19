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
    # if no males present, leave the area
    private$lspot = "l"
  } else {
    # choose a mate based on male mating fitness
    mates = private$LandscapePointer$get_MatingSites(private$ix)$get_MatingQ()$get_MatingQ()
    ix = sampleIx_utility(x = 1:length(mates$maleID), size = 1, prob = mates$mateFitness)
    private$mateID = mates$maleID[ix]
    private$mateGenotype = mates$maleGenotype[ix]
  }

}
