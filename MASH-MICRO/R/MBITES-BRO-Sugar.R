###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES-BROS (BRO+Sugar): Bouts
#   MASH-MICRO Team
#   September 18, 2017
#
###############################################################################

#' MBITES-BROS: Sugar Feeding Bout \code{MosquitoFemale}
#'
#' A mosquito performs a sugar feeding bout (all actions taken launch to launch when resting required).
#' Upon entering the sugar feeding behavioral state prompted by \code{\link{mbitesGeneric_queueSugarBout}}, the mosquito will move to a \code{\link{SugarSite}} and
#' sugar feed .
#'  * This method is bound to \code{MosquitoFemale$boutS()}.
#'
mbitesBROS_boutS <- function(){

  if(self$isAlive()){
    if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("S_succeed")){
      private$energy = 1

      if(!private$mature){
        private$energyPreG = private$energyPreG - private$FemalePopPointer$get_MBITES_PAR("preGsugar")
        if(private$energyPreG <= 0){
          private$mature = TRUE
        }
      }

    } else {
      private$stateNew = "R"
    }
  }

}
