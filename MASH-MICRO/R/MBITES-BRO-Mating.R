###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES-BROM (BRO+Mating): Bouts
#   MASH-MICRO Team
#   September 18, 2017
#
###############################################################################

#' MBITES-BROM: Mating Bout \code{MosquitoFemale}
#'
#' A mosquito performs mating bout (all actions taken launch to launch when resting required).
#' Upon entering the mating behavioral state, the mosquito will move to a \code{\link{MatingSite}} and
#' finds a mate in the \code{\link[MASHcpp]{MatingQ}}.
#'  * This method is bound to \code{MosquitoFemale$boutR()}.
#'
mbitesBROM_boutM <- function(){

  if(self$isAlive()){
    if(runif(1) < private$get_MBITES_PAR("M.s")){
      private$stateNew = "B"
      self$chooseMate()
    } else {
      private$stateNew = "D"
    }
  }

}
