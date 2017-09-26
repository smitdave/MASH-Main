###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   M-BITES: Blood & Sugar Energetics
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################


#################################################################
# Sugar Energetics
#################################################################

# see MBITES-Generic-Energetics.R


#################################################################
# Blood Energetics
#################################################################

#' M-BITES: Bloodmeal Energetics for \code{\link{MosquitoFemale}}
#'
#' Handle bloodmeal size (see \code{\link{mbitesGeneric_rBloodMealSize}}), overfeeding (see \code{\link{mbitesGeneric_pOverFeed}}),
#' and egg batch size and maturation time. Note that refeeding behavior is calculated during resting bout.
#'  * This method is bound to \code{MosquitoFemale$BloodMeal()}.
#' @md
mbites_BloodMeal <- function(){
  private$bmSize = self$rBloodMealSize()

  if(!private$mature){
    private$energyPreG = private$energyPreG - private$FemalePopPointer$get_MBITES_PAR("preGblood")
    if(private$energyPreG <= 0){
      private$mature = TRUE
    }
  }

  # Overfeeding mortality
  if(private$FemalePopPointer$get_MBITES_PAR("OVERFEED")){
    if(runif(1) < self$pOverFeed()){
      private$stateNew = "D"
    }
  }

  # egg production
  private$batch = self$rBatchSize()
  private$eggT = private$tNow + self$rEggMaturationTime()
}
