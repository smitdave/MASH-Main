###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES-BRO: Energetics
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################


#################################################################
# Sugar Energetics
#################################################################

# MBITES-BRO does not implement sugar energetics.


#################################################################
# Blood Energetics
#################################################################

#' MBITES-BRO: Bloodmeal Energetics for \code{\link{MosquitoFemale}}
#'
#' Handle bloodmeal size (see \code{\link{mbitesGeneric_rBloodMealSize}}), overfeeding (see \code{\link{mbitesGeneric_pOverFeed}}),
#' and egg batch size and maturation time.
#'  * This method is bound to \code{MosquitoFemale$BloodMeal()}.
#' @md
mbitesBRO_BloodMeal <- function(){
  private$bmSize = self$rBloodMealSize()

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
