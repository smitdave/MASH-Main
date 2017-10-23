#################################################################
#
#   MASH
#   MBITES-BRO
#   Sugar feeding, blood feeding, overfeed, refeed, egg batch
#   David Smith, Hector Sanchez, Sean Wu
#   July 30, 2017
#
#################################################################

#################################################################
# Sugar Energetics
#################################################################

# MBITES-BRO does not implement sugar energetics.


#################################################################
# Blood Energetics
#################################################################

#' MBITES-BRO: Bloodmeal Energetics for \code{\link{MicroMosquitoFemale}}
#'
#' Handle bloodmeal size (see \code{\link{mbitesGeneric_rBloodMealSize}}), overfeeding (see \code{\link{mbitesGeneric_pOverFeed}}),
#' and egg batch size and maturation time.
#'  * This method is bound to \code{MicroMosquitoFemale$BloodMeal()}.
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
