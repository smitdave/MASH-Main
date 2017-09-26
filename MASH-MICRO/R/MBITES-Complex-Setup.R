###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   M-BITES: Setup
#   MASH-MICRO Team
#   September 2017
#
###############################################################################

#' M-BITES: Initialize Additional Methods & Fields in \code{\link{MosquitoPopFemale}} and \code{\link{MosquitoFemale}}
#'
#' Initialize M-BITES lifecycle model (complex form). This model includes:
#'  * Blood Feeding Search Bout: see \code{\link{mbites_boutF}}
#'  * Blood Feeding Attempt Bout: see \code{\link{mbites_boutB}}
#'  * Post-Prandial Resting Bout: see \code{\link{mbites_boutR}}
#'  * Egg Laying Search Bout: see \code{\link{mbites_boutL}}
#'  * Egg Laying Attempt Bout: see \code{\link{mbites_boutO}}
#'  * Sugar Feeding Attempt Bout: see \code{\link{mbites_boutS}}
#'  * Mating Bout: see \code{\link{mbites_boutM}}
#'
#' @param batchSize character switch that should be one of \code{"bms","norm"} for egg batch sizes dependent on bloodmeal size or normally distributed
#' @param eggMatT character switch that should be one of \code{"off","norm"} for egg batch maturation time turned off or normally distributed
#'
#'
#'
#' @return writes methods to \code{\link{MosquitoFemale}} and \code{\link{MosquitoMale}} classes.
#' @export
MBITES.Complex.Setup <- function(
  overwrite = TRUE,
  aquaModule = "emerge",
  timing = "exponential"
  ){

  # alert user
  cat("initializing M-BITES complex module\n",sep="")

  ##############################################################
  # MBITES-Complex-Energetics.R
  ##############################################################

  MosquitoFemale$set(which = "public",name = "BloodMeal",
            value = mbites_BloodMeal, overwrite = overwrite
  )


  ##############################################################
  # MBITES-Complex-Bouts.R
  ##############################################################

  switch(timing,
    exponential = {
      MosquitoFemale$set(which = "public",name = "timing",
                value = mbites_timingExponential, overwrite = overwrite
      )
    },
    gamma = {
      MosquitoFemale$set(which = "public",name = "timing",
                value = mbites_timingGamma, overwrite = overwrite
      )
    },
    weibull = {
      stop("sean hasn't derived semi-Markov mosquito processes based on weibull distributed waiting times yet. maybe when his days exceed 24 hours.")
    },
    {stop(cat("unrecognized entry for timing, expected character in 'exponential', 'gamma', 'weibull', got: ",timing,"\n",sep=""))}
  )

  MosquitoFemale$set(which = "public",name = "get_MySiteType",
            value = mbites_get_MySiteType, overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "get_WTS",
            value = mbites_get_WTS, overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "newSpot",
            value = mbites_newSpot, overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "enterHouse",
            value = mbites_enterHouse, overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "landingSpot",
            value = mbites_landingSpot, overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "boutF",
            value = mbites_boutF, overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "boutB",
            value = mbites_boutB, overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "boutR",
            value = mbites_boutR, overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "boutL",
            value = mbites_boutL, overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "boutO",
            value = mbites_boutO, overwrite = overwrite
  )

  switch(aquaModule,
    emerge = {
      MosquitoFemale$set(which = "public",name = "layEggs",
                value = mbites_layEggs_Emerge, overwrite = overwrite
      )
    },
    EL4P = {
      MosquitoFemale$set(which = "public",name = "layEggs",
                value = mbites_layEggs_EL4P, overwrite = overwrite
      )
    },
    {stop("unrecognized Aquatic Ecology module")}
  )


  MosquitoFemale$set(which = "public",name = "boutS",
            value = mbites_boutS, overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "boutM",
            value = mbites_boutM, overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "oneBout",
            value = mbites_oneBout, overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "MBITES",
            value = mbites_oneMosquito_MBITES, overwrite = overwrite
  )

  MosquitoPopFemale$set(which = "public",name = "MBITES",
            value = mbites_Pop_MBITES, overwrite = overwrite
  )

}
