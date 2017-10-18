###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES-Male: Setup
#   MASH-MICRO Team
#   September 18, 2017
#
###############################################################################

#' Initialize Male Methods for M-BITES
#'
#' This function initializes methods for M-BITES Male module; this function adds methods to the \code{\link{MosquitoMale}} class.
#'
#' @param timing character in "exponential", "gamma", "weibull"
#'
#'
#'
#' @return writes methods to \code{\link{MosquitoMale}} class.
#' @export
MBITES.Male.Setup <- function(
  overwrite = TRUE,
  timing = "exponential"
  ){

  # alert user
  cat("initializing M-BITES Male module\n",sep="")

  ##############################################################
  # MBITES-Male-Survival.R
  ##############################################################

  MosquitoMale$set(which = "public",name = "surviveResting",
            value = mbitesMale_surviveResting, overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "get_restHaz",
            value = mbitesMale_get_restHaz, overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "surviveFlight",
            value = mbitesMale_surviveFlight, overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "pSenesce",
            value = mbitesMale_pSenesce, overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "rTatterSize",
            value = mbitesMale_rTatterSize, overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "pTatter",
            value = mbitesMale_pTatter, overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "get_surviveFlightProb",
            value = mbitesMale_get_surviveFlightProb, overwrite = overwrite
  )

  ##############################################################
  # MBITES-Male-Energetics.R
  ##############################################################

  MosquitoMale$set(which = "public",name = "sugarEnergetics",
            value = mbitesMale_sugarEnergetics, overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "pEnergySurvival",
            value = mbitesMale_pEnergySurvival, overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "pSugarBout",
            value = mbitesMale_pSugarBout, overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "queueSugarBout",
            value = mbitesMale_queueSugarBout, overwrite = overwrite
  )

  ##############################################################
  # MBITES-Male-Bouts.R
  ##############################################################

  switch(timing,
    exponential = {
      MosquitoMale$set(which = "public",name = "timing",
                value = mbitesMale_timingExponential, overwrite = overwrite
      )
    },
    gamma = {
      MosquitoMale$set(which = "public",name = "timing",
                value = mbitesMale_timingGamma, overwrite = overwrite
      )
    },
    weibull = {
      stop(cat("sean hasn't written weibull waiting times yet\n",sep=""))
    },
    {stop(cat("unrecognized entry for timing, expected character in 'exponential', 'gamma', 'weibull', got: ",timing,"\n",sep=""))}
  )

  MosquitoMale$set(which = "public",name = "get_MySiteType",
            value = mbitesMale_get_MySiteType, overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "get_WTS",
            value = mbitesMale_get_WTS, overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "newSpot",
            value = mbitesMale_newSpot, overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "enterHouse",
            value = mbitesMale_enterHouse, overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "restingSpot",
            value = mbitesMale_restingSpot, overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "boutM",
            value = mbitesMale_boutM, overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "boutS",
            value = mbitesMale_boutS, overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "boutR",
            value = mbitesMale_boutR, overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "oneBout",
            value = mbitesMale_oneBout, overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "MBITES",
            value = mbitesMale_oneMosquito_MBITES, overwrite = overwrite
  )

  MosquitoPopMale$set(which = "public",name = "MBITES",
            value = mbitesMale_Pop_MBITES, overwrite = overwrite
  )


} # end setup
