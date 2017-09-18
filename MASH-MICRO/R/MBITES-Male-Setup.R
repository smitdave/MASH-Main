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
MBITES.Generic.Setup <- function(
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




}
