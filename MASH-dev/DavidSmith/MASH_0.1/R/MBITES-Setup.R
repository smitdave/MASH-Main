#################################################################
#
#   MASH
#   R6-ified
#   M-BITES Generic Methods
#   Setup Module
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   July 19, 2017
#
#################################################################


##############################################################
# Setup Function
##############################################################

#' Initialize Generic Methods for M-BITES
#'
#' This function initializes generic methods for M-BITES models; please note that the
#' switches for this function modify only the methods that are added to the \code{\link{MicroMosquitoFemale}}
#' and \code{\link{MicroMosquitoMale}} classes.
#'
#' @param batchSize character switch that should be one of \code{"bms","norm"} for egg batch sizes dependent on bloodmeal size or normally distributed
#' @param eggMatT character switch that should be one of \code{"off","norm"} for egg batch maturation time turned off or normally distributed
#'
#'
#'
#' @return modifies the \code{\link{MicroMosquitoFemale}} and \code{\link{MicroMosquitoMale}} classes.
#' @export
MBITES.Generic.Setup <- function(
  overwrite = TRUE,
  batchSize = "bms",
  eggMatT = "off"
  ){

  # alert user
  message("initializing M-BITES generic shared methods")

  ##############################################################
  # MBITES-ChooseHost.R
  ##############################################################

  MicroMosquitoFemale$set(which = "public",name = "chooseHost",
            value = mbitesGeneric_chooseHost,
            overwrite = overwrite
  )

  ##############################################################
  # MBITES-Auxiliary.R
  ##############################################################

  MicroMosquitoFemale$set(which = "public",name = "isAlive",
            value = mbitesGeneric_isAlive,
            overwrite = overwrite
  )

  MicroMosquitoFemale$set(which = "public",name = "isActive",
            value = mbitesGeneric_isActive,
            overwrite = overwrite
  )

  MicroMosquitoMale$set(which = "public",name = "isAlive",
            value = mbitesGeneric_isAlive,
            overwrite = overwrite
  )

  MicroMosquitoMale$set(which = "public",name = "isActive",
            value = mbitesGeneric_isActive,
            overwrite = overwrite
  )

  ##############################################################
  # MBITES-Energetics.R
  ##############################################################

  MicroMosquitoFemale$set(which = "public",name = "pEnergySurvival",
            value = mbitesGeneric_pEnergySurvival,
            overwrite = overwrite
  )

  MicroMosquitoFemale$set(which = "public",name = "pSugarBout",
            value = mbitesGeneric_pSugarBout,
            overwrite = overwrite
  )

  MicroMosquitoFemale$set(which = "public",name = "rBloodMealSize",
            value = mbitesGeneric_rBloodMealSize,
            overwrite = overwrite
  )

  MicroMosquitoFemale$set(which = "public",name = "pOverFeed",
            value = mbitesGeneric_pOverFeed,
            overwrite = overwrite
  )

  MicroMosquitoFemale$set(which = "public",name = "pReFeed",
            value = mbitesGeneric_pReFeed,
            overwrite = overwrite
  )

  # rBatchSize
  switch(batchSize,
    bms = {
      MicroMosquitoFemale$set(which = "public",name = "rBatchSize",
                value = mbitesGeneric_rBatchSizeBms,
                overwrite = overwrite
      )
    },
    norm = {
      MicroMosquitoFemale$set(which = "public",name = "rBatchSize",
                value = mbitesGeneric_rBatchSizeNorm,
                overwrite = overwrite
      )
    },
    {stop("unrecognized entry for batchSize")}
  )

  # rEggMaturationTime
  switch(eggMatT,
    off = {
      MicroMosquitoFemale$set(which = "public",name = "rEggMaturationTime",
                value = mbitesGeneric_rEggMaturationTimeOff,
                overwrite = overwrite
      )
    },
    norm = {
      MicroMosquitoFemale$set(which = "public",name = "rEggMaturationTime",
                value = mbitesGeneric_rEggMaturationTimeNorm,
                overwrite = overwrite
      )
    },
    {stop("unrecognized entry for eggMatT")}
  )

  # MicroMosquitoFemale$set(which = "public",name = "makeBatches",
  #           value = mbitesGeneric_makeBatches,
  #           overwrite = overwrite
  # )

  ##############################################################
  # MBITES-Survival.R
  ##############################################################

  MicroMosquitoFemale$set(which = "public",name = "surviveFlight",
            value = mbitesGeneric_surviveFlight,
            overwrite = overwrite
  )

  MicroMosquitoFemale$set(which = "public",name = "rTatterSize",
            value = mbitesGeneric_rTatterSize,
            overwrite = overwrite
  )

  MicroMosquitoFemale$set(which = "public",name = "pTatter",
            value = mbitesGeneric_pTatter,
            overwrite = overwrite
  )

  MicroMosquitoFemale$set(which = "public",name = "pSenesce",
            value = mbitesGeneric_pSenesce,
            overwrite = overwrite
  )

  MicroMosquitoFemale$set(which = "public",name = "surviveResting",
            value = mbitesGeneric_surviveResting,
            overwrite = overwrite
  )

}
