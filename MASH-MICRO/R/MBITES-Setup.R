###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES-Setup
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################


##############################################################
# Setup Function
##############################################################

#' Initialize Generic Methods for M-BITES
#'
#' This function initializes generic methods for M-BITES models; please note that the
#' switches for this function modify only the methods that are added to the \code{\link{MosquitoFemale}}
#' and \code{\link{MosquitoMale}} classes.
#'
#' @param batchSize character switch that should be one of \code{"bms","norm"} for egg batch sizes dependent on bloodmeal size or normally distributed
#' @param eggMatT character switch that should be one of \code{"off","norm"} for egg batch maturation time turned off or normally distributed
#'
#'
#'
#' @return modifies the \code{\link{MosquitoFemale}} and \code{\link{MosquitoMale}} classes.
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

  MosquitoFemale$set(which = "public",name = "chooseHost",
            value = mbitesGeneric_chooseHost,
            overwrite = overwrite
  )

  ##############################################################
  # MBITES-Auxiliary.R
  ##############################################################

  MosquitoFemale$set(which = "public",name = "isAlive",
            value = mbitesGeneric_isAlive,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "isActive",
            value = mbitesGeneric_isActive,
            overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "isAlive",
            value = mbitesGeneric_isAlive,
            overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "isActive",
            value = mbitesGeneric_isActive,
            overwrite = overwrite
  )

  ##############################################################
  # MBITES-Energetics.R
  ##############################################################

  MosquitoFemale$set(which = "public",name = "pEnergySurvival",
            value = mbitesGeneric_pEnergySurvival,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "pSugarBout",
            value = mbitesGeneric_pSugarBout,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "rBloodMealSize",
            value = mbitesGeneric_rBloodMealSize,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "pOverFeed",
            value = mbitesGeneric_pOverFeed,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "pReFeed",
            value = mbitesGeneric_pReFeed,
            overwrite = overwrite
  )

  # rBatchSize
  switch(batchSize,
    bms = {
      MosquitoFemale$set(which = "public",name = "rBatchSize",
                value = mbitesGeneric_rBatchSizeBms,
                overwrite = overwrite
      )
    },
    norm = {
      MosquitoFemale$set(which = "public",name = "rBatchSize",
                value = mbitesGeneric_rBatchSizeNorm,
                overwrite = overwrite
      )
    },
    {stop("unrecognized entry for batchSize")}
  )

  # rEggMaturationTime
  switch(eggMatT,
    off = {
      MosquitoFemale$set(which = "public",name = "rEggMaturationTime",
                value = mbitesGeneric_rEggMaturationTimeOff,
                overwrite = overwrite
      )
    },
    norm = {
      MosquitoFemale$set(which = "public",name = "rEggMaturationTime",
                value = mbitesGeneric_rEggMaturationTimeNorm,
                overwrite = overwrite
      )
    },
    {stop("unrecognized entry for eggMatT")}
  )

  # MosquitoFemale$set(which = "public",name = "makeBatches",
  #           value = mbitesGeneric_makeBatches,
  #           overwrite = overwrite
  # )

  ##############################################################
  # MBITES-Survival.R
  ##############################################################

  MosquitoFemale$set(which = "public",name = "surviveFlight",
            value = mbitesGeneric_surviveFlight,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "rTatterSize",
            value = mbitesGeneric_rTatterSize,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "pTatter",
            value = mbitesGeneric_pTatter,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "pSenesce",
            value = mbitesGeneric_pSenesce,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "surviveResting",
            value = mbitesGeneric_surviveResting,
            overwrite = overwrite
  )

}
