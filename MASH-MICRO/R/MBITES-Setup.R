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
#' @param overwrite overwrite existing methods
#' @param SUGAR enable/disable sugar feeding and sugar energetics
#' @param MATE enable/disable mating behavior
#' @param aquaModule character switch in \code{"emerge","EL4P"}, selects aquatic ecology module
#' @param timing character switch in \code{"exponential","gamma"}, selects waiting time distribution from \code{\link{mbites_timingExponential}} or \code{\link{mbites_timingGamma}}
#' @param tattering character switch in \code{"mean","exact"}, mean will sample wing tattering from \code{\link{mbites_WingTattering}} regardless of flight distance
#' @param energy character switch in \code{"mean","exact"}, mean will sample energy consumption
#' @param batchSize character switch in \code{"bms","norm"} for egg batch sizes dependent on bloodmeal size or normally distributed
#' @param eggMatT character switch in \code{"off","norm"} for egg batch maturation time turned off or normally distributed
#'
#'
#' @return writes methods to \code{\link{MosquitoFemale}} and \code{\link{MosquitoMale}} classes.
#' @export
MBITES.Complex.Setup <- function(
  overwrite = TRUE,
  SUGAR = TRUE,
  MATE = TRUE,
  aquaModule = "emerge",
  timing = "exponential",
  tattering = "mean",
  energy = "mean",
  batchSize = "bms",
  eggMatT = "off"
){

  # alert user
  cat("initializing M-BITES complex module\n",sep="")

  ##############################################################
  # MBITES-Energetics.R
  ##############################################################

  MosquitoFemale$set(which = "public",name = "BloodMeal",
            value = mbites_BloodMeal, overwrite = overwrite
  )

  ##############################################################
  # MBITES-Survival.R
  ##############################################################

  MosquitoFemale$set(which = "public",name = "get_surviveFlightProb",
            value = mbites_get_surviveFlightProb, overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "get_restHaz",
            value = mbites_get_restHaz, overwrite = overwrite
  )


  ##############################################################
  # MBITES-Timing.R
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
    {stop(cat("unrecognized entry for timing, expected character in 'exponential', 'gamma', 'weibull', got: ",timing,"\n",sep=""))}
  )

  ##############################################################
  # MBITES-Bouts.R
  ##############################################################

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

  MosquitoFemale$set(which = "public",name = "restingSpot",
            value = mbites_restingSpot, overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "searchFail",
            value = mbites_searchFail, overwrite = overwrite
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

  MosquitoFemale$set(which = "public",name = "OvipositSearchCheck",
            value = mbites_OvipositSearchCheck, overwrite = overwrite
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


  if(SUGAR){
    MosquitoFemale$set(which = "public",name = "boutS",
              value = mbites_boutS, overwrite = overwrite
    )
  }

  if(MATE){
    MosquitoFemale$set(which = "public",name = "boutM",
              value = mbites_boutM, overwrite = overwrite
    )
  }

  MosquitoFemale$set(which = "public",name = "oneBout",
            value = mbites_oneBout, overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "MBITES",
            value = mbites_oneMosquito_MBITES, overwrite = overwrite
  )

  MosquitoPopFemale$set(which = "public",name = "MBITES",
            value = mbites_Pop_MBITES, overwrite = overwrite
  )

  #################################################################
  # MBITES-HostEncounter.R
  #################################################################

  MosquitoFemale$set(which = "public",name = "humanEncounter",
            value = mbites_humanEncounter,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "zooEncounter",
            value = mbites_zooEncounter,
            overwrite = overwrite
  )

  ##############################################################
  # MBITES-ChooseHost.R
  ##############################################################

  MosquitoFemale$set(which = "public",name = "chooseHost",
            value = mbites_chooseHost,
            overwrite = overwrite
  )

  ##############################################################
  # MBITES-ChooseMate.R
  ##############################################################

  MosquitoFemale$set(which = "public",name = "chooseMate",
            value = mbites_chooseMate,
            overwrite = overwrite
  )

  ##############################################################
  # MBITES-Auxiliary.R
  ##############################################################

  MosquitoFemale$set(which = "public",name = "isAlive",
            value = mbites_isAlive,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "isActive",
            value = mbites_isActive,
            overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "isAlive",
            value = mbites_isAlive,
            overwrite = overwrite
  )

  MosquitoMale$set(which = "public",name = "isActive",
            value = mbites_isActive,
            overwrite = overwrite
  )

  ##############################################################
  # MBITES-Energetics.R
  ##############################################################

  MosquitoFemale$set(which = "public",name = "sugarEnergetics",
            value = mbites_sugarEnergetics, overwrite = overwrite
  )

  if(energy=="mean"){

    MosquitoFemale$set(which = "public",name = "flightEnergetics",
              value = mbites_flightEnergetics_Mean, overwrite = overwrite
    )

  } else if(energy=="exact"){

    MosquitoFemale$set(which = "public",name = "flightEnergetics",
              value = mbites_flightEnergetics_Exact, overwrite = overwrite
    )

  } else {
    stop(cat("unrecognized entry for energy, expected character in 'mean', 'exact', got: ",energy,"\n",sep=""))
  }

  MosquitoFemale$set(which = "public",name = "queueSugarBout",
            value = mbites_queueSugarBout,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "pEnergySurvival",
            value = mbites_pEnergySurvival,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "pSugarBout",
            value = mbites_pSugarBout,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "rBloodMealSize",
            value = mbites_rBloodMealSize,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "pOverFeed",
            value = mbites_pOverFeed,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "pReFeed",
            value = mbites_pReFeed,
            overwrite = overwrite
  )

  # rBatchSize
  switch(batchSize,
    bms = {
      MosquitoFemale$set(which = "public",name = "rBatchSize",
                value = mbites_rBatchSizeBms,
                overwrite = overwrite
      )
    },
    norm = {
      MosquitoFemale$set(which = "public",name = "rBatchSize",
                value = mbites_rBatchSizeNorm,
                overwrite = overwrite
      )
    },
    {stop("unrecognized entry for batchSize")}
  )

  # rEggMaturationTime
  switch(eggMatT,
    off = {
      MosquitoFemale$set(which = "public",name = "rEggMaturationTime",
                value = mbites_rEggMaturationTimeOff,
                overwrite = overwrite
      )
    },
    norm = {
      MosquitoFemale$set(which = "public",name = "rEggMaturationTime",
                value = mbites_rEggMaturationTimeNorm,
                overwrite = overwrite
      )
    },
    {stop("unrecognized entry for eggMatT")}
  )


  ##############################################################
  # MBITES-Survival.R
  ##############################################################

  MosquitoFemale$set(which = "public",name = "surviveFlight",
            value = mbites_surviveFlight,
            overwrite = overwrite
  )

  if(tattering=="mean"){

    MosquitoFemale$set(which = "public",name = "WingTattering",
              value = mbites_WingTattering_Mean, overwrite = overwrite
    )

  } else if(tattering=="exact"){

    MosquitoFemale$set(which = "public",name = "WingTattering",
              value = mbites_WingTattering_Exact, overwrite = overwrite
    )

  } else {
    stop(cat("unrecognized entry for tattering, expected character in 'mean', 'exact', got: ",energy,"\n",sep=""))
  }


  MosquitoFemale$set(which = "public",name = "pTatter",
            value = mbites_pTatter,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "pSenesce",
            value = mbites_pSenesce,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "surviveResting",
            value = mbites_surviveResting,
            overwrite = overwrite
  )


}
