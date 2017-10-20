#################################################################
#
#   MASH
#   R6-ified
#   M-BITES BRO
#   Setup Module
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   July 19, 2017
#
#################################################################

#################################################################
# Initialize Methods and Fields (Setup)
#################################################################

#' MBITES-BRO: Initialize Additional Methods & Fields in \code{\link{MicroMosquitoPopFemale}} and \code{\link{MicroMosquitoFemale}}
#'
#' Initialize M-BITES BRO (Blood Feeding, Resting, Oviposition) lifecycle model.
#'
#' @param overwrite overwrite methods
#' @param aquaModule character in "emerge" or "EL4P"
#' @examples
#' MBITES.BRO.Setup()
#' @export
MBITES.BRO.Setup <- function(overwrite = TRUE, aquaModule){

    message("initializing M-BITES BRO methods")

    #################################################################
    # MBITES-BRO-Bouts.R
    #################################################################

    MicroMosquitoFemale$set(which = "public",name = "timingExponential",
              value = mbitesBRO_timingExponential,
              overwrite = overwrite
    )

    MicroMosquitoFemale$set(which = "public",name = "get_MySiteType",
              value = mbitesBRO_get_MySiteType,
              overwrite = overwrite
    )

    MicroMosquitoFemale$set(which = "public",name = "get_WTS",
              value = mbitesBRO_get_WTS,
              overwrite = overwrite
    )

    MicroMosquitoFemale$set(which = "public",name = "newSpot",
              value = mbitesBRO_newSpot,
              overwrite = overwrite
    )

    MicroMosquitoFemale$set(which = "public",name = "enterHouse",
              value = mbitesBRO_enterHouse,
              overwrite = overwrite
    )

    MicroMosquitoFemale$set(which = "public",name = "landingSpot",
              value = mbitesBRO_landingSpot,
              overwrite = overwrite
    )

    MicroMosquitoFemale$set(which = "public",name = "boutB",
              value = mbitesBRO_boutB,
              overwrite = overwrite
    )

    MicroMosquitoFemale$set(which = "public",name = "boutR",
              value = mbitesBRO_boutR,
              overwrite = overwrite
    )

    MicroMosquitoFemale$set(which = "public",name = "boutO",
              value = mbitesBRO_boutO,
              overwrite = overwrite
    )

    switch(aquaModule,
      emerge = {
        MicroMosquitoFemale$set(which = "public",name = "layEggs",
                  value = mbitesBRO_layEggs_Emerge,
                  overwrite = overwrite
        )
      },
      EL4P = {
        MicroMosquitoFemale$set(which = "public",name = "layEggs",
                  value = mbitesBRO_layEggs_EL4P,
                  overwrite = overwrite
        )
      },
      {stop("unrecognized Aquatic Ecology module")}
    )

    MicroMosquitoFemale$set(which = "public",name = "oneBout",
              value = mbitesBRO_oneBout,
              overwrite = overwrite
    )

    MicroMosquitoFemale$set(which = "public",name = "MBITES",
              value = mbitesBRO_oneMosquito_MBITES,
              overwrite = overwrite
    )

    MicroMosquitoPopFemale$set(which = "public",name = "MBITES",
              value = mbitesBRO_Pop_MBITES,
              overwrite = overwrite
    )

    #################################################################
    # MBITES-BRO-Energetics.R
    #################################################################

    MicroMosquitoFemale$set(which = "public",name = "BloodMeal",
              value = mbitesBRO_BloodMeal,
              overwrite = overwrite
    )

    #################################################################
    # MBITES-BRO-HostEncounter.R
    #################################################################

    MicroMosquitoFemale$set(which = "public",name = "humanEncounter",
              value = mbitesBRO_humanEncounter,
              overwrite = overwrite
    )

    MicroMosquitoFemale$set(which = "public",name = "zooEncounter",
              value = mbitesBRO_zooEncounter,
              overwrite = overwrite
    )

    #################################################################
    # MBITES-BRO-Survival.R
    #################################################################

    MicroMosquitoFemale$set(which = "public",name = "get_surviveFlightProb",
              value = mbitesBRO_get_surviveFlightProb,
              overwrite = overwrite
    )

    MicroMosquitoFemale$set(which = "public",name = "get_restHaz",
              value = mbitesBRO_get_restHaz,
              overwrite = overwrite
    )

    MicroMosquitoFemale$set(which = "public",name = "landingSpot",
              value = mbitesBRO_landingSpot,
              overwrite = overwrite
    )

    MicroMosquitoFemale$set(which = "public",name = "boutB",
              value = mbitesBRO_boutB,
              overwrite = overwrite
    )

    MicroMosquitoFemale$set(which = "public",name = "boutR",
              value = mbitesBRO_boutR,
              overwrite = overwrite
    )


}
