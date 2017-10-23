#################################################################
#
#   MASH
#   R6-ified
#   MBITES-BRO (Blood Feeding, Resting, Oviposition)
#   Simplified Cohort Simulation Setup
#   David Smith, Hector Sanchez, Sean Wu
#   August 9, 2017
#
#################################################################

##############################################################
# Setup Function
##############################################################

#' MBITES-BRO-Cohort: Initialize Additional Methods & Fields in \code{\link{MicroMosquitoPopFemale}} and \code{\link{MicroMosquitoFemale}}
#'
#' WRITE MEEEEE Initialize M-BITES BRO (Blood Feeding, Resting, Oviposition) lifecycle model.
#'
#' @param overwrite overwrite methods
#' @examples
#' MBITES.BRO.Setup()
#' @export
MBITES.BRO.Cohort.Setup <- function(overwrite = TRUE){

  # alert user
  message("initializing MBITES-BRO Cohort shared methods")

  MBITES.BRO.Setup(overwrite=TRUE,aquaModule="emerge")
  MBITES.Cohort.Setup(overwrite=TRUE)

  ##############################################################
  # MBITES-BRO-Cohort.R
  ##############################################################

  # humanEncounter
  MicroMosquitoFemale$set(which = "public",name = "humanEncounter",
            value = mbitesBRO_cohort_humanEncounter,
            overwrite = overwrite
  )

  # MBITES_Cohort
  MicroMosquitoFemale$set(which = "public",name = "MBITES_Cohort",
            value = mbitesBRO_cohort_oneMosquito_MBITES,
            overwrite = overwrite
  )

  # simCohort
  MicroMosquitoPopFemale$set(which = "public",name = "simCohort",
            value = mbitesBRO_cohort_simCohort,
            overwrite = overwrite
  )

}
