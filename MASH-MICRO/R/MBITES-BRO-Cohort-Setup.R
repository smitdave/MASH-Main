###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES-BRO: Cohort Setup
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################

##############################################################
# Setup Function
##############################################################

#' MBITES-BRO-Cohort: Initialize Additional Methods & Fields in \code{\link{MosquitoPopFemale}} and \code{\link{MosquitoFemale}}
#'
#' WRITE MEEEEE Initialize M-BITES BRO (Blood Feeding, Resting, Oviposition) lifecycle model.
#'
#' @param overwrite overwrite methods
#' @examples
#' MBITES.BRO.Setup()
#' @export
MBITES.BRO.Cohort.Setup <- function(overwrite = TRUE){

  # alert user
  cat("initializing M-BITES BRO Cohort module\n",sep="")

  ##############################################################
  # MBITES-BRO-Cohort.R
  ##############################################################

  # chooseHost
  MosquitoFemale$set(which = "public",name = "chooseHost",
            value = mbitesCohort_chooseHost,
            overwrite = overwrite
  )

  # humanEncounter
  MosquitoFemale$set(which = "public",name = "humanEncounter",
            value = mbitesBRO_cohort_humanEncounter,
            overwrite = overwrite
  )

  # MBITES_Cohort
  MosquitoFemale$set(which = "public",name = "MBITES_Cohort",
            value = mbitesBRO_cohort_oneMosquito_MBITES,
            overwrite = overwrite
  )

  # simCohort
  MosquitoPopFemale$set(which = "public",name = "simCohort",
            value = mbitesBRO_cohort_simCohort,
            overwrite = overwrite
  )

}
