###############################################################################
#
#      ___   ____  __  _____
#     /   | / __ \/ / / /   |
#    / /| |/ / / / / / / /| |
#   / ___ / /_/ / /_/ / ___ |
#  /_/  |_\___\_\____/_/  |_|
#
#   MASH-MICRO
#   AQUATIC ECOLOGY: Emerge Setup
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################


#################################################################
# Setup
#################################################################

#' Initialize Emerge Aquatic Ecology Module
#'
#' This function initializes methods and fields for the 'Emerge' MODULE of Aquatic Ecology COMPONENT.
#' It modifies \code{\link{AquaticSite}} and \code{\link{Landscape}} classes.
#'
#' @param dunno sdf
#' @return stuff
#' @examples
#' Aqua.Emerge.Setup()
#' @export
AQUA.Emerge.Setup <- function(overwrite = TRUE){

  cat("initializing Emerge Aquatic Ecology module\n",sep="")

  #################################################################
  # One Day 'Emerge'
  #################################################################

  # lambda to ImagoQ
  AquaticSite$set(which = "public",name = "oneStep_EmergeSite",
            value = oneStep_EmergeSite,
            overwrite = overwrite
  )

  # lambda to ImagoQ
  Landscape$set(which = "public",name = "oneStep_AquaticEcology",
            value = oneStep_Emerge,
            overwrite = overwrite
  )

  # ImagoQ to MosquitoPopFemale
  AquaticSite$set(which = "public",name = "addCohort_MicroEmergeSite",
            value = addCohort_EmergeSite,
            overwrite = overwrite
  )

  # ImagoQ to MosquitoPopFemale
  Landscape$set(which = "public",name = "addCohort_AquaticEcology",
            value = addCohort_Emerge,
            overwrite = overwrite
  )



  #################################################################
  # Lambda
  #################################################################

  AquaticSite$set(which = "public",name = "get_lambda",
            value = get_lambda_Emerge,
            overwrite = overwrite
  )

  AquaticSite$set(which = "public",name = "set_lambda",
            value = set_lambda_Emerge,
            overwrite = overwrite
  )

}
