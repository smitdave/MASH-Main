###############################################################################
#
#      ___   ____  __  _____
#     /   | / __ \/ / / /   |
#    / /| |/ / / / / / / /| |
#   / ___ / /_/ / /_/ / ___ |
#  /_/  |_\___\_\____/_/  |_|
#
#   MASH-MICRO
#   AQUATIC ECOLOGY: EL4P Setup
#   MASH-MICRO Team
#   October 2017
#
###############################################################################



#' Initialize EL4P Aquatic Ecology Module
#'
#' This function initializes methods and fields for the 'EL4P' MODULE of Aquatic Ecology COMPONENT.
#' It modifies \code{\link{AquaticSite}} and \code{\link{Landscape}} classes.
#'
#' @param overwrite overwrite existing methods
#' @examples
#' AQUA.EL4P.Setup()
#' @export
AQUA.EL4P.Setup <- function(overwrite = TRUE){

  cat("initializing EL4P Aquatic Ecology module\n",sep="")

  # #################################################################
  # # One Day 'Emerge'
  # #################################################################
  #
  # # lambda to ImagoQ
  # AquaticSite$set(which = "public",name = "oneStep_EmergeSite",
  #           value = oneStep_EmergeSite,
  #           overwrite = overwrite
  # )
  #
  # # lambda to ImagoQ
  # Landscape$set(which = "public",name = "oneStep_AquaticEcology",
  #           value = oneStep_Emerge,
  #           overwrite = overwrite
  # )
  #
  # # ImagoQ to MosquitoPopFemale
  # AquaticSite$set(which = "public",name = "addCohort_MicroEmergeSite",
  #           value = addCohort_EmergeSite,
  #           overwrite = overwrite
  # )
  #
  # # ImagoQ to MosquitoPopFemale
  # Landscape$set(which = "public",name = "addCohort_AquaticEcology",
  #           value = addCohort_Emerge,
  #           overwrite = overwrite
  # )
  #
  #
  #
  # #################################################################
  # # Lambda
  # #################################################################
  #
  # AquaticSite$set(which = "public",name = "get_lambda",
  #           value = get_lambda_Emerge,
  #           overwrite = overwrite
  # )
  #
  # AquaticSite$set(which = "public",name = "set_lambda",
  #           value = set_lambda_Emerge,
  #           overwrite = overwrite
  # )

  #################################################################
  # EL4P
  #################################################################

  AquaticSite$set(which = "private",name = "EL4P",
            value = {MASHmicro::EL4P$new(NULL,NULL,NULL,NULL,NULL,NULL,NULL,1,0.01)},
            overwrite = overwrite
  )

  AquaticSite$set(which = "public",name = "get_EL4P",
            value = get_EL4P_AquaticSite,
            overwrite = overwrite
  )

}
