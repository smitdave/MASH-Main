#################################################################
#
#   MASH
#   MICRO Aquatic Ecology: Emerge
#   Setup Function
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   July 31, 2017
#
#################################################################

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
MICRO.Emerge.Setup <- function(overwrite = TRUE){

  message("initializing 'Emerge' module for Aquatic Ecology")

  #################################################################
  # One Day 'Emerge'
  #################################################################

  # lambda to ImagoQ
  AquaticSite$set(which = "public",name = "oneStep_EmergeSite",
            value = oneStep_MicroEmergeSite,
            overwrite = overwrite
  )

  # lambda to ImagoQ
  Landscape$set(which = "public",name = "oneStep_AquaticEcology",
            value = oneStep_MicroEmerge,
            overwrite = overwrite
  )

  # ImagoQ to MicroMosquitoPopFemale
  AquaticSite$set(which = "public",name = "addCohort_MicroEmergeSite",
            value = addCohort_MicroEmergeSite,
            overwrite = overwrite
  )

  # ImagoQ to MicroMosquitoPopFemale
  Landscape$set(which = "public",name = "addCohort",
            value = addCohort_MicroEmerge,
            overwrite = overwrite
  )



  #################################################################
  # Lambda
  #################################################################

  AquaticSite$set(which = "public",name = "get_lambda",
            value = get_lambda_MicroEmerge,
            overwrite = overwrite
  )

  AquaticSite$set(which = "public",name = "set_lambda",
            value = set_lambda_MicroEmerge,
            overwrite = overwrite
  )

}
