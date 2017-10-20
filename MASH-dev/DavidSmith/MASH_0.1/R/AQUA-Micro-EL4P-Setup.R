#################################################################
#
#   MASH
#   MICRO Aquatic Ecology: EL4P
#   Setup Function
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   August 7, 2017
#
#################################################################

#################################################################
# Setup
#################################################################

#' Initialize EL4P Aquatic Ecology Module
#'
#' This function initializes methods and fields for the 'EL4P' MODULE of Aquatic Ecology COMPONENT.
#' It modifies \code{\link{AquaticSite}} and \code{\link{Landscape}} classes.
#'
#' @param dunno sdf
#' @return stuff
#' @examples
#' MICRO.EL4P.Setup()
#' @export
MICRO.EL4P.Setup <- function(overwrite = TRUE){

  #################################################################
  # Landscape and AquaticSite Methods
  #################################################################

  Landscape$set(which = "public",name = "updateLandscapeEL4P",
            value = updateLandscape_MicroEL4P,
            overwrite = overwrite
  )


  #################################################################
  # One Step 'EL4P'
  #################################################################

  AquaticSite$set(which = "public",name = "oneStep_EL4PSite",
            value = oneStep_MicroEL4PSite,
            overwrite = overwrite
  )

  Landscape$set(which = "public",name = "oneStep_AquaticEcology",
            value = oneStep_MicroEL4P,
            overwrite = overwrite
  )

  AquaticSite$set(which = "public",name = "addCohort_MicroEL4PSite",
            value = addCohort_MicroEL4PSite,
            overwrite = overwrite
  )

  Landscape$set(which = "public",name = "addCohort",
            value = addCohort_MicroEL4P,
            overwrite = overwrite
  )

}
