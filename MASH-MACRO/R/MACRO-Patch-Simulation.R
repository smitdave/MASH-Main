###############################################################################
#       ____        __       __
#      / __ \____ _/ /______/ /_
#     / /_/ / __ `/ __/ ___/ __ \
#    / ____/ /_/ / /_/ /__/ / / /
#   /_/    \__,_/\__/\___/_/ /_/
#
#   MASH-MACRO
#   Patch Class Simulation
#   MASH Team
#   November 2017
#
###############################################################################

#' Patch Aquatic Population Dynamics
#'
#' wrapper around the oneDay_popDynamics of the embedded AquaPop_Base for polymorphism
#'
oneDay_popDynamics_Patch <- function(){
  private$AquaPop$oneDay_popDynamics()
}

MacroPatch$set(which = "public",name = "oneDay_popDynamics",
          value = oneDay_popDynamics_Patch, overwrite = TRUE
)

#' Patch Aquatic Population Dynamics
#'
#' wrapper around the oneDay_addCohort of the embedded AquaPop_Base for polymorphism
#'
oneDay_addCohort_Patch <- function(){
  private$AquaPop$oneDay_addCohort(ix=private$patchID)
}

MacroPatch$set(which = "public",name = "oneDay_addCohort",
          value = oneDay_addCohort_Patch, overwrite = TRUE
)
