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


###############################################################################
# Output
###############################################################################

# #' Initialize Patch Output
# #'
# #' Initialize patch output
# #'
# #'  * This method is bound to \code{MacroTile$initOutput}
# #'
# initOutput_Patch <- function(con){
#   writeLines(text = paste0(c("time","patchID","bWeightHuman","bWeightZoo","bWeightZootox","kappa"),collapse = ","),con = con, sep = "\n")
# }
#
# MacroTile$set(which = "public",name = "initOutput",
#           value = initOutput_Patch, overwrite = TRUE
# )

#' Write Patch Output
#'
#' Write patch output
#'
#'  * This method is bound to \code{MacroPatch$output}
#'
output_Patch <- function(con){
  tNow = private$TilePointer$get_tNow()
  writeLines(text = paste0(c(tNow,private$patchID,private$bWeightHuman,private$bWeightZoo,private$bWeightZootox,private$kappa),collapse = ","), con = con, sep = "\n")
}

MacroPatch$set(which = "public",name = "output",
          value = output_Patch, overwrite = TRUE
)

###############################################################################
# Reset
###############################################################################

#' Reset Patch
#'
#' Reset patch between simulation runs
#'
#'  * This method is bound to \code{MacroPatch$reset}
#'
reset_Patch <- function(bWeightZoo, bWeightZootox){

  # reset aquatic population
  private$AquaPop$reset()

  # reset biting parameters
  private$kappa = 0
  private$bWeightHuman = 0
  private$bWeightZoo = bWeightZoo
  private$bWeightZootox = bWeightZootox
}

MacroPatch$set(which = "public",name = "reset",
          value = reset_Patch, overwrite = TRUE
)
