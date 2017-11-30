###############################################################################
#     _______ __
#    /_  __(_) /__
#     / / / / / _ \
#    / / / / /  __/
#   /_/ /_/_/\___/
#
#   MASH-MACRO
#   Tile Simulation
#   MASH Team
#   November 2017
#
###############################################################################

#' Run a MACRO Simulation
#'
#' write more stuff!!!!!!!!
#'
#'  * This method is bound to \code{MacroTile$simMacro}
#'
simMacro <- function(tMax){
  private$tNow = private$tStart
  cat("beginning simulation\n",sep="")
  while(private$tNow < tMax){
    private$tNow = private$tNow + 1

    private$Patches$apply(tag="oneDay_popDynamics")
    private$Patches$apply(tag="oneDay_addCohort")

    private$Mosquito$oneDay_popDynamics()
    private$Mosquito$oneDay_oviposition()

    cat("day: ",private$tNow,"\n",sep="")
  }
}

MacroTile$set(which = "public",name = "simMacro",
          value = simMacro, overwrite = TRUE
)
