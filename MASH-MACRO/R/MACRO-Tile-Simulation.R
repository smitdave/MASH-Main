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
#' write me
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


#' Reset Tile between Runs
#'
#' write me
#'
#'  * This method is bound to \code{MacroTile$resetMacro}
#'
resetMacro <- function(PatchPar, MosquitoPar){

  # reset patches
  for(i in 1:private$nPatch){
    private$Patches$get(as.character(i))$reset(bWeightZoo=PatchPar[[i]]$bWeightZoo, bWeightZootox=PatchPar[[i]]$bWeightZootox)
  }

  # reset mosquitoes
  private$Mosquito$reset(MosquitoPar)
}

MacroTile$set(which = "public",name = "resetMacro",
          value = resetMacro, overwrite = TRUE
)
