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

  # open connections
  self$initCon()
  private$Mosquito$initOutput(con = private$conMosquito)

  cat("beginning simulation\n",sep="")

  while(private$tNow < tMax){
    # increment time
    private$tNow = private$tNow + 1

    # simulation
    private$Patches$apply(tag="oneDay_popDynamics")
    private$Patches$apply(tag="oneDay_addCohort")

    private$Mosquito$oneDay_popDynamics()
    private$Mosquito$oneDay_oviposition()

    # output
    private$Mosquito$output(con = private$conMosquito)

    cat("day: ",private$tNow,"\n",sep="")
  }

  # close connections
  self$closeCon()
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

  # reset humans

  # reset tile
  private$tNow = private$tStart
  private$runID = private$runID + 1L
}

MacroTile$set(which = "public",name = "resetMacro",
          value = resetMacro, overwrite = TRUE
)

#' Initialize Output Connections
#'
#' Open \code{\link[base]{connection}} objects to write output
#'
#'  * This method is bound to \code{MacroTile$initCon}
#'
initCon_Tile <- function(){
  private$conMosquito = file(description=paste0(private$directory,"/Mosquito_Run",private$runID,".csv"),open="wt")
  # private$conHuman
}

MacroTile$set(which = "public",name = "initCon",
          value = initCon_Tile, overwrite = TRUE
)

#' Close Output Connections
#'
#' Close \code{\link[base]{connection}} objects
#'
#'  * This method is bound to \code{MacroTile$closeCon}
#'
closeCon_Tile <- function(){
  close(private$conMosquito)
  # private$conHuman
}

MacroTile$set(which = "public",name = "closeCon",
          value = closeCon_Tile, overwrite = TRUE
)
