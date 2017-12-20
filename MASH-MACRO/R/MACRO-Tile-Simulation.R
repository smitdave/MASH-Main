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

###############################################################################
# Run Simulation
###############################################################################

#' Run a MACRO Simulation
#'
#' write me
#'
#'  * This method is bound to \code{MacroTile$simMacro}
#'
simMacro <- function(tMax, PfPAR){

  cat("initializing simulation, ",private$runID,"\n",sep="")

  # open connections
  self$initCon()

  # mosquito
  private$Mosquito$initialize_output(con = private$conMosquito)

  # patches
  writeLines(text = paste0(c("time","patchID","bWeightHuman","bWeightZoo","bWeightZootox","kappa"),collapse = ","),con = private$conPatches, sep = "\n")

  # human output
  # humans
  conPathogen = file(description=paste0(private$directory,"/HumanPathogen_Run",private$runID,".csv"),open="wt")
  conMove = file(description=paste0(private$directory,"/HumanMove_Run",private$runID,".csv"),open="wt")
  private$HumanPop$set_conPathogen(conPathogen)
  private$HumanPop$set_conMove(conMove)
  private$HumanPop$initialize_output_Pathogen()
  private$HumanPop$initialize_output_Move()

  # initialize humans
  private$HumanPop$initialize_Pathogens(PfPAR)
  private$HumanPop$initialize_bWeightHuman()
  private$HumanPop$initialize_travel()

  # progress bar
  progress_bar = txtProgressBar(min=1,max=tMax,style=3)

  cat("beginning simulation ",private$runID,"\n",sep="")

  while(private$tNow < tMax) {
    # increment time
    private$tNow = private$tNow + 1

    # simulation
    private$Patches$apply(tag="oneDay_popDynamics")
    private$Patches$apply(tag="oneDay_addCohort")

    private$Mosquito$oneDay_popDynamics()
    private$Mosquito$oneDay_oviposition()

    private$HumanPop$simHumans(tPause=private$tNow)

    private$HumanPop$updateKappa()
    private$HumanPop$updateEIR()
    private$HumanPop$queueInfectiousBites()

    # output
    private$Mosquito$output(con = private$conMosquito)
    private$Patches$apply(tag="output",returnVal=FALSE,con = private$conPatches)

    setTxtProgressBar(progress_bar,private$tNow)
  }
  cat("\n")

  # close connections
  self$closeCon()
  private$HumanPop$close_conPathogen()
  private$HumanPop$close_conMove()
}

MacroTile$set(which = "public",name = "simMacro",
          value = simMacro, overwrite = TRUE
)


###############################################################################
# Reset Tile
###############################################################################

#' Reset Tile between Runs
#'
#' write me
#'
#'  * This method is bound to \code{MacroTile$resetMacro}
#'
resetMacro <- function(PatchPar, MosquitoPar, HumanPar){

  # reset patches
  for(i in 1:private$nPatch){
    private$Patches$get(as.character(i))$reset(bWeightZoo=PatchPar[[i]]$bWeightZoo, bWeightZootox=PatchPar[[i]]$bWeightZootox)
  }

  # reset mosquitoes
  private$Mosquito$reset(MosquitoPar)

  # reset humans
  private$HumanPop$reset(HumanPar)

  # reset tile
  private$tNow = private$tStart
  private$runID = private$runID + 1L
}

MacroTile$set(which = "public",name = "resetMacro",
          value = resetMacro, overwrite = TRUE
)
