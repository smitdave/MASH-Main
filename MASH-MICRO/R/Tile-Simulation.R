###############################################################################
#
#       __  ____               _______ __
#      /  |/  (_)_____________/_  __(_) /__
#     / /|_/ / / ___/ ___/ __ \/ / / / / _ \
#    / /  / / / /__/ /  / /_/ / / / / /  __/
#   /_/  /_/_/\___/_/   \____/_/ /_/_/\___/
#
#   MASH-MICRO
#   MICRO: Tile Simulation Methods
#   MASH-MICRO Team
#   September 12, 2017
#
###############################################################################


###############################################################################
# MICRO MicroTile Simulation
###############################################################################

#' MICRO \code{\link{MicroTile}}: One Simulation Run
#'
#' write me
#'  * This method is bound to \code{Tile$simMICRO_oneRun}
#'
#' @param tMax length of simulation run
#' @param PfPAR parameters passed to \code{HumanPop$initialize_Pathogens}
#' @param verbose print information
#' @param trackPop write mosquito population counts to CSV file
#'
simMICRO_oneRun <- function(tMax, PfPAR, verbose = FALSE, trackPop = FALSE){

  # set runID
  private$runID = private$runID + 1L
  cat("running MICRO simulation runID: ",private$runID,"\n",sep="")

  # reset tNow
  private$tNow = 1

  # set up output connections
  FemaleHistoryFile = paste0(self$get_MosquitoDirectory(),"FemaleMosquitoHistory_Run",private$runID,".json")
  self$set_FemaleHistoryCon(file(description = FemaleHistoryFile,open = "wt"))

  if(!is.null(private$MalePop)){
    MaleHistoryFile = paste0(self$get_MosquitoDirectory(),"MaleMosquitoHistory_Run",private$runID,".json")
    self$set_MaleHistoryCon(file(description = MaleHistoryFile,open = "wt"))
  }

  MosquitoPathogenFile = paste0(self$get_MosquitoDirectory(),"MosquitoPathogens_Run",private$runID,".json")
  self$set_MosquitoPathogenCon(file(description = MosquitoPathogenFile,open = "wt"))

  # HumanPathogenFile = paste0(self$get_HumanDirectory(),"HumanPathogens_Run",private$runID,".json")
  # self$set_HumanPathogenCon(file(description = HumanPathogenFile,open = "wt"))

  HumanPathogenFile = paste0(self$get_HumanDirectory(),"/HumanPathogen_Run",private$runID,".csv")
  conPathogen = file(description=HumanPathogenFile,open="wt")
  # conMove = file(description=paste0(self$get_HumanDirectory(),"/HumanMove_Run",private$runID,".csv"),open="wt")
  private$HumanPop$set_conPathogen(conPathogen)
  # private$HumanPop$set_conMove(conMove)
  private$HumanPop$initialize_output_Pathogen()
  # private$HumanPop$initialize_output_Move()

  if(trackPop){
    FemaleCSVFile = paste0(self$get_MosquitoDirectory(),"FemaleMosquitoPop_Run",private$runID,".csv")
    self$set_FemaleCSVCon(file(description = FemaleCSVFile,open = "wt"))
    writeLines(text = paste0(c("time",private$FemalePop$get_MBITES_PAR("stateSpace")),collapse = ","),con = self$get_FemaleCSVCon(),sep = "\n") # write header
    if(!is.null(private$MalePop)){
      MaleCSVFile = paste0(self$get_MosquitoDirectory(),"MaleMosquitoPop_Run",private$runID,".csv")
      self$set_MaleCSVCon(file(description = MaleCSVFile,open = "wt"))
      writeLines(text = paste0(c("time",private$MalePop$get_MBITES_PAR("stateSpace")),collapse = ","),con = self$get_MaleCSVCon(),sep = "\n") # write header
    }
  }

  if(verbose){
    cat("writing output to: \n",FemaleHistoryFile,"\n",MosquitoPathogenFile,"\n",HumanPathogenFile,"\n",sep="")
  }

  # set up JSON output
  writeLines(text = "[",con = self$get_FemaleHistoryCon())
  if(!is.null(private$MalePop)){writeLines(text = "[",con = self$get_MaleHistoryCon())}
  writeLines(text = "[",con = self$get_MosquitoPathogenCon())

  private$HumanPop$initialize_Pathogens(PfPAR)

  # progress bar
  progress_bar = txtProgressBar(min=1,max=tMax,style=3)

  # run simulation
  while(private$tNow < tMax){
    self$simMICRO_oneStep(trackPop = trackPop)
    setTxtProgressBar(progress_bar,private$tNow)
  }
  cat("\n")
  close(progress_bar)

  # write human JSON output
  # writeLines(text = jsonlite::toJSON(x = private$HumanPop$get_PathogensHistory(),pretty = TRUE),con = self$get_HumanPathogenCon())

  # finish writing JSON output
  cat(jsonlite::toJSON(x = mbitesGeneric_NULL,pretty = TRUE),"\n",sep="",file = self$get_FemaleHistoryCon()) # female EOF
  writeLines(text = "]",con = self$get_FemaleHistoryCon())
  if(!is.null(private$MalePop)){
    cat(jsonlite::toJSON(x = mbitesGeneric_NULL,pretty = TRUE),"\n",sep="",file = self$get_MaleHistoryCon()) # male EOF
    writeLines(text = "]",con = self$get_MaleHistoryCon())
  }
  cat(jsonlite::toJSON(x = PfSI_NULL,pretty = TRUE),"\n",sep="",file = self$get_MosquitoPathogenCon()) # male EOF
  writeLines(text = "]",con = self$get_MosquitoPathogenCon())

  # close connections
  cat("closing connections, finishing MICRO simulation runID: ",private$runID,"\n",sep="")
  self$close_FemaleHistoryCon()
  if(!is.null(private$MalePop)){self$close_MaleHistoryCon()}
  self$close_MosquitoPathogenCon()
  # self$close_HumanPathogenCon()
  if(trackPop){
    self$close_FemaleCSVCon()
    if(!is.null(private$MalePop)){self$close_MaleCSVCon()}
  }

  private$HumanPop$close_conPathogen()

}

# add to MicroTile
Tile$set(which = "public",name = "simMICRO_oneRun",
          value = simMICRO_oneRun,
          overwrite = TRUE
)


###############################################################################
# MICRO MicroTile Simulation: oneStep
###############################################################################

#' MICRO \code{\link{MicroTile}}: Run Simulation one Time Step
#'
#' Run MICRO simulation for one time step, the length of which defines the temporal window for indifference to contingent events.
#'
#' @param timeStep the time step of the model
#' @param clearInterval interval to clear population and track mosquito history (see \code{\link{clear_pop}})
#' @param popTrack log daily counts of mosquitoes in life stages or not (written to MicroTile$directory)
#' @param historyTrack output mosquito histories to JSON via \code{\link{MosquitoPopFemale_clear_pop}} every clearInterval days or not (written to MicroTile$directory)
#' @md
simMICRO_oneStep <- function(trackPop = FALSE){

  # human activity space simulation
  private$HumanPop$sim_ActivitySpace()

  # Aquatic Ecology
  private$Landscape$oneStep_AquaticEcology()
  private$Landscape$addCohort_AquaticEcology()

  # M-BITES
  if(!is.null(private$MalePop)){
    private$MalePop$MBITES()
  }
  private$FemalePop$MBITES()

  # human event queue simulation
  private$HumanPop$simHumans(tPause = private$tNow)

  # log population count
  if(trackPop){
    Fcount = table(unlist(private$FemalePop$get_pop()$apply(tag="get_state",returnVal=TRUE)))
    Fstate = private$FemalePop$get_MBITES_PAR("Fstate")
    Fstate[names(Fcount)] = Fcount
    writeLines(text = paste0(c(private$tNow,Fstate),collapse = ","),con = self$get_FemaleCSVCon(),sep = "\n")
    if(!is.null(private$MalePop)){
      Mcount = table(unlist(private$MalePop$get_pop()$apply(tag="get_state",returnVal=TRUE)))
      Mstate = private$MalePop$get_MBITES_PAR("Mstate")
      Mstate[names(Mcount)] = Mcount
      writeLines(text = paste0(c(private$tNow,Mstate),collapse = ","),con = self$get_MaleCSVCon(),sep = "\n")
    }
  }

  # update tNow
  private$tNow = private$tNow + 1

}

# add to MicroTile
Tile$set(which = "public",name = "simMICRO_oneStep",
          value = simMICRO_oneStep,
          overwrite = TRUE
)


###############################################################################
# Reset MosquitoPopFemale: Used to reset Tile between runs
###############################################################################

#' MICRO \code{\link{MicroTile}}: Re-initialize \code{\link{MosquitoPopFemale}}
#'
#' Erase the old \code{private$FemalePop} object in the tile and re-allocate.
#' This function is needed for running MBITES-Cohort models, themselves needed to parameterize EL4P Aquatic Ecology Module.
#'
#' @section How to use with fitting EL4P:
#'
#'  1. generate a list of parameters and set methods with \code{\link{MicroMosquitoPop.Setup}} with argument \code{cohort=TRUE}
#'  2. initialize a `MicroTile` as normal
#'  3. run the cohort simulation and output data
#'  4. use \code{\link{EL4P.Mesh.Fit}} or \code{\link{EL4P.Landscape.Fit}} to fit EL4P; see those functions for details
#'  5. make a new list of parameters and overwrite cohort methods with \code{\link{MicroMosquitoPop.Setup}} with argument \code{cohort=FALSE}
#'  6. use this method to re-allocate the females using the parameters and methods from step 4.
#'
#'  * This method is bound to \code{MicroTile$set_FemalePop}
#'
#' @param MosquitoPop_PAR output of parameter generation function for the chosen M-BITES module
#'
reset_FemalePop_Tile <- function(MosquitoPop_PAR){

  # clean population
  private$FemalePop = NULL
  gc()

  private$FemalePop = MosquitoPopFemale$new(
    N = MosquitoPop_PAR$N_female,  # number of female mosquitoes at initialization
    locNow_init = MosquitoPop_PAR$ix_female,  # landscape indices of female mosquitoes
    genotype_init = MosquitoPop_PAR$genotype_female,  # genotypes of females
    MBITES_PAR = MosquitoPop_PAR$MBITES_PAR_FEMALE  # M-BITES parameters
 )

  # Female Mosquito Population Pointers
  private$FemalePop$set_TilePointer(self)
  private$FemalePop$set_LandscapePointer(private$Landscape)
  private$FemalePop$set_HumansPointer(private$HumanPop)
  private$FemalePop$set_MalePopPointer(private$MalePop)

  private$FemalePop$get_pop()$apply(tag="set_FemalePopPointer",returnVal=FALSE,FemalePopPointer=private$FemalePop)
  private$FemalePop$get_pop()$apply(tag="set_MalePopPointer",returnVal=FALSE,MalePopPointer=private$MalePop)

  private$FemalePop$get_pop()$apply(tag="set_TilePointer",returnVal=FALSE,TilePointer=self)
  private$FemalePop$get_pop()$apply(tag="set_LandscapePointer",returnVal=FALSE,LandscapePointer=private$Landscape)
  private$FemalePop$get_pop()$apply(tag="set_HumansPointer",returnVal=FALSE,HumansPointer=private$HumanPop)

}

Tile$set(which = "public",name = "reset_FemalePop",
          value = reset_FemalePop_Tile,
          overwrite = TRUE
)

###############################################################################
# Reset MosquitoPopMale: Used to reset Tile between runs
###############################################################################

#' MICRO \code{\link{MicroTile}}: Re-initialize \code{\link{MosquitoPopMale}}
#'
#' Erase the old \code{private$FemalePop} object in the tile and re-allocate.
#' This function is needed for running MBITES-Cohort models, themselves needed to parameterize EL4P Aquatic Ecology Module.
#'
#' @section How to use with fitting EL4P:
#'
#'  1. generate a list of parameters and set methods with \code{\link{MicroMosquitoPop.Setup}} with argument \code{cohort=TRUE}
#'  2. initialize a `MicroTile` as normal
#'  3. run the cohort simulation and output data
#'  4. use \code{\link{EL4P.Mesh.Fit}} or \code{\link{EL4P.Landscape.Fit}} to fit EL4P; see those functions for details
#'  5. make a new list of parameters and overwrite cohort methods with \code{\link{MicroMosquitoPop.Setup}} with argument \code{cohort=FALSE}
#'  6. use this method to re-allocate the females using the parameters and methods from step 4.
#'
#'  * This method is bound to \code{MicroTile$set_FemalePop}
#'
#' @param MosquitoPop_PAR output of parameter generation function for the chosen M-BITES module
#'
reset_MalePop_Tile <- function(MosquitoPop_PAR){

  # clean population
  private$MalePop = NULL
  gc()

  private$MalePop = MosquitoPopMale$new(
    N = MosquitoPop_PAR$N_male,
    locNow_init = MosquitoPop_PAR$ix_male,
    genotype_init = MosquitoPop_PAR$genotype_male,
    MBITES_PAR = MosquitoPop_PAR$MBITES_PAR_MALE
 )

  # Female Mosquito Population Pointers
  private$MalePop$set_TilePointer(self)
  private$MalePop$set_LandscapePointer(private$Landscape)
  private$MalePop$set_HumansPointer(private$HumanPop)
  private$MalePop$set_FemalePopPointer(private$FemalePop)

  private$MalePop$get_pop()$apply(tag="set_FemalePopPointer",returnVal=FALSE,FemalePopPointer=private$FemalePop)
  private$MalePop$get_pop()$apply(tag="set_MalePopPointer",returnVal=FALSE,MalePopPointer=private$MalePop)

  private$MalePop$get_pop()$apply(tag="set_TilePointer",returnVal=FALSE,TilePointer=self)
  private$MalePop$get_pop()$apply(tag="set_LandscapePointer",returnVal=FALSE,LandscapePointer=private$Landscape)
  private$MalePop$get_pop()$apply(tag="set_HumansPointer",returnVal=FALSE,HumansPointer=private$HumanPop)

}

Tile$set(which = "public",name = "reset_MalePop",
          value = reset_MalePop_Tile,
          overwrite = TRUE
)


###############################################################################
# Set HumanPop: Used to reset Tile between runs
###############################################################################

#' MICRO \code{\link{MicroTile}}: Re-initialize \code{\link[MASHmacro]{HumanPop}}
#'
#' write me! used to reset humanpop between simulation runs
#'
#' @param HumanPop_PAR from \code{\link[MASHmacro]{HumanPop.Parameters}}
#'
reset_HumanPop_Tile <- function(HumanPop_PAR){

  # clear old HumanPop
  # private$HumanPop$get_pop()$rmAll()
  # private$HumanPop = NULL
  # gc()

  # generate human object
  # private$HumanPop = MASHmacro::HumanPop$new(patchID = 1L,HumanPop_PAR)
  private$HumanPop$reset(HumanPop_PAR)

  # Human & HumanPop Pointers
  private$HumanPop$set_TilePointer(self)
  private$HumanPop$set_LandscapePointer(private$Landscape)
  private$HumanPop$set_FemalePopPointer(private$FemalePop)

  private$HumanPop$get_pop()$apply(tag="set_TilePointer",returnVal=FALSE,TilePointer=self)
  private$HumanPop$get_pop()$apply(tag="set_HumansPointer",returnVal=FALSE,HumansPointer=private$HumanPop)
  private$HumanPop$get_pop()$apply(tag="set_LandscapePointer",returnVal=FALSE,LandscapePointer=private$Landscape)
  private$HumanPop$get_pop()$apply(tag="set_FemalePopPointer",returnVal=FALSE,FemalePopPointer=private$FemalePop)

}

Tile$set(which = "public",name = "reset_HumanPop",
          value = reset_HumanPop_Tile,
          overwrite = TRUE
)


###############################################################################
# Reset Tile
###############################################################################

#' Reset Tile Between Runs
#'
#' write me
#'
#'    * This method is bound to \code{Tile$resetMicro}
#'
resetMicro <- function(MosquitoPar, HumanPar, EL4P = FALSE, mating = FALSE){

  # reset mosquitoes
  self$reset_FemalePop(MosquitoPar)
  if(!is.null(private$MalePop)){
    self$reset_MalePop(MosquitoPar)
  }

  # reset humans
  self$reset_HumanPop(HumanPar)

  # reset landscape
  private$Landscape$clear_ImagoQ()
  if(EL4P){
    private$Landscape$clear_EggQ()
    cat("need to write clear_EL4P\n")
  }
  if(mating){
    private$Landscape$clear_MatingQ()
  }

  # reset landscape pointers
  private$Landscape$set_TilePointer(self)
  private$Landscape$set_HumansPointer(private$HumanPop)
  private$Landscape$set_FemalePopPointer(private$FemalePop)
  private$Landscape$set_MalePopPointer(private$MalePop)

  # reset tile
  private$tNow = private$tStart
  # runID updates in the simMICRO_oneRun function (move later)
}

Tile$set(which = "public",name = "resetMicro",
          value = resetMicro, overwrite = TRUE
)
