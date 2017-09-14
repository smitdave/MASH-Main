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
#' @param verbose print information
#'
simMICRO_oneRun <- function(tMax, verbose = FALSE, trackPop = FALSE){

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

  HumanPathogenFile = paste0(self$get_HumanDirectory(),"HumanPathogens_Run",private$runID,".json")
  self$set_HumanPathogenCon(file(description = HumanPathogenFile,open = "wt"))

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

  # run simulation
  while(private$tNow < tMax){
    self$simMICRO_oneStep(verbose = verbose,trackPop = trackPop)
  }

  # write human JSON output
  writeLines(text = jsonlite::toJSON(x = private$HumanPop$get_PathogensHistory(),pretty = TRUE),con = self$get_HumanPathogenCon())

  # finish writing JSON output
  writeLines(text = "]",con = self$get_FemaleHistoryCon())
  if(!is.null(private$MalePop)){writeLines(text = "]",con = self$get_MaleHistoryCon())}
  writeLines(text = "]",con = self$get_MosquitoPathogenCon())

  # close connections
  cat("closing connections, finishing MICRO simulation runID: ",private$runID,"\n",sep="")
  self$close_FemaleHistoryCon()
  if(!is.null(private$MalePop)){self$close_MaleHistoryCon()}
  self$close_MosquitoPathogenCon()
  self$close_HumanPathogenCon()
  if(trackPop){
    self$close_FemaleCSVCon()
    if(!is.null(private$MalePop)){self$close_MaleCSVCon()}
  }

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
#' @param verbose current iteration (disable for running in batch mode)
#' @param clearInterval interval to clear population and track mosquito history (see \code{\link{clear_pop}})
#' @param popTrack log daily counts of mosquitoes in life stages or not (written to MicroTile$directory)
#' @param historyTrack output mosquito histories to JSON via \code{\link{MosquitoPopFemale_clear_pop}} every clearInterval days or not (written to MicroTile$directory)
#' @md
simMICRO_oneStep <- function(verbose = FALSE, trackPop = FALSE){

  if(verbose){
    print(paste0("time step: ",private$tNow))
  }

  # human activity space simulation
  private$HumanPop$sim_ActivitySpace()

  # Aquatic Ecology
  private$Landscape$oneStep_AquaticEcology()
  private$Landscape$addCohort_AquaticEcology()

  # M-BITES
  private$FemalePop$MBITES()
  if(!is.null(private$MalePop)){
    private$MalePop$MBITES()
  }

  # human event queue simulation
  private$HumanPop$simHumans(tPause = private$tNow)

  # log population count
  if(trackPop){
    # write pops to csv (THIS IS PROBABLY INEFFICIENT; SET UP)
    Fcount = table(unlist(private$FemalePop$get_pop()$apply(tag="get_state",returnVal=TRUE)))
    Fstate = private$FemalePop$get_MBITES_PAR("Fstate")
    # Fstate = setNames(object = rep(0,length(private$FemalePop$get_MBITES_PAR("stateSpace"))),nm = private$FemalePop$get_MBITES_PAR("stateSpace"))
    Fstate[names(Fcount)] = Fcount
    writeLines(text = paste0(c(private$tNow,Fstate),collapse = ","),con = self$get_FemaleCSVCon(),sep = "\n")
    if(!is.null(private$MalePop)){
      # do something here
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
# Set MosquitoPopFemale: Used to reset Tile between runs
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
  private$FemalePop$get_pop()$rmAll()
  gc()

  private$FemalePop = MosquitoPopFemale$new(N = MosquitoPop_PAR$N_female,  # number of female mosquitoes at initialization
                                               ix_init = MosquitoPop_PAR$ix_female,  # landscape indices of female mosquitoes
                                               genotype_init = MosquitoPop_PAR$genotype_female,  # genotypes of females
                                               MBITES_PAR = MosquitoPop_PAR$MBITES_PAR  # M-BITES parameters
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
  private$HumanPop = NULL
  gc()

  # generate human object
  private$HumanPop = MASHmacro::HumanPop$new(patchID = 1L,HumanPop_PAR)

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
