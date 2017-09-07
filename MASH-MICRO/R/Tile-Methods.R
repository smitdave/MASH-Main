###############################################################################
#
#       __  ____               _______ __
#      /  |/  (_)_____________/_  __(_) /__
#     / /|_/ / / ___/ ___/ __ \/ / / / / _ \
#    / /  / / / /__/ /  / /_/ / / / / /  __/
#   /_/  /_/_/\___/_/   \____/_/ /_/_/\___/
#
#   MASH-MICRO
#   MICRO: Tile Class Methods
#   MASH-MICRO Team
#   September 6, 2017
#
###############################################################################


###############################################################################
# MICRO MicroTile Simulation
###############################################################################

#' MICRO \code{\link{MicroTile}}: One Simulation Run
#'
#' write me
#'
#' @param tMax length of simulation run
#' @param runID unique simulation run identifier
#' @param verbose print information
#' @param timeStep the time step of the model
#' @param clearInterval interval to clear population and track mosquito history (see \code{\link{clear_pop}})
#' @param popTrack log daily counts of mosquitoes in life stages or not (written to MicroTile$directory)
#' @param historyTrack output mosquito histories to JSON via \code{\link{MicroMosquitoPopFemale_clear_pop}} every clearInterval days or not (written to MicroTile$directory)
#' @md
simMICRO_oneRun <- function(tMax, runID, verbose = FALSE, timeStep = 1, clearInterval = 10, popTrack = FALSE, historyTrack = FALSE){

  if(!is.null(private$runID) & private$runID == runID){stop("please choose unique runID for new simuation run")}

  # set up text connections
  if(popTrack){
    # set up female csv out
    private$FemaleCSVCon = file(description = paste0(private$directory,"MOSQUITO/popDynamicsF_Run",runID,".csv"),open = "wt")
    writeLines(text = paste0(c("time",private$FemalePop$get_MBITES_PAR("stateSpace")),collapse=","),con = private$FemaleCSVCon,sep="\n") # write header
    # set up male csv out
    if(!is.null(private$MalePop)){
      print(paste0("male CSV out not coded yet"))
      # private$MaleCSVCon =
    }
  }

  while(private$tNow < tMax){
    self$simMICRO_oneStep(timeStep = timeStep,verbose = verbose,clearInterval = clearInterval,popTrack = popTrack,historyTrack = historyTrack)
  }

  # close text connections
  if(popTrack){
    self$close_con()
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
#' @param historyTrack output mosquito histories to JSON via \code{\link{MicroMosquitoPopFemale_clear_pop}} every clearInterval days or not (written to MicroTile$directory)
#' @md
simMICRO_oneStep <- function(timeStep = 1, verbose = FALSE, clearInterval = 10, popTrack = FALSE, historyTrack = TRUE){

  if(verbose){
    print(paste0("Current tNow: ",private$tNow))
  }

  # human activity space simulation
  private$HumanPop$sim_ActivitySpace()

  # Aquatic Ecology
  private$Landscape$oneStep_AquaticEcology() # manage lambda -> ImagoQ (emerge) or EggQ -> ImagoQ (EL4P)
  private$Landscape$addCohort() # emerging adults from ImagoQ to MicroMosquitoPopFemale

  # M-BITES
  private$FemalePop$MBITES()
  if(!is.null(private$MalePop)){
    private$MalePop$MBITES()
  }

  # human event queue simulation
  private$HumanPop$simHumans(tPause = private$tNow)

  # clear mosquito pop and track output
  if(private$tNow %% clearInterval == 0){
    private$FemalePop$clear_pop(historyTrack = historyTrack)
  }

  # log count data
  if(popTrack){
    private$FemalePop$track_pop()
    if(!is.null(private$MalePop)){private$MalePop$track_pop()}
  }

  # update tNow
  private$tNow = private$tNow + timeStep

}

# add to MicroTile
Tile$set(which = "public",name = "simMICRO_oneStep",
          value = simMICRO_oneStep,
          overwrite = TRUE
)


###############################################################################
# MICRO MicroTile Set MicroMosquitoPopFemale
# This is needed for running DHM-Cohort:
#   1. use MicroMosquitoPop.Setup with cohort=TRUE
#   2. initialize the MicroTile as per normal
#   3. run the cohort sim and output data
#   4. make new parameters with cohort = FALSE
#   5. use this function to make the mosquitoes for full sim
###############################################################################

#' MICRO \code{\link{MicroTile}}: Re-initialize \code{\link{MicroMosquitoPopFemale}}
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
#' @param MosquitoPop_PAR output of \code{\link{MicroMosquitoPop.Setup}}
#' @md
set_FemalePop_MicroTile <- function(MosquitoPop_PAR){

  # clear old FemalePop
  private$FemalePop = NULL

  # generate female mosquito object
  private$FemalePop = MicroMosquitoPopFemale$new(N = MosquitoPop_PAR$N_female,  # number of female mosquitoes at initialization
                                               time_init = MosquitoPop_PAR$time,  # time simulation begins
                                               ix_init = MosquitoPop_PAR$ix_female,  # landscape indices of female mosquitoes
                                               genotype_init = MosquitoPop_PAR$genotype_female,  # genotypes of females
                                               MBITES_PAR = MosquitoPop_PAR$MBITES_PAR,  # M-BITES parameters
                                               module = MosquitoPop_PAR$module)  # M-BITES module

  # initialize female mosquito Pathogen object field
  private$FemalePop$init_Pathogens()

  # Female Mosquito Population Pointers
  private$FemalePop$set_TilePointer(self)
  private$FemalePop$set_LandscapePointer(private$Landscape)
  private$FemalePop$set_HumansPointer(private$HumanPop)
  private$FemalePop$set_MalePopPointer(private$MalePop)

  for(ixM in private$FemalePop$which_alive()){
    private$FemalePop$get_MosquitoIxM(ixM)$set_TilePointer(self)
    private$FemalePop$get_MosquitoIxM(ixM)$set_LandscapePointer(private$Landscape)
    private$FemalePop$get_MosquitoIxM(ixM)$set_HumansPointer(private$HumanPop)
    private$FemalePop$get_MosquitoIxM(ixM)$set_MalePopPointer(private$MalePop)
  }

  # reset pointers

  # Human & HumanPop Pointers (duplicate for Humans in HumanPop$pop)
  private$HumanPop$set_FemalePopPointer(private$FemalePop)
  for(ixH in 1:private$HumanPop$nHumans){
    private$HumanPop$get_Human(ixH)$set_FemalePopPointer(private$FemalePop)
  }

  # Landscape Pointers
  private$Landscape$set_FemalePopPointer(private$FemalePop)

  # garbage collection
  invisible(gc())
}

Tile$set(which = "public",name = "set_FemalePop",
          value = set_FemalePop_MicroTile,
          overwrite = TRUE
)

# #################################################################
# # Set HumanPop: Used to reset MicroTile in between simulation runs
# #################################################################
#
# #' MICRO \code{\link{MicroTile}}: Re-initialize \code{\link{HumanPop}}
# #'
# #' write me! used to reset humanpop between simulation runs
# #'
# #' @param timeStep the time step of the model
# #' @param verbose current iteration (disable for running in batch mode)
# #' @param clearInterval interval to clear population and track mosquito history (see \code{\link{clear_pop}})
# #' @param popTrack log daily counts of mosquitoes in life stages or not (written to MicroTile$directory)
# #' @param historyTrack output mosquito histories to JSON via \code{\link{MicroMosquitoPopFemale_clear_pop}} every clearInterval days or not (written to MicroTile$directory)
# #' @md
# reset_MicroTile <- function(HumanPop_PAR){
#
#   # clear old HumanPop
#   private$HumanPop = NULL
#
#   # generate human object
#   private$HumanPop = HumanPop$new(HumanPop_PAR)
#
#   # initialize Human pointers
#    private$HumanPop$set_TilePointer(self)
#    private$HumanPop$set_LandscapePointer(private$Landscape)
#    private$HumanPop$set_FemalePopPointer(private$FemalePop)
#
#    for(ixH in 1:private$HumanPop$nHumans){
#      private$HumanPop$get_Human(ixH)$set_TilePointer(self)
#      private$HumanPop$get_Human(ixH)$set_LandscapePointer(private$Landscape)
#      private$HumanPop$get_Human(ixH)$set_FemalePopPointer(private$FemalePop)
#    }
#
# }
