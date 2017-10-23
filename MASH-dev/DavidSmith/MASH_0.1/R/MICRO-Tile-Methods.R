#################################################################
#
#   MASH
#   MICRO Tile
#   Microsimulation 'MicroTile' MICRO Simulation Method
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   August 1, 2017
#
#################################################################

#################################################################
# MICRO MicroTile Simulation
#################################################################

#' MICRO \code{\link{MicroTile}}: Run Simulation one Time Step
#'
#' Run MICRO simulation for one time step, the length of which defines the temporal window for indifference to contingent events.
#'
#' @param timeStep the time step of the model
#' @param print print current iteration (disable for running in batch mode)
#' @param logInterval interval to clear population and track mosquito history (see \code{\link{clear_pop}})
#' @md
simMICRO_oneStep <- function(timeStep = 1, print = FALSE, logInterval = 10){

  if(print){
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
  if(private$tNow %% logInterval == 0){
    private$FemalePop$clear_pop(historyTrack = TRUE)
  }

  # update tNow
  private$tNow = private$tNow + timeStep


}

#################################################################
# Set Methods
#################################################################

MicroTile$set(which = "public",name = "simMICRO_oneStep",
          value = simMICRO_oneStep,
          overwrite = TRUE
)


#################################################################
# MICRO MicroTile Set MicroMosquitoPopFemale
# This is needed for running DHM-Cohort:
#   1. use MicroMosquitoPop.Setup with cohort=TRUE
#   2. initialize the MicroTile as per normal
#   3. run the cohort sim and output data
#   4. make new parameters with cohort = FALSE
#   5. use this function to make the mosquitoes for full sim
#################################################################

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

MicroTile$set(which = "public",name = "set_FemalePop",
          value = set_FemalePop_MicroTile,
          overwrite = TRUE
)

#################################################################
# Set EL4P (used after running EL4P fitting routines)
#################################################################
