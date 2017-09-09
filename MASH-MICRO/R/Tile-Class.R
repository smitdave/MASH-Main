###############################################################################
#
#       __  ____               _______ __
#      /  |/  (_)_____________/_  __(_) /__
#     / /|_/ / / ___/ ___/ __ \/ / / / / _ \
#    / /  / / / /__/ /  / /_/ / / / / /  __/
#   /_/  /_/_/\___/_/   \____/_/ /_/_/\___/
#
#   MASH-MICRO
#   MICRO: Tile Class Definition
#   MASH-MICRO Team
#   September 6, 2017
#
###############################################################################


#' Tile Class Definition
#'
#' im a class!
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: im an agument!
#'
#' @section **Methods**:
#'  * method: im a method!
#'
#' @section **Fields**:
#'  * field: im a field!
#'
#' @export
Tile <- R6::R6Class(classname = "Tile",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   #################################################
                   # Initialize
                   #################################################

                   # Landscape_PAR: Landscape.Parameters (MICRO-Landscape-Parameters.R)
                   # HumanPop_PAR: HumanPop.Parameters (HUMANS-HumanPop-Parameters.R)
                   # MosquitoPop_PAR: MicroMosquitoPop.Setup (MOSQUITO-MosquitoPop-Parameters.R)
                   # directory: directory to write data to (ex: "/Users/slwu89/Desktop/mash.out/")
                   initialize = function(Landscape_PAR, HumanPop_PAR, MosquitoPop_PAR, directory){

                     #################################################
                     # Set Objects
                     #################################################

                     # set simulation time
                     private$tNow = 1

                     # generate landscape object
                     private$Landscape = Landscape$new(Landscape_PAR)

                     # generate human object
                     private$HumanPop = HumanPop$new(HumanPop_PAR)

                     # generate movement object
                     private$movementFemale = MicroKernel_exactAll(private$Landscape,sigma=3,eps=0.1,beta=0)

                     # generate female mosquito object
                     private$FemalePop = MosquitoPopFemale$new(N = MosquitoPop_PAR$N_female,  # number of female mosquitoes at initialization
                                                                  time_init = MosquitoPop_PAR$time,  # time simulation begins
                                                                  ix_init = MosquitoPop_PAR$ix_female,  # landscape indices of female mosquitoes
                                                                  genotype_init = MosquitoPop_PAR$genotype_female,  # genotypes of females
                                                                  MBITES_PAR = MosquitoPop_PAR$MBITES_PAR,  # M-BITES parameters
                                                                  module = MosquitoPop_PAR$module)  # M-BITES module

                     # initialize female mosquito Pathogen object field
                     private$FemalePop$init_Pathogens()

                     #################################################
                     # Set Pointers
                     #################################################

                     # Human & HumanPop Pointers (duplicate for Humans in HumanPop$pop)
                     private$HumanPop$set_TilePointer(self)
                     private$HumanPop$set_LandscapePointer(private$Landscape)
                     private$HumanPop$set_FemalePopPointer(private$FemalePop)

                     for(ixH in 1:private$HumanPop$nHumans){
                       private$HumanPop$get_Human(ixH)$set_TilePointer(self)
                       private$HumanPop$get_Human(ixH)$set_LandscapePointer(private$Landscape)
                       private$HumanPop$get_Human(ixH)$set_FemalePopPointer(private$FemalePop)
                     }

                     # Landscape Pointers
                     private$Landscape$set_TilePointer(self)
                     private$Landscape$set_HumansPointer(private$HumanPop)
                     private$Landscape$set_FemalePopPointer(private$FemalePop)

                     # Female Mosquito Population Pointers
                     private$FemalePop$set_TilePointer(self)
                     private$FemalePop$set_LandscapePointer(private$Landscape)
                     private$FemalePop$set_HumansPointer(private$HumanPop)

                     for(ixM in private$FemalePop$which_alive()){
                       private$FemalePop$get_MosquitoIxM(ixM)$set_TilePointer(self)
                       private$FemalePop$get_MosquitoIxM(ixM)$set_LandscapePointer(private$Landscape)
                       private$FemalePop$get_MosquitoIxM(ixM)$set_HumansPointer(private$HumanPop)
                     }

                     #################################################
                     # Set Output Directory
                     #################################################

                     # set output directory
                     private$directory = directory

                     if(!dir.exists(paste0(directory))){
                       dir.create(paste0(directory))
                     }

                     if(!dir.exists(paste0(directory,"MOSQUITO/"))){
                       dir.create(paste0(directory,"MOSQUITO/"))
                     }
                     if(!dir.exists(paste0(directory,"HUMAN/"))){
                       dir.create(paste0(directory,"HUMAN/"))
                     }


                   }

                 ),

                 # private members
                 private = list(

                   # Tile level fields
                   tNow = numeric(1),
                   Landscape_PAR = list(),
                   HumanPop_PAR = list(),
                   movementFemale = list(),
                   movementMale = list(),
                   directory = character(1),

                   # objects interacting on a tile
                   HumanPop = NULL,
                   Landscape = NULL,
                   FemalePop = NULL,
                   MalePop = NULL,

                   # simulation output and logging
                   runID = integer(1),
                   FemaleCSVCon = NULL,
                   MaleCSVCon = NULL,
                   FemaleHistoryCon = NULL,
                   MaleHistoryCon = NULL

                 )
)
