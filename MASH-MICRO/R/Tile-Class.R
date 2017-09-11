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
                     private$HumanPop = MASHmacro::HumanPop$new(patchID = 1L,HumanPop_PAR)

                     # generate movement object
                     private$movementFemale = MicroKernel_exactAll(private$Landscape,sigma=3,eps=0.1,beta=0)

                     # generate female mosquito object
                     private$FemalePop = MosquitoPopFemale$new(N = MosquitoPop_PAR$N_female,  # number of female mosquitoes at initialization
                                                                  ix_init = MosquitoPop_PAR$ix_female,  # landscape indices of female mosquitoes
                                                                  genotype_init = MosquitoPop_PAR$genotype_female,  # genotypes of females
                                                                  MBITES_PAR = MosquitoPop_PAR$MBITES_PAR  # M-BITES parameters
                                                                )

                     #################################################
                     # Set Pointers
                     #################################################

                     # Human & HumanPop Pointers
                     private$HumanPop$set_TilePointer(self)
                     private$HumanPop$set_LandscapePointer(private$Landscape)
                     private$HumanPop$set_FemalePopPointer(private$FemalePop)

                     private$HumanPop$get_pop()$apply(tag="set_TilePointer",returnVal=FALSE,TilePointer=self)
                     private$HumanPop$get_pop()$apply(tag="set_LandscapePointer",returnVal=FALSE,LandscapePointer=private$Landscape)
                     private$HumanPop$get_pop()$apply(tag="set_FemalePopPointer",returnVal=FALSE,FemalePopPointer=private$FemalePop)

                     # Landscape Pointers
                     private$Landscape$set_TilePointer(self)
                     private$Landscape$set_HumansPointer(private$HumanPop)
                     private$Landscape$set_FemalePopPointer(private$FemalePop)

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
                     private$MosquitoDirectory = paste0(directory,"MOSQUITO/")

                     if(!dir.exists(paste0(directory,"HUMAN/"))){
                       dir.create(paste0(directory,"HUMAN/"))
                     }
                     private$HumanDirectory = paste0(directory,"HUMAN/")

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

                   # objects interacting on a tile
                   HumanPop = NULL,
                   Landscape = NULL,
                   FemalePop = NULL,
                   MalePop = NULL,

                   # simulation output and logging
                   runID = integer(1),
                   directory = character(1),
                   MosquitoDirectory = character(1),
                   HumanDirectory = character(1),
                   FemaleCSVCon = NULL,
                   MaleCSVCon = NULL,
                   FemaleHistoryCon = NULL,
                   MaleHistoryCon = NULL

                 )
)
