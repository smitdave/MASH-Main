#################################################################
#
#   MASH
#   MICRO Tile
#   Microsimulation 'MicroTile' Class Definition
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   May 9, 2017
#
#################################################################

#################################################################
# Patch Definition
#################################################################

#' MICRO Tile Class Definition
#'
#' This is a generic MICRO microsimulation tile blah blah ...
#' I talk about something here
#' * somewhere 1
#' * somewhere 2
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section Methods:
#'  * **Constructor**
#'    * new: initialize a new \code{MicroTile} object
#'      * Arguments:
#'        * \code{MicroTile_PAR}: see \code{\link{MICRO.Tile.Parameters}} for parameter generation function.
#'  * **Getters & Setters**
#'    * get_tNow:
#'    * set_tNow:
#'    * get_MicroTile_PAR:
#'    * set_MicroTile_PAR:
#'    * get_HumanPop:
#'    * get_Landscape:
#'    * get_FemalePop:
#'    * get_MalePop:
#'
#'
#'
#'
#'
#' @md
#' @export
MicroTile <- R6::R6Class(classname = "MicroTile",
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
                     private$FemalePop = MicroMosquitoPopFemale$new(N = MosquitoPop_PAR$N_female,  # number of female mosquitoes at initialization
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
                       if(!dir.exists(paste0(directory,"MOSQUITO/"))){
                         dir.create(paste0(directory,"MOSQUITO/"))
                       }
                       if(!dir.exists(paste0(directory,"HUMAN/"))){
                         dir.create(paste0(directory,"HUMAN/"))
                       }
                     }

                   },

                   #################################################################
                   # Getters & Setters
                   #################################################################

                   get_tNow = function(){return(private$tNow)},
                   set_tNow = function(tNow){private$tNow = tNow},

                   get_HumanPop = function(){return(private$HumanPop)},

                   get_Landscape = function(){return(private$Landscape)},

                   get_FemalePop = function(){return(private$FemalePop)},

                   get_MalePop = function(){return(private$MalePop)},

                   get_directory = function(){return(private$directory)},
                   set_directory = function(directory){private$directory = directory}

                 ),

                 # private members
                 private = list(

                   # Tile level fields
                   tNow = NULL,
                   Landscape_PAR = NULL,
                   HumanPop_PAR = NULL,
                   movementFemale = NULL,
                   movementMale = NULL,
                   directory = NULL,

                   # objects interacting on a tile
                   HumanPop = NULL,
                   Landscape = NULL,
                   FemalePop = NULL,
                   MalePop = NULL

                 )
)
