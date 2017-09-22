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
                     private$movementFemale = MicroKernel_exactAll(private$Landscape,male=FALSE,sigma=3,eps=0.1,beta=0)

                     # generate female mosquito object
                     private$FemalePop = MosquitoPopFemale$new(
                       N = MosquitoPop_PAR$N_female,  # number of female mosquitoes at initialization
                       ix_init = MosquitoPop_PAR$ix_female,  # landscape indices of female mosquitoes
                       genotype_init = MosquitoPop_PAR$genotype_female,  # genotypes of females
                       MBITES_PAR = MosquitoPop_PAR$MBITES_PAR_FEMALE  # M-BITES parameters
                    )

                    # generate male mosquito object
                    if(!is.null(MosquitoPop_PAR$N_male)){
                      private$movementMale = MicroKernel_exactAll(private$Landscape,male=TRUE,sigma=3,eps=0.1,beta=0)

                      private$MalePop = MosquitoPopMale$new(
                        N = MosquitoPop_PAR$N_male,
                        ix_init = MosquitoPop_PAR$ix_male,
                        genotype_init = MosquitoPop_PAR$genotype_male,
                        MBITES_PAR = MosquitoPop_PAR$MBITES_PAR_MALE
                      )
                    }

                     #################################################
                     # Set Pointers
                     #################################################

                     # Human & HumanPop Pointers
                     private$HumanPop$set_TilePointer(self)
                     private$HumanPop$set_LandscapePointer(private$Landscape)
                     private$HumanPop$set_FemalePopPointer(private$FemalePop)

                     private$HumanPop$get_pop()$apply(tag="set_TilePointer",returnVal=FALSE,TilePointer=self)
                     private$HumanPop$get_pop()$apply(tag="set_HumansPointer",returnVal=FALSE,HumansPointer=private$HumanPop)
                     private$HumanPop$get_pop()$apply(tag="set_LandscapePointer",returnVal=FALSE,LandscapePointer=private$Landscape)
                     private$HumanPop$get_pop()$apply(tag="set_FemalePopPointer",returnVal=FALSE,FemalePopPointer=private$FemalePop)

                     # Landscape Pointers
                     private$Landscape$set_TilePointer(self)
                     private$Landscape$set_HumansPointer(private$HumanPop)
                     private$Landscape$set_FemalePopPointer(private$FemalePop)
                     private$Landscape$set_MalePopPointer(private$MalePop)

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

                    #  Male Mosquito Population Pointers
                    if(!is.null(MosquitoPop_PAR$N_male)){
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

                     mosyFiles = list.files(path = private$MosquitoDirectory)
                     if(length(mosyFiles)>0){
                       cat("removing files: ",mosyFiles,"\n",sep="")
                       file.remove(paste0(private$MosquitoDirectory,mosyFiles))
                     }

                     if(!dir.exists(paste0(directory,"HUMAN/"))){
                       dir.create(paste0(directory,"HUMAN/"))
                     }
                     private$HumanDirectory = paste0(directory,"HUMAN/")

                     humanFiles = list.files(path = private$HumanDirectory)
                     if(length(humanFiles)>0){
                       cat("removing files: ",humanFiles,"\n",sep="")
                       file.remove(paste0(private$HumanDirectory,humanFiles))
                     }

                     cat("output will be written to: ",directory,"\n human output will be in: ",directory,"HUMAN/\n mosquito output will be in: ",directory,"MOSQUITO/\n",sep="")

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

                   # human output
                   HumanPathogenCon = NULL,

                   # track populations each day
                   FemaleCSVCon = NULL,
                   MaleCSVCon = NULL,

                   # write individual histories to JSON
                   FemaleHistoryCon = NULL,
                   MaleHistoryCon = NULL,

                   # write mosquito stage pathogen objects to JSON
                   MosquitoPathogenCon = NULL

                 )
)
