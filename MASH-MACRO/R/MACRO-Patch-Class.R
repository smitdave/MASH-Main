###############################################################################
#       __  ___                      ____        __       __
#      /  |/  /___ _______________  / __ \____ _/ /______/ /_
#     / /|_/ / __ `/ ___/ ___/ __ \/ /_/ / __ `/ __/ ___/ __ \
#    / /  / / /_/ / /__/ /  / /_/ / ____/ /_/ / /_/ /__/ / / /
#   /_/  /_/\__,_/\___/_/   \____/_/    \__,_/\__/\___/_/ /_/
#
#   MASH-MACRO
#   MACRO: MacroPatch Class Definition
#   David Smith, Hector Sanchez, Sean Wu
#   August 18, 2017
#
###############################################################################

#' MASH-MACRO MacroPatch Class Definition
#'
#' Generate a single well-mixed MacroPatch, which can run classical models in standalone mode or be linked in a network of
#' other patches in a \code{\link{MacroTile}}
#' Each instance of a \code{Human} lives in a \code{\link{HumanPop}}
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * patchID: integer ID of this patch
#'  * patchID: integer ID of this patch
#'  * patchID: integer ID of this patch
#'  * aquaModule: character string giving the MACRO Aquatic Ecology module to use; "MosquitoRM" initializes \code{\link{ELP}}
#'  * mosquitoModule: character string giving the MACRO mosquito module to use; "MosquitoRM" initializes \code{\link{MosquitoRM}}
#'  * mosquitoPars: named list giving parameters passed to the mosquito class constructor; "MosquitoRM" uses \code{\link{MosquitoRM.Parameters}}
#'  * humanPars: named list giving parameters passed to the \code{\link{HumanPop}} class constructor
#'  * bWeightZoo = 0: biting weight on livestock
#'  * tStart = 1: time to start all simulations
#'
#'  * directory: character string giving directory to write output
#'
#'
#'
#'
#' @section **Methods**:
#'  * get_bWeightHuman: see \code{\link{get_bWeightHuman_MacroPatch}}
#'  * set_bWeightHuman: see \code{\link{set_bWeightHuman_MacroPatch}}
#'  * accumulate_bWeightHuman: see \code{\link{accumulate_bWeightHuman_MacroPatch}}
#'  * get_bWeightZoo: see \code{\link{get_bWeightZoo_MacroPatch}}
#'  * set_bWeightZoo: see \code{\link{set_bWeightZoo_MacroPatch}}
#'  * get_kappa: see \code{\link{get_kappa_MacroPatch}}
#'  * set_kappa: see \code{\link{set_kappa_MacroPatch}}
#'  * accumulate_kappa: see \code{\link{accumulate_kappa_MacroPatch}}
#'  * get_MosquitoPop: see \code{\link{get_MosquitoPop_MacroPatch}}
#'  * set_MosquitoPop: see \code{\link{set_MosquitoPop_MacroPatch}}
#'  * get_HumanPop: see \code{\link{get_HumanPop_MacroPatch}}
#'  * set_MosquitoPop: see \code{\link{set_HumanPop_MacroPatch}}
#'  * get_TilePointer: see \code{\link{get_TilePointer_MacroPatch}}
#'  * set_TilePointer: see \code{\link{set_TilePointer_MacroPatch}}
#'
#' @section **Fields**:
#'  * patchID: integer ID of this patch
#'  * tStart: time to start simulation
#'  * tNow: current time of simulation
#'
#'  * directory: character string giving directory to write output
#'
#'
#'
#'
#'
#'
#' @md
#' @export
MacroPatch <- R6::R6Class(classname = "MacroPatch",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public methods & fields
                 public = list(

                   #################################################
                   # Constructor
                   #################################################

                   initialize = function(patchID, aquaModule = "ELP", mosquitoModule = "MosquitoRM", aquaPars, mosquitoPars, humanPars,  bWeightZoo = 0, tStart = 1, directory){

                     # Patch Fields
                     private$patchID = patchID
                     private$tStart = tStart
                     private$tNow = tStart

                     # Aquatic Ecology
                     switch(aquaModule,
                       ELP = {
                         private$ImagoQ = MASHcpp::ImagoQ()
                         private$ELP = MASHcpp::ELP(aquaPars$alpha,aquaPars$gamma,aquaPars$psi,aquaPars$sigma)
                        },
                       {stop("unrecognized entry for 'aquaModule'")}
                      )

                     # MACRO Mosquito
                     switch(mosquitoModule,
                       MosquitoRM = {private$MosquitoPop = MosquitoRM$new(patchID = patchID, M = mosquitoPars$M, Y = 0, Z = 0, par = mosquitoPars$par)},
                       {stop("unrecognized entry for 'mosquitoModule'")}
                      )

                     # HUMAN HumanPop
                     private$HumanPop = HumanPop$new(N = humanPars$N, patchID = patchID, houseIDs = NULL, bDays = humanPars$bDays, bWeights = humanPars$bWeights, tStart = tStart - 1)

                     # Output
                     if(!dir.exists(directory)){
                       dir.create(directory)
                     } else {

                       dirFiles = system(command = paste0("ls ",directory),intern = TRUE)
                       rmFiles = grep(pattern = paste0("Patch",patchID),x = dirFiles)
                       for(i in rmFiles){
                         print(paste0("removing file: ",directory,dirFiles[i]))
                         file.remove(paste0(directory,dirFiles[i]))
                       }

                     }

                     private$directory = directory


                   }

                  ),

                  # private methods & fields
                  private = list(

                    # Simulation-level parameters
                    tStart = NULL,
                    tNow = NULL,

                    # Mosquito Feeding
                    bWeightHuman   = NULL,  # biting weight of HumanPop
                    bWeightZoo     = NULL,  # biting weight of livestock
                    kappa          = NULL,  # relative infectiousness

                    # R6 Classes
                    MosquitoPop = NULL,
                    HumanPop = NULL,

                    # Output Connections
                    directory = NULL,
                    conMosquito = NULL,

                    # Pointers
                    TilePointer = NULL     # point to the enclosing metapopulation TILE

                  )

) #end class definition


###############################################################################
# MacroPatch: Generic & Shared Methods
###############################################################################

###############################################################################
# MacroPatch: Getters & Setters
###############################################################################

#' MacroPatch: Get Patch's Human Biting Weight
#'
#' Return \code{private$bWeightHuman}
#'
#'
get_bWeightHuman_MacroPatch <- function(){
   return(private$bWeightHuman)
}

MacroPatch$set(which = "public",name = "get_bWeightHuman",
  value = get_bWeightHuman_MacroPatch,
  overwrite = TRUE)

#' MacroPatch: Set Patch's Human Biting Weight
#'
#' Set \code{private$bWeightHuman}
#'
#' @param bWeightHuman new human biting weight
#'
set_bWeightHuman_MacroPatch <- function(bWeightHuman){
   private$bWeightHuman = bWeightHuman
}

MacroPatch$set(which = "public",name = "set_bWeightHuman",
  value = set_bWeightHuman_MacroPatch,
  overwrite = TRUE)

#' MacroPatch: Accumulate Patch's Human Biting Weight
#'
#' Accumulate \code{private$bWeightHuman} (equivalent to private$bWeightHuman += bWeightHuman)
#'
#' @param bWeightHuman increment to accumulate
#'
accumulate_bWeightHuman_MacroPatch <- function(bWeightHuman){
  private$bWeightHuman = private$bWeightHuman + bWeightHuman
}

MacroPatch$set(which = "public",name = "accumulate_bWeightHuman",
  value = accumulate_bWeightHuman_MacroPatch,
  overwrite = TRUE)

get_bWeightZoo_MacroPatch <- function(){
   return(private$bWeightZoo)
}

MacroPatch$set(which = "public",name = "get_bWeightZoo",
  value = get_bWeightZoo_MacroPatch,
  overwrite = TRUE)

set_bWeightZoo_MacroPatch <- function(bWeightZoo){
   private$bWeightZoo = bWeightZoo
}

MacroPatch$set(which = "public",name = "set_bWeightZoo",
  value = set_bWeightZoo_MacroPatch,
  overwrite = TRUE)

get_kappa_MacroPatch <- function(){
  return(private$kappa)
}

MacroPatch$set(which = "public",name = "get_kappa",
  value = get_kappa_MacroPatch,
  overwrite = TRUE)

set_kappa_MacroPatch <- function(kappa){
  private$kappa = kappa
}

MacroPatch$set(which = "public",name = "set_kappa",
  value = set_kappa_MacroPatch,
  overwrite = TRUE)

accumulate_kappa_MacroPatch <- function(kappa){
  private$kappa = private$kappa + kappa
}

MacroPatch$set(which = "public",name = "accumulate_kappa",
  value = accumulate_kappa_MacroPatch,
  overwrite = TRUE)

# MosquitoPop
get_MosquitoPop_MacroPatch <- function(){
  return(private$MosquitoPop)
}

MacroPatch$set(which = "public",name = "get_MosquitoPop",
  value = get_MosquitoPop_MacroPatch,
  overwrite = TRUE)

set_MosquitoPop_MacroPatch <- function(MosquitoPop){
  private$MosquitoPop = MosquitoPop
}

MacroPatch$set(which = "public",name = "set_MosquitoPop",
  value = set_MosquitoPop_MacroPatch,
  overwrite = TRUE)

# HumanPop
get_HumanPop_MacroPatch <- function(){
  return(private$HumanPop)
}

MacroPatch$set(which = "public",name = "get_HumanPop",
  value = get_HumanPop_MacroPatch,
  overwrite = TRUE)

set_HumanPop_MacroPatch <- function(HumanPop){
  private$HumanPop = HumanPop
}

MacroPatch$set(which = "public",name = "set_HumanPop",
  value = set_HumanPop_MacroPatch,
  overwrite = TRUE)

#' MacroPatch: Get Patch's Pointer to Enclosing \code{\link{MacroTile}}
#'
#' Return \code{private$TilePointer}
#'
get_TilePointer_MacroPatch <- function(){
  return(private$TilePointer)
}

MacroPatch$set(which = "public",name = "get_TilePointer",
  value = get_TilePointer_MacroPatch,
  overwrite = TRUE)

#' MacroPatch: Set Patch's Pointer to Enclosing \code{\link{MacroTile}}
#'
#' Set \code{private$TilePointer}
#'
#' @param TilePointer new pointer
#'
set_TilePointer_MacroPatch <- function(TilePointer){
  private$TilePointer = TilePointer
}

MacroPatch$set(which = "public",name = "set_TilePointer",
  value = set_TilePointer_MacroPatch,
  overwrite = TRUE)
