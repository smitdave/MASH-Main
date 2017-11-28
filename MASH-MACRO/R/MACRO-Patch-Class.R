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
#' write me
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: i'm an argument!
#'
#' @section **Methods**:
#'  * method: i'm a method!
#'
#' @section **Fields**:
#'  * field: i'm a field!
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

                   initialize = function(patchID){

                     private$patchID = patchID

                   }

                  ),

                  # private methods & fields
                  private = list(

                    patchID                   = integer(1),

                    # population dynamic parameters
                    EggQ                      = NULL,
                    ImagoQ                    = NULL,

                    # infection dynamic parameters
                    bWeightHuman              = numeric(1),
                    bWeightZoo                = numeric(1),
                    bWeightZootox             = numeric(1),
                    kappa                     = numeric(1),

                    # Pointers
                    TilePointer               = NULL,     # point to the enclosing metapopulation TILE
                    MosquitoPointer           = NULL,
                    HumansPointer             = NULL

                  )

) #end class definition


###############################################################################
# MacroPatch: Generic & Shared Methods
###############################################################################

###############################################################################
# Infection Dynamics
###############################################################################

#' MacroPatch: Get Patch Human Biting Weight
#'
#' Return \code{private$bWeightHuman}
#'
get_bWeightHuman_MacroPatch <- function(){
   return(private$bWeightHuman)
}

MacroPatch$set(which = "public",name = "get_bWeightHuman",
  value = get_bWeightHuman_MacroPatch, overwrite = TRUE
)

#' MacroPatch: Set Patch Human Biting Weight
#'
#' Set \code{private$bWeightHuman}
#'
#' @param bWeightHuman numeric
#'
set_bWeightHuman_MacroPatch <- function(bWeightHuman){
   private$bWeightHuman = bWeightHuman
}

MacroPatch$set(which = "public",name = "set_bWeightHuman",
  value = set_bWeightHuman_MacroPatch, overwrite = TRUE
)

#' MacroPatch: Accumulate Patch Human Biting Weight
#'
#' Accumulate \code{private$bWeightHuman} (equivalent to private$bWeightHuman += bWeightHuman)
#'
#' @param bWeightHuman numeric
#'
accumulate_bWeightHuman_MacroPatch <- function(bWeightHuman){
  private$bWeightHuman = private$bWeightHuman + bWeightHuman
}

MacroPatch$set(which = "public",name = "accumulate_bWeightHuman",
  value = accumulate_bWeightHuman_MacroPatch, overwrite = TRUE
)

#' MacroPatch: Get Patch Zoo Biting Weight
#'
#' Return \code{private$bWeightZoo}
#'
get_bWeightZoo_MacroPatch <- function(){
   return(private$bWeightZoo)
}

MacroPatch$set(which = "public",name = "get_bWeightZoo",
  value = get_bWeightZoo_MacroPatch, overwrite = TRUE
)

#' MacroPatch: Set Patch Zoo Biting Weight
#'
#' Set \code{private$bWeightZoo}
#'
#' @param bWeightZoo numeric
#'
set_bWeightZoo_MacroPatch <- function(bWeightZoo){
   private$bWeightZoo = bWeightZoo
}

MacroPatch$set(which = "public",name = "set_bWeightZoo",
  value = set_bWeightZoo_MacroPatch, overwrite = TRUE
)

#' MacroPatch: Accumulate Patch Zoo Biting Weight
#'
#' Accumulate \code{private$bWeightZoo} (equivalent to private$bWeightZoo += bWeightZoo)
#'
#' @param bWeightZoo numeric
#'
accumulate_bWeightZoo_MacroPatch <- function(bWeightZoo){
  private$bWeightZoo = private$bWeightZoo + bWeightZoo
}

MacroPatch$set(which = "public",name = "accumulate_bWeightZoo",
  value = accumulate_bWeightZoo_MacroPatch, overwrite = TRUE
)

#' MacroPatch: Get Patch \eqn{\Kappa}
#'
#' \eqn{\Kappa} is the proportion of mosquitoes that would become infected after blood feeding on any human, the net infectiousness of humans.
#' Return \code{private$kappa}
#'
get_kappa_MacroPatch <- function(){
  return(private$kappa)
}

MacroPatch$set(which = "public",name = "get_kappa",
  value = get_kappa_MacroPatch, overwrite = TRUE
)

#' MacroPatch: Set Patch \eqn{\Kappa}
#'
#' \eqn{\Kappa} is the proportion of mosquitoes that would become infected after blood feeding on any human, the net infectiousness of humans.
#' Set \code{private$kappa}
#'
#' @param kappa numeric
#'
set_kappa_MacroPatch <- function(kappa){
  private$kappa = kappa
}

MacroPatch$set(which = "public",name = "set_kappa",
  value = set_kappa_MacroPatch, overwrite = TRUE
)

#' MacroPatch: Accumulate Patch \eqn{\Kappa}
#'
#' \eqn{\Kappa} is the proportion of mosquitoes that would become infected after blood feeding on any human, the net infectiousness of humans.
#' Accumulate \code{private$kappa} (equivalent to private$kappa += kappa)
#'
#' @param kappa numeric
#'
accumulate_kappa_MacroPatch <- function(kappa){
  private$kappa = private$kappa + kappa
}

MacroPatch$set(which = "public",name = "accumulate_kappa",
  value = accumulate_kappa_MacroPatch, overwrite = TRUE
)


###############################################################################
# Other Getters
###############################################################################

#' MacroPatch: Get Patch Pointer to Enclosing \code{\link{MacroTile}}
#'
#' Return \code{private$TilePointer}
#'
get_TilePointer_MacroPatch <- function(){
  return(private$TilePointer)
}

MacroPatch$set(which = "public",name = "get_TilePointer",
  value = get_TilePointer_MacroPatch, overwrite = TRUE
)

#' MacroPatch: Set Patch Pointer to Enclosing \code{\link{MacroTile}}
#'
#' Set \code{private$TilePointer}
#'
#' @param TilePointer new pointer
#'
set_TilePointer_MacroPatch <- function(TilePointer){
  private$TilePointer = TilePointer
}

MacroPatch$set(which = "public",name = "set_TilePointer",
  value = set_TilePointer_MacroPatch, overwrite = TRUE
)
