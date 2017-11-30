###############################################################################
#     _______ __
#    /_  __(_) /__
#     / / / / / _ \
#    / / / / /  __/
#   /_/ /_/_/\___/
#
#   MASH-MACRO
#   Tile Class Definition
#   MASH Team
#   November 2017
#
###############################################################################

#' MASH-MACRO MacroTile Class Definition
#'
#' write me
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * nPatch: number of patches
#'  * AquaPar: named list of parameters for aquatic ecology module (see \code{\link{AquaPop_Emerge.Parameters}} or ... for structure)
#'  * PatchPar:
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
MacroTile <- R6::R6Class(classname = "MacroTile",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public methods & fields
                 public = list(

                   #################################################
                   # Constructor
                   #################################################

                   initialize = function(nPatch, AquaPar, PatchPar){

                     # initialize patches
                     cat("initializing patches\n")
                     private$Patches = MASHcpp::HashMap$new(N = nPatch)

                     for(i in 1:nPatch){

                       switch(AquaPar$model,
                         Emerge = {
                           AquaPop = AquaPop_Emerge$new(lambda = AquaPar$Patches[[i]]$lambda)
                         },
                         EL4P = {
                           stop("not written yet")
                         },
                         {stop("invalid aquatic ecology model selected")}
                       )

                       patch = MacroPatch$new(patchID=i, AquaPop=AquaPop, bWeightZoo=PatchPar[[i]]$bWeightZoo, bWeightZootox=PatchPar[[i]]$bWeightZootox)
                       private$Patches$assign(key=as.character(i),value=patch)

                     } # finish initializing patches

                     cat("initializing mosquito population\n")

                     cat("initializing human population\n")

                     cat("setting pointers\n")


                   } # end constructor

                   # finalize = function(){}

                  ),

                  # private methods & fields
                  private = list(

                    # Simulation-level parameters
                    tStart                    = integer(1),
                    tNow                      = integer(1),
                    nPatch                    = integer(1),

                    # class containers
                    Patches                   = NULL,
                    HumanPop                  = NULL,
                    Mosquito                  = NULL,

                    # Output Connections
                    directory                 = character(1)

                  )

) #end class definition



get_MosquitoPointer_MacroTile <- function(){

}

get_HumansPointer_MacroTile <- function(){

}

get_PatchesPointer_MacroTile <- function(){

}
