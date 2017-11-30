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
#'  * AquaPar: named list of parameters for aquatic ecology model (see \code{\link{AquaPop_Emerge.Parameters}} or ... for structure)
#'  * PatchPar: list of parameters (length of list must be equal to number of patches and have \code{bWeightZoo} and \code{bWeightZootox} specified)
#'  * MosquitoPar: named list of parameters for mosquito model
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

                   initialize = function(nPatch, AquaPar, PatchPar, MosquitoPar){

                     # initialize patches
                     cat("initializing patches\n")
                     private$nPatch = nPatch
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
                       private$Patches$get(as.character(i))$set_TilePointer(self)

                     } # finish initializing patches

                     # initialize mosquito population (in C++ Mosquito is a std::unique_ptr<Mosquito_Base> and then fill it with a derived class)
                     cat("initializing mosquito population\n")
                     switch(MosquitoPar$model,
                       RM = {
                         private$Mosquito = Mosquito_RM$new(M = MosquitoPar$M, EIP = MosquitoPar$EIP, p=MosquitoPar$p, f=MosquitoPar$f, Q=MosquitoPar$Q, v=MosquitoPar$v, psi=MosquitoPar$psi)
                       },
                       {stop("invalid mosquito ecology model selected")}
                     )
                     private$Mosquito$set_TilePointer(self)
                     # finish initializing mosquito population

                     cat("initializing human population\n")

                     cat("setting pointers\n")


                   } # end constructor

                   # finalize = function(){}

                  ),

                  # private methods & fields
                  private = list(

                    # Simulation-level parameters
                    tStart                    = 0,
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


###############################################################################
# Getters & Setters
###############################################################################

#' Get Current Time
#'
#' write me
#'
#'  * This method is bound to \code{MacroTile$get_tNow}
#'
get_tNow_MacroTile <- function(){
  return(private$tNow)
}

MacroTile$set(which = "public",name = "get_tNow",
          value = get_tNow_MacroTile, overwrite = TRUE
)

#' Get a Patch
#'
#' Return a reference to a \code{\link{MacroPatch}}
#'
#'  * This method is bound to \code{MacroTile$get_Patch}
#'
#' @param patchID a id of the patch that will be coerced to a character string key
#'
get_Patch_MacroTile <- function(patchID){
  return(private$Patches$get(as.character(patchID)))
}

MacroTile$set(which = "public",name = "get_Patch",
          value = get_Patch_MacroTile, overwrite = TRUE
)

#' Get Patches
#'
#' Return a reference to the \code{\link[MASHcpp]{HashMap}} object that contains all patches
#'
#'  * This method is bound to \code{MacroTile$get_Patches}
#'
get_Patches_MacroTile <- function(){
  return(private$Patches)
}

MacroTile$set(which = "public",name = "get_Patches",
          value = get_Patches_MacroTile, overwrite = TRUE
)

#' Get Patches
#'
#' Return a reference to a mosquito population derived from \code{\link{Mosquito_Base}} residing in this tile
#'
#'  * This method is bound to \code{MacroTile$get_MosquitoPointer}
#'
get_MosquitoPointer_MacroTile <- function(){
  return(private$Mosquito)
}

MacroTile$set(which = "public",name = "get_MosquitoPointer",
          value = get_MosquitoPointer_MacroTile, overwrite = TRUE
)

#' Get Patches
#'
#' Return a reference to the \code{\link{HumanPop}} residing in this tile
#'
#'  * This method is bound to \code{MacroTile$get_HumansPointer}
#'
get_HumansPointer_MacroTile <- function(){
  return(private$HumanPop)
}

MacroTile$set(which = "public",name = "get_HumansPointer",
          value = get_HumansPointer_MacroTile, overwrite = TRUE
)
