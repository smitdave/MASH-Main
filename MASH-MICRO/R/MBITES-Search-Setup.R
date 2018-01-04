###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MICRO
#   MBITES: Search Setup
#   MASH Team
#   January 2018
#
###############################################################################

#' MICRO Search Kernels: Initialize Additional Methods & Fields in \code{\link{MosquitoPopFemale}} and \code{\link{MosquitoFemale}}
#'
#' Initialize MICRO Search algorithms for mosquito movement.
#'
#' @param module character in "kernel"
#' @param overwrite overwrite existing methods
#'
#' @export
MBITES.Search.Setup <- function(
  module = "kernel",
  overwrite = TRUE
){

  cat("initializing MBITES-Search module\n",sep="")

  switch(module,
    kernel = {

      #################################################################
      # SampleMove() Methods in Populations
      #################################################################

      MosquitoPopFemale$set(which = "public",name = "SampleMove",
                  value = MicroKernel_SampleMvOb_MosquitoPopFemale, overwrite = overwrite
      )

      MosquitoPopMale$set(which = "public",name = "SampleMove",
                  value = MicroKernel_SampleMvOb_MosquitoPopMale, overwrite = overwrite
      )

      #################################################################
      # Male Movement
      #################################################################

      # male movement does not change betwen female lifecycle modules
      Tile$set(which = "public",name = "get_movementMale",
                  value = get_MicroKernel_movement_Male, overwrite = overwrite
      )

      MosquitoMale$set(which = "public",name = "moveMe",
                  value = MicroKernel_moveMe_Male, overwrite = overwrite
      )

      #################################################################
      # Female Movement
      #################################################################

      Tile$set(which = "public",name = "get_movement",
                  value = get_MicroKernel_movement_FULL, overwrite = overwrite
      )

      MosquitoFemale$set(which = "public",name = "moveMe",
                  value = MicroKernel_moveMe_FULL, overwrite = overwrite
      )


    },
    {stop(cat("unrecognized entry for module, expected character in 'kernel', got: ",module,"\n",sep=""))}
  )

}
