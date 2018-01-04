###############################################################################
#      _____                      __
#     / ___/___  ____ ___________/ /_
#     \__ \/ _ \/ __ `/ ___/ ___/ __ \
#    ___/ /  __/ /_/ / /  / /__/ / / /
#   /____/\___/\__,_/_/   \___/_/ /_/
#
#   MASH-MICRO
#   SEARCH: Kernel Setup
#   MASH-MICRO Team
#   September 2017
#
###############################################################################

#################################################################
# Initialize Methods
#################################################################

#' MICRO Search Kernels: Initialize Additional Methods & Fields in \code{\link{MosquitoPopFemale}} and \code{\link{MosquitoFemale}}
#'
#' Initialize MICRO Search Kernels module of mosquito search behavior.
#'
#' @param overwrite overwrite existing methods
#'
#' @export
SEARCH.Kernel.Setup <- function(overwrite = TRUE){

  cat("initializing MBITES-Search module\n",sep="")

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

}
