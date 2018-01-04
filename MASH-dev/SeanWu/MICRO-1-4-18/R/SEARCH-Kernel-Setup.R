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
#' @param MBITES what M-BITES module to run?
#'  * BRO: Blood Feeding, Resting, Oviposition module (also including BRO with SUGAR and/or MATE)
#'  * FULL: Full life cycle M-BITES module (Blood Feeding Search & Attempt, Resting, Oviposition Search & Attempt, Mating, Sugar Feeding)
#'
#' @export
SEARCH.Kernel.Setup <- function(MBITES = "BRO", overwrite = TRUE){

  cat("initializing MICRO component methods & fields for MicroMosquitoPop & MicroMosquito Class for M-BITES module: ",MBITES,"\n",sep="")

  #################################################################
  # SampleMove() Methods in Populations
  #################################################################

  MosquitoPopFemale$set(which = "public",name = "SampleMove",
              value = MicroKernel_SampleMvOb_MosquitoPopFemale,
              overwrite = overwrite
  )

  MosquitoPopMale$set(which = "public",name = "SampleMove",
              value = MicroKernel_SampleMvOb_MosquitoPopMale,
              overwrite = overwrite
  )

  #################################################################
  # Male Movement
  #################################################################

  # male movement does not change betwen female lifecycle modules
  Tile$set(which = "public",name = "get_movementMale",
              value = get_MicroKernel_movement_Male,
              overwrite = overwrite
  )
  MosquitoMale$set(which = "public",name = "moveMe",
              value = MicroKernel_moveMe_Male,
              overwrite = overwrite
  )

  #################################################################
  # Female Movement
  #################################################################

  switch(MBITES,
    BRO = {
      Tile$set(which = "public",name = "get_movement",
                  value = get_MicroKernel_movement_BRO,
                  overwrite = overwrite
      )
      MosquitoFemale$set(which = "public",name = "moveMe",
                  value = MicroKernel_moveMe_BRO,
                  overwrite = overwrite
      )
    },
    FULL = {
      Tile$set(which = "public",name = "get_movement",
                  value = get_MicroKernel_movement_FULL,
                  overwrite = overwrite
      )
      MosquitoFemale$set(which = "public",name = "moveMe",
                  value = MicroKernel_moveMe_FULL,
                  overwrite = overwrite
      )
    },
    {stop(cat("MBITES module must be a character in 'BRO' or 'FULL', got: ",MBITES,"\n",sep=""))}
  )

}
