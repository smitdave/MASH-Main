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
#   September 7, 2017
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
#'  * BRO: Blood Feeding, Resting, Oviposition module
#'  * BROM: Blood Feeding, Resting, Oviposition, Mating module
#'  * BROMS: Blood Feeding, Resting, Oviposition, Mating & Sugar Feeding module
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
  MosquitoPopMale$set(which = "public",name = "get_movement",
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

  if(MBITES == "BRO"){

    Tile$set(which = "public",name = "get_movement",
                value = get_MicroKernel_movement_BRO,
                overwrite = overwrite
    )
    MosquitoFemale$set(which = "public",name = "moveMe",
                value = MicroKernel_moveMe_BRO,
                overwrite = overwrite
    )

  } else if(MBITES == "BROM"){

    Tile$set(which = "public",name = "get_movement",
                value = get_MicroKernel_movement_BROM,
                overwrite = overwrite
    )
    MosquitoFemale$set(which = "public",name = "moveMe",
                value = MicroKernel_moveMe_BROM,
                overwrite = overwrite
    )

  } else if(MBITES == "FULL"){

    Tile$set(which = "public",name = "get_movement",
                value = get_MicroKernel_movement_FULL,
                overwrite = overwrite
    )
    MosquitoFemale$set(which = "public",name = "moveMe",
                value = MicroKernel_moveMe_FULL,
                overwrite = overwrite
    )

  } else {
    stop("argument MBITES must be a character in 'BRO', 'BROM', or 'FULL'")
  }

}
