#################################################################
#
#   MASH
#   SEARCH.MicroKernel.Setup() definition
#   MICRO: MBITES Search Movement
#   David Smith, Hector Sanchez, Sean Wu
#   June 1, 2017
#
#################################################################

#################################################################
# Initialize Methods
#################################################################

#' MICRO Search Kernels: Initialize Additional Methods & Fields in \code{\link{MicroMosquitoPopFemale}} and \code{\link{MicroMosquitoFemale}}
#'
#' Initialize MICRO Search Kernels module of mosquito search behavior.
#'
#' @param MBITES what M-BITES module to run?
#'  * BRO: Blood Feeding, Resting, Oviposition module
#'  * BROM: Blood Feeding, Resting, Oviposition, Mating module
#'  * FULL: Full life cycle M-BITES module
#' @md
#' @return does stuff
#' @examples
#' SEARCH.MicroKernel.Setup()
#' @export
SEARCH.MicroKernel.Setup <- function(MBITES = "BRO", overwrite = TRUE){

  message(paste0("initializing MICRO component methods & fields for MicroMosquitoPop & MicroMosquito Class for M-BITES module: ",MBITES))

  #################################################################
  # SampleMove() Methods in Populations
  #################################################################

  MicroMosquitoPopFemale$set(which = "public",name = "SampleMove",
              value = MicroKernel_SampleMvOb_MicroMosquitoPopFemale,
              overwrite = overwrite
  )

  # MicroMosquitoPopMale$set(which = "public",name = "SampleMove",
  #             value = MicroKernel_SampleMvOb_MicroMosquitoPopMale,
  #             overwrite = overwrite
  # )

  #################################################################
  # Male Movement
  #################################################################

  # male movement does not change betwen female lifecycle modules
  MicroMosquitoPopMale$set(which = "public",name = "get_movement",
              value = get_MicroKernel_movement_Male,
              overwrite = overwrite
  )
  MicroMosquitoMale$set(which = "public",name = "moveMe",
              value = MicroKernel_moveMe_Male,
              overwrite = overwrite
  )

  #################################################################
  # Female Movement
  #################################################################

  if(MBITES == "BRO"){

    MicroTile$set(which = "public",name = "get_movement",
                value = get_MicroKernel_movement_BRO,
                overwrite = overwrite
    )
    MicroMosquitoFemale$set(which = "public",name = "moveMe",
                value = MicroKernel_moveMe_BRO,
                overwrite = overwrite
    )

  } else if(MBITES == "BROM"){

    MicroTile$set(which = "public",name = "get_movement",
                value = get_MicroKernel_movement_BROM,
                overwrite = overwrite
    )
    MicroMosquitoFemale$set(which = "public",name = "moveMe",
                value = MicroKernel_moveMe_BROM,
                overwrite = overwrite
    )

  } else if(MBITES == "FULL"){

    MicroTile$set(which = "public",name = "get_movement",
                value = get_MicroKernel_movement_FULL,
                overwrite = overwrite
    )
    MicroMosquitoFemale$set(which = "public",name = "moveMe",
                value = MicroKernel_moveMe_FULL,
                overwrite = overwrite
    )

  } else {
    stop("argument MBITES must be a character in 'BRO', 'BROM', or 'FULL'")
  }

  # if(MBITES == "BRO"){
  #
  #   MicroMosquitoPopFemale$set(which = "public",name = "get_movement",
  #               value = get_MicroKernel_movement_BRO,
  #               overwrite = overwrite
  #   )
  #   MicroMosquitoFemale$set(which = "public",name = "moveMe",
  #               value = MicroKernel_moveMe_BRO,
  #               overwrite = overwrite
  #   )
  #
  # } else if(MBITES == "BROM"){
  #
  #   MicroMosquitoPopFemale$set(which = "public",name = "get_movement",
  #               value = get_MicroKernel_movement_BROM,
  #               overwrite = overwrite
  #   )
  #   MicroMosquitoFemale$set(which = "public",name = "moveMe",
  #               value = MicroKernel_moveMe_BROM,
  #               overwrite = overwrite
  #   )
  #
  # } else if(MBITES == "FULL"){
  #
  #   MicroMosquitoPopFemale$set(which = "public",name = "get_movement",
  #               value = get_MicroKernel_movement_FULL,
  #               overwrite = overwrite
  #   )
  #   MicroMosquitoFemale$set(which = "public",name = "moveMe",
  #               value = MicroKernel_moveMe_FULL,
  #               overwrite = overwrite
  #   )
  #
  # } else {
  #   stop("argument MBITES must be a character in 'BRO', 'BROM', or 'FULL'")
  # }

}
