#################################################################
#
#   MASH
#   R6-ified
#   MACRO MacroPatch Class Parameters
#   David Smith, Hector Sanchez, Sean Wu
#   May 22, 2016
#
#################################################################


#' Initialize MACRO Patch Parameters for \code{MacroPatch}
#'
#' This is used to generate a list of parameters for \code{\link{MacroPatch}} and should be used during its initialization.
#'
#' @param N number of patches
#' @param bWeightZoo1 shape parameter of gamma zoophilic biting weights
#' @param bWeightZoo2 rate parameter of gamma zoophilic biting weights
#' @return return a list
#' @examples
#' MACRO.Patch.Parameters()
#' @export
MACRO.Patch.Parameters <- function(

    ########################################
    #  Parameters
    ########################################

    # number of Patches
    N,

    # houses
    hhID, # list

    # humans
    humanIDs, # list

    # Biting weights
    bWeightHuman = NULL,
    bWeightZoo1 = 1,
    bWeightZoo2 = 1,

    # component options
    aquaModule = "emerge",
    aquaPars

  ){

    # if(is.null(bWeightHuman)){
    #   bWeightHuman = rep(0,N)
    # }

    MacroPatch_PAR = list(
        N   = N,
        hhID = hhID,

        bWeightHuman = rep(0,N),
        bWeightZoo   = rgamma(n = N,shape = bWeightZoo1, rate = bWeightZoo1),
        bWeightZootox = rep(0,N),

        Q         = rep(0,N),
        kappa     = rep(0,N),
        humanIDs  = humanIDs,

        #Egg laying
        aquaID        = 1L:N,
        aquaP         = rep(1,N),
        aquaNewM      = rep(0,N),
        weightAqua    = rep(0,N),   # For modeling movement
        weightOvitrap = rep(0,N),

        weightSugar   = rep(0,N),
        weightBait    = rep(0,N),

        weightMate    = rep(0,N)
      )

  ################################################
  # AQUATIC ECOLOGY
  ################################################

  if(aquaModule == "emerge"){

    MacroPatch_PAR$aquaModule = aquaModule
    MacroPatch_PAR$season = makeLambda_Macro(aquaPars)
    MacroPatch_PAR$PatchesImagoQ =  replicate(n=N,expr=newImago(),simplify=FALSE)
    MacroPatch_PAR$PatchesEggQ = replicate(n=N,expr=newEgg(),simplify=FALSE)

  } else if(aquaModule == "EL4P"){

    MacroPatch_PAR$aquaModule = aquaModule
    stop("sean hasn't written the routines for MACRO EL4P Aquatic Ecology")

  } else {
    stop("aquaModule must be a value in 'emerge' or 'EL4P'")
  }

  return(MacroPatch_PAR)
}
