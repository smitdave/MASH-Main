#################################################################
#
#   MASH
#   R6-ified
#   'Emerge' model of aquatic ecology for MacroPatch
#   David Smith, Hector Sanchez, Sean Wu
#   May 24, 2016
#
#################################################################

#################################################################
# Standalone functions for lambda
#################################################################

#' MACRO: Generate Seasonal Emergence (Lambda) for \code{Emerge} model of Aquatic Ecology
#'
#' Generate lambda for all patches.
#'
#' @param aquaPars a list of the following structure
#'  * lambda: vector of length equal to number of patches \code{\link{MacroPatch}} where each element is the number of emerging adult females per human per day averaged over one year (required)
#'  * lambdaWeight: vector of weights applied to each site (if not specified or set to \code{NULL} initialize to Gamma(1,1) distribution)
#'  * offset: vector of seasonal offsets in peak emergence applied to each site (if not specified or set to \code{NULL} initialize to 0 for all sites)
#' @md
#' @return list \code{lambda} where each element is the daily emergence for that \code{\link{MacroPatch}}
#' @examples
#' makeLambda_Macro(aquaPars = list(lambda=c(5,10,15)))
#' @export
makeLambda_Macro <- function(aquaPars){

  with(aquaPars,{

    N = length(lambda)
    if(!exists("lambdaWeight",inherits = FALSE) || is.null(lambdaWeight)){lambdaWeight = rgamma(n = N,shape = 1,rate = 1)}

    K = lambda*lambdaWeight / sum(lambdaWeight)
    if(!exists("offset",inherits = FALSE) || is.null(lambdaWeight)){offset = rep(0,length=N)}

    lambdaOut = vector(mode="list",length=N)
    for(ix in 1:N){
      lambdaOut[[ix]] = K[ix]*(1+sin(2*pi*(c(1:365)-offset[ix])/365))
    }

    return(lambdaOut)

  })

}


#################################################################
# Setup
#################################################################

#' Initialize Additional Methods & Fields in \code{MacroPatch} for Emerge Module of Aquatic Ecology
#'
#' Write me! See for parameters that are initialized after this; ie: anything that \code{\link{MACRO.Patch.Parameters}} calls for 'Emerge' should be defined here.
#'
#' @param a parameter
#' @return do stuff
#' @examples
#' MACRO.Patch.Emerge.Setup()
#' @export
MACRO.Patch.Emerge.Setup <- function(overwrite = TRUE){

  message("initializing MACRO 'Emerge' Module Methods for 'MacroPatch' Class")

  #################################################################
  # Methods
  #################################################################

    # addCohort_MacroEmerge takes the generic name addCohort because it interfaces with the MacroMosquitoPop class
    MacroPatch$set(which = "public",name = "addCohort_MacroEmerge",
              value = addCohort_MacroEmerge,
              overwrite = overwrite
    )

    # add adults from lambda to PatchesImagoQ
    MacroPatch$set(which = "public",name = "emergingAdults_MacroEmerge",
              value = emergingAdults_MacroEmerge,
              overwrite = overwrite
    )

    # helper function to get from ImagoQ to addCohort
    MacroPatch$set(which = "public",name = "oneDay_MacroEmerge",
              value = oneDay_MacroEmerge,
              overwrite = overwrite
    )


  #################################################################
  # Getters & Setters
  #################################################################

  # season: lambda emergence for each patch
  MacroPatch$set(which = "private",name = "season",
            value = NULL,
            overwrite = overwrite
  )

  MacroPatch$set(which = "public",name = "get_season",
            value = get_season,
            overwrite = overwrite
  )

  MacroPatch$set(which = "public",name = "set_season",
            value = set_season,
            overwrite = overwrite
  )

  # ImagoQ: queue for emerging adults
  MacroPatch$set(which = "private",name = "PatchesImagoQ",
            value = NULL,
            overwrite = overwrite
  )

  MacroPatch$set(which = "public",name = "get_PatchesImagoQ",
            value = get_PatchesImagoQ,
            overwrite = overwrite
  )

  MacroPatch$set(which = "public",name = "set_PatchesImagoQ",
            value = set_PatchesImagoQ,
            overwrite = overwrite
  )

  # eventually may need push_ImagoQ and push_EggQ when doing mosquito genetics in MACRO; ie it will push to ImagoQ[[PATCH]][[EMERGING_PACKET_OF_MOSY]]

  # EggQ: queue for egg batches (not used in Emerge; here for compatibility with mosquito ecology models only)
  MacroPatch$set(which = "private",name = "PatchesEggQ",
            value = NULL,
            overwrite = overwrite
  )

  MacroPatch$set(which = "public",name = "get_PatchesEggQ",
            value = get_PatchesEggQ,
            overwrite = overwrite
  )

  MacroPatch$set(which = "public",name = "set_PatchesEggQ",
            value = set_PatchesEggQ,
            overwrite = overwrite
  )

}


#################################################################
# Methods
#################################################################

#' MACRO: Calculate Emerging Adults for \code{MacroPatch}
#'
#' Write me! does this for all patches
#'  * This method is bound to \code{MacroPatch$addCohort_MacroEmerge}
#'
#' @md
addCohort_MacroEmerge <- function(){
  newM = self$get_MosquitoPointer()$get_M() + self$emergingAdults_MacroEmerge()
  self$get_MosquitoPointer()$set_M(M = newM, ix = NULL)
}

#' MACRO: Calculate Emerging Adults from ImagoQ for \code{MacroPatch}
#'
#' Write me! does this for all patches. generates a vector of emerging adults, and zeros out the ImagoQ
#'  * This method is bound to \code{MacroPatch$emergingAdults_MacroEmerge}
#'
#' @md
emergingAdults_MacroEmerge <- function(){

  # grab slots that are ready to emerge
  tNow = self$get_TilePointer()$get_tNow()
  newM = vector(mode="integer",length=private$N)
  for(ixP in 1:private$N){

    # imago = private$ImagoQ$get_ImagoQTime(tNow = tNow,clear = TRUE)
    # newM[ixP] = sum(vapply(X = imago,FUN = function(x){x$N},FUN.VALUE = integer(1)))

    newM[ixP] = private$PatchesImagoQ[[ixP]]$N
    self$set_PatchesImagoQ(PatchesImagoQ = newImago(),ixP = ixP)
  }

  return(newM)
}

#' Get \code{MacroPatch} Seasonal Emergence
#'
#' Queue the ImagoQ
#'  * This method is bound to \code{MacroPatch$oneDay_MacroEmerge}
#'
#' @md
oneDay_MacroEmerge <- function(){

  tNow = self$get_TilePointer()$get_tNow()
  lambdaExact = vapply(X = private$season,FUN = function(x){x[floor(tNow)%%365+1]},FUN.VALUE = numeric(1))
  lambdaEmerge = rpois(n = length(lambdaExact),lambda = lambdaExact)
  for(ixP in 1:private$N){
    # private$ImagoQ$add_ImagoQ(N_new = lambdaEmerge[ixP], tEmerge_new = tNow, genotype_new = -1L, damID_new = "-1", sireID_new = "-1")
    self$set_PatchesImagoQ(PatchesImagoQ = newImago(N = lambdaEmerge[ixP], tEmerge = tNow), ixP = ixP)
  }

}


#################################################################
# Getters & Setters
#################################################################


#' Get \code{MacroPatch} Seasonal Emergence
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
get_season <- function(ixP = NULL){
  if(is.null(ixP)){
    return(private$season)
  } else {
    return(private$season[[ixP]])
  }
}

#' Set \code{MacroPatch} Seasonal Emergence
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
set_season <- function(season, ixP = NULL){
  if(is.null(ixP)){
    private$season = season
  } else {
    private$season[[ixP]] = season
  }
}

#' Get \code{MacroPatch} ImagoQ (Emerging Adults Queue)
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
get_PatchesImagoQ <- function(ixP = NULL){
  if(is.null(ixP)){
    return(private$PatchesImagoQ)
  } else {
    return(private$PatchesImagoQ[[ixP]])
  }
}

#' Set \code{MacroPatch} ImagoQ (Emerging Adults Queue)
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
set_PatchesImagoQ <- function(PatchesImagoQ, ixP = NULL){
  if(is.null(ixP)){
    private$PatchesImagoQ = PatchesImagoQ
  } else {
    private$PatchesImagoQ[[ixP]] = PatchesImagoQ
  }
}

#' Get \code{MacroPatch} EggQ (Egg Batches)
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
get_PatchesEggQ <- function(ixP = NULL){
  if(is.null(ixP)){
    return(private$PatchesEggQ)
  } else {
    return(private$PatchesEggQ[[ixP]])
  }
}

#' Set \code{MacroPatch} EggQ (Egg Batches)
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
set_PatchesEggQ <- function(PatchesEggQ, ixP = NULL){
  if(is.null(ixP)){
    private$PatchesEggQ = PatchesEggQ
  } else {
    private$PatchesEggQ[[ixP]] = PatchesEggQ
  }
}
