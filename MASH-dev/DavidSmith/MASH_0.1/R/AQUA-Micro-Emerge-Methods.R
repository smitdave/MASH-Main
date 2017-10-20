#################################################################
#
#   MASH
#   MICRO Aquatic Ecology: Emerge
#   Method definitions
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   May 10, 2017
#
#################################################################

#################################################################
# Standalone functions for lambda
#################################################################

#' MICRO: Generate Seasonal Emergence (Lambda) for \code{Emerge} model of Aquatic Ecology
#'
#' Generate lambda for all sites.
#'
#' @param aquaPars a list of the following structure
#'  * N: number of aquatic habitats (required)
#'  * lambda: number of emerging adult females per human per day averaged over one year for the entire \code{\link{Landscape}} (required)
#'  * lambdaWeight: vector of weights applied to each site (if not specified or set to \code{NULL} initialize to Gamma(1,1) distribution)
#'  * offset: vector of seasonal offsets in peak emergence applied to each site (if not specified or set to \code{NULL} initialize to 0 for all sites)
#' @md
#' @return list \code{lambda} where each element is the daily emergence for that \code{\link{FeedingSite}}
#' @examples
#' makeLambda_MicroEmerge(aquaPars= list(lambda = c(2,3,4)))
#' @export
makeLambda_MicroEmerge <- function(aquaPars){

  with(aquaPars,{

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
# Lambda
#################################################################

#' MICRO \code{\link{AquaticSite}} Method: Get Lambda
#'
#' Get either a single day lambda or entire vector
#' This method is bound to \code{AquaticSite$get_lambda()}.
#'
#' @param ixQ if \code{NULL} return the entire vector of lambda, else, return the value corresponding to day \code{ix}
get_lambda_MicroEmerge <- function(ix = NULL){
  if(is.null(ix)){
    return(private$lambda)
  } else {
    return(private$lambda[ix])
  }
}


#' MICRO \code{\link{AquaticSite}} Method: Set Lambda
#'
#' Set either a single day lambda or entire vector
#' This method is bound to \code{AquaticSite$set_lambda()}.
#'
#' @param lambda the object to insert; if \code{ix = NULL} then it should be vector of lambda values, see \code{\link{makeLambda_Macro}} for details, else it should be a numeric value.
#' @param ixQ if \code{NULL} set the entire ImagoQ, else, set the slot \code{ixQ}
set_lambda_MicroEmerge <- function(lambda, ix = NULL){
  if(is.null(ix)){
    private$lambda = lambda
  } else {
    private$lambda[ix] = lambda
  }
}


#################################################################
# One Step 'Emerge'
#################################################################

#' MICRO \code{\link{AquaticSite}} Method: Emerge One Day Dynamics
#'
#' Calculate emerging adults for a single aquatic habitat and add them to that site's ImagoQ.
#' This method is bound to \code{AquaticSite$oneStep_EmergeSite()}.
#'
#' @param tNow current global time of \code{\link{MicroTile}}
oneStep_MicroEmergeSite <- function(tNow){

  lambdaExact = private$lambda[floor(tNow)%%365+1]
  lambdaEmerge = rpois(n = 1, lambda = lambdaExact)
  if(lambdaEmerge > 0){
    private$ImagoQ$add_ImagoQ(N_new = lambdaEmerge, tEmerge_new = tNow, genotype_new = -1L, damID_new = "-1", sireID_new = "-1")
  }

}


#' MICRO \code{\link{Landscape}} Method: Emerge One Day Dynamics
#'
#' Calculate emerging adults for a single aquatic habitat and add them to that site's ImagoQ for all sites.
#' This function fills a generic for \code{\link{simMICRO_oneStep}}.
#'  * This method is bound to \code{Landscape$oneStep_AquaticEcology()}.
#'
#' @md
oneStep_MicroEmerge <- function(){
  tNow = private$TilePointer$get_tNow()
  for(ixA in 1:self$AquaSitesN){
    private$AquaSites[[ixA]]$oneStep_EmergeSite(tNow)
  }
}


#' MICRO \code{\link{Landscape}} Method: Get Emerging Adults from ImagoQ and Zero out ImagoQ
#'
#' Grab emerging adult batches where tEmerge <= tNow and add to the \code{\link{MicroMosquitoPopFemale}}.
#' This is a helper function for \code{\link{addCohort_MicroEmerge}}.
#'  * This method is bound to \code{AquaticSite$addCohort_MicroEmergeSite()}.
#'
#' @md
addCohort_MicroEmergeSite <- function(tNow){
  # use tNow in the TILE and see who is ready to be taken from ImagoQ into the MosyPop.
  EmergingAdults = private$ImagoQ$get_ImagoQTime(tNow = tNow,clear = TRUE)

  if(length(EmergingAdults) > 0){
    for(i in 1:length(EmergingAdults)){
      private$LandscapePointer$get_FemalePopPointer()$push_pop(N = EmergingAdults[[i]]$N, tEmerge = EmergingAdults[[i]]$tEmerge, ix = private$ix, genotype = EmergingAdults[[i]]$genotype, damID = EmergingAdults[[i]]$damID, sireID = EmergingAdults[[i]]$sireID)
    }
  }

  rm(EmergingAdults)
}


#' MICRO \code{\link{Landscape}} Method: Get Emerging Adults from ImagoQ and Zero out ImagoQ
#'
#' Grab emerging adult batches where tEmerge <= tNow and add to the \code{\link{MicroMosquitoPopFemale}}.
#' This function fills a generic for \code{\link{simMICRO_oneStep}}.
#'  * This method is bound to \code{Landscape$addCohort()}
#'
#'
#' @md
addCohort_MicroEmerge <- function(){
  tNow = private$TilePointer$get_tNow()
  for(ixA in 1:self$AquaSitesN){
    private$AquaSites[[ixA]]$addCohort_MicroEmergeSite(tNow)
  }
}
