###############################################################################
#
#      ___   ____  __  _____
#     /   | / __ \/ / / /   |
#    / /| |/ / / / / / / /| |
#   / ___ / /_/ / /_/ / ___ |
#  /_/  |_\___\_\____/_/  |_|
#
#   MASH-MICRO
#   AQUATIC ECOLOGY: Emerge Methods
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################


# #################################################################
# # Lambda
# #################################################################
#
# #' MICRO \code{\link{AquaticSite}} Method: Get Lambda
# #'
# #' Get either a single day lambda or entire vector
# #' This method is bound to \code{AquaticSite$get_lambda()}.
# #'
# #' @param ixQ if \code{NULL} return the entire vector of lambda, else, return the value corresponding to day \code{ix}
# get_lambda_Emerge <- function(ix = NULL){
#   if(is.null(ix)){
#     return(private$lambda)
#   } else {
#     return(private$lambda[ix])
#   }
# }
#
#
# #' Aquatic Ecology \code{\link{AquaticSite}} Method: Set Lambda
# #'
# #' Set lambda.
# #' This method is bound to \code{AquaticSite$set_lambda()}.
# #'
# #' @param lambda numeric vector
# #'
# set_lambda_Emerge <- function(lambda){
#   private$lambda = lambda
# }


#################################################################
# One Step 'Emerge'
#################################################################

#' MICRO \code{\link{AquaticSite}} Method: Emerge One Day Dynamics
#'
#' Calculate emerging adults for a single aquatic habitat and add them to that site's ImagoQ.
#'  * This method is bound to \code{AquaticSite$oneStep_EmergeSite} and \code{periDomestic_AquaticSite$oneStep_EmergeSite}
#'
#' @param tNow current global time of \code{\link{MicroTile}}
oneStep_EmergeSite <- function(tNow){

  lambdaExact = private$lambda[floor(tNow)%%365+1]
  lambdaEmerge = rpois(n = 1, lambda = lambdaExact)
  if(lambdaEmerge > 0){
    private$ImagoQ$add_ImagoQ(N_new = lambdaEmerge, tEmerge_new = tNow, genotype_new = 1L)
  }

}


#' MICRO \code{\link{Landscape}} Method: Emerge One Day Dynamics
#'
#' Calculate emerging adults for a single aquatic habitat and add them to that site's ImagoQ for all sites.
#' This function fills a generic for \code{\link{simMICRO_oneStep}}.
#'  * This method is bound to \code{Landscape$oneStep_AquaticEcology()}.
#'
#' @md
oneStep_Emerge <- function(){
  tNow = private$TilePointer$get_tNow()
  # independent aquatic habitats
  for(ixA in 1:self$get_AquaSitesN()){
    private$AquaSites[[ixA]]$oneStep_EmergeSite(tNow)
  }
  # peri-domestic habitats
  for(i in 1:private$FeedingSitesN){
    if(!is.null(private$FeedingSites[[i]]$get_periDomestic())){
      private$FeedingSites[[i]]$get_periDomestic()$oneStep_EmergeSite(tNow)
    }
  }
}


#' MICRO \code{\link{Landscape}} Method: Get Emerging Adults from ImagoQ and Zero out ImagoQ
#'
#' Grab emerging adult batches where tEmerge <= tNow and add to the \code{\link{MosquitoPopFemale}}.
#' This is a helper function for \code{\link{addCohort_MicroEmerge}}.
#'  * This method is bound to \code{AquaticSite$addCohort_MicroEmergeSite()}.
#'
#' @md
addCohort_EmergeSite <- function(tNow, locNow, pSetNow = "l"){
  # use tNow in the TILE and see who is ready to be taken from ImagoQ into the MosyPop.
  EmergingAdults = private$ImagoQ$get_ImagoQTime(tNow = tNow,clear = TRUE)

  if(length(EmergingAdults) > 0){
    for(i in 1:length(EmergingAdults)){
      private$LandscapePointer$get_FemalePopPointer()$push_pop(N = EmergingAdults[[i]]$N, tEmerge = EmergingAdults[[i]]$tEmerge, pSetNow = pSetNow, locNow = locNow, genotype = EmergingAdults[[i]]$genotype)
      if(!is.null(private$LandscapePointer$get_MalePopPointer())){
        private$LandscapePointer$get_MalePopPointer()$push_pop(N = EmergingAdults[[i]]$N, tEmerge = EmergingAdults[[i]]$tEmerge, pSetNow = pSetNow, locNow = locNow, genotype = EmergingAdults[[i]]$genotype)
      }
    }
  }

  rm(EmergingAdults)
}


#' MICRO \code{\link{Landscape}} Method: Get Emerging Adults from ImagoQ and Zero out ImagoQ
#'
#' Grab emerging adult batches where tEmerge <= tNow and add to the \code{\link{MosquitoPopFemale}}.
#' This function fills a generic for \code{\link{simMICRO_oneStep}}.
#'  * This method is bound to \code{Landscape$addCohort_AquaticEcology()}
#'
#'
#' @md
addCohort_Emerge <- function(){
  tNow = private$TilePointer$get_tNow()
  # independent aquatic habitats
  for(i in 1:self$get_AquaSitesN()){
    private$AquaSites[[i]]$addCohort_MicroEmergeSite(tNow,locNow=i,pSetNow="l")
  }
  # peri-domestic habitats
  for(i in 1:private$FeedingSitesN){
    if(!is.null(private$FeedingSites[[i]]$get_periDomestic())){
      private$FeedingSites[[i]]$get_periDomestic()$addCohort_MicroEmergeSite(tNow,locNow=i,pSetNow="f")
    }
  }
}
