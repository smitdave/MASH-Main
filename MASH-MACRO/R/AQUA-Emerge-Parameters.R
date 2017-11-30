###############################################################################
#       ___
#      /   | ____ ___  ______ _
#     / /| |/ __ `/ / / / __ `/
#    / ___ / /_/ / /_/ / /_/ /
#   /_/  |_\__, /\__,_/\__,_/
#            /_/
#
#   MASH-MACRO
#   AquaPop Emerge Parameters
#   MASH Team
#   November 2017
#
###############################################################################


#' Generate Parameters for Emerge Aquatic Ecology Model
#'
#' Make parameters for Emerge
#'
#' @param nPatch integer number of patches
#' @param lambda seasonal mean emergence for each patch
#' @param seasonality boolean flag for seasonality
#' @param offset optional offset in peak emergence
#'
#' @export
AquaPop_Emerge.Parameters <- function(nPatch, lambda, seasonality = TRUE, offset = NULL){
  if(length(lambda)!=nPatch){
    stop("number of patches and length of lambda vector should be identical")
  }

  out = list()
  out$model = "Emerge"
  out$Patches = vector(mode="list",length=nPatch)
  if(is.null(offset)){offset = rep(0,nPatch)}

  for(i in 1:nPatch){
    if(seasonality){
      out$Patches[[i]]$lambda = lambda[i]*(1+sin(2*pi*(c(1:365)-offset[i])/365))
    } else {
      out$Patches[[i]]$lambda = rep(lambda[i],365)
    }
  }
  return(out)
}
