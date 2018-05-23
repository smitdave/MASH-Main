###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Auxiliary functions for M-BITES (not classes)
#     MBITES Team
#     March 2018
#
###############################################################################


###############################################################################
# Sample
###############################################################################

#' Sample Indices
#'
#' Wrapper for \code{\link{sample}} that will not lead to unexpected behavior when \code{x} is of length 1.
#'  * This function is used in \code{\link{mbites_chooseHost}}
#'
#' @export
sample <- function(x, ...){
  return(
    x[sample.int(length(x), ...)]
  )
}


###############################################################################
# spatstat wrappers
###############################################################################

#' Generate Poisson Point Pattern for Landscape Sites
#'
#' This function is a low-level utility which calls \code{\link[spatstat]{rpoispp}} to generate a realization of a homogeneous Poisson point process.
#' From: \url{https://github.com/TAlexPerkins/PlosCompBio_2013}
#'
#' @param n number of points to generate
#' @param owin a sampling plane (object of class \code{\link[spatstat]{owin}})
#'
#' @export
pointsPoisson <- function(n, owin){
  ps = spatstat::rpoispp(lambda = n,win = owin)
  while(ps$n != n){
    ps = spatstat::rpoispp(lambda = n,win = owin)
  }
  return(cbind(x=ps$x,y=ps$y))
}

#' Generate Matérn Clustering Point Pattern for Landscape Sites
#'
#' This function is a low-level utility which calls \code{\link[spatstat]{rMatClust}} to generate a realization of a homogeneous Matérn cluster process.
#' From: \url{https://github.com/TAlexPerkins/PlosCompBio_2013}
#'
#' @param n number of points to generate
#' @param meanParents intensity of Poisson process for cluster centers
#' @param clusteredness control mean scatter of child points around cluster centers
#' @param owin a sampling plane (object of class \code{\link[spatstat]{owin}})
#'
#' @export
pointsClustered <- function(n, meanParents = 10, clusteredness = .25, owin){
  meanDist = clusteredness / sqrt(meanParents)
  meanChildren = n / meanParents

  ps = spatstat::rMatClust(meanParents, meanDist, meanChildren, win = owin)
  while(ps$n != n){
    ps = spatstat::rMatClust(meanParents, meanDist, meanChildren, win = owin)
  }

  return(cbind(x=ps$x,y=ps$y))
}

#' Generate Overdispersed (SSI) Point Pattern for Landscape Sites
#'
#' This function is a low-level utility which calls \code{\link[spatstat]{rSSI}} to generate a realization of a homogeneous hard-core spatial inhibition process.
#' From: \url{https://github.com/TAlexPerkins/PlosCompBio_2013}
#'
#' @param n number of points to generate
#' @param inhibitionFactor controls level of overdispersion (higher values correspond to a more overdispersed spatial point process)
#' @param owin a sampling plane (object of class \code{\link[spatstat]{owin}})
#'
#' @export
pointsOverdispersed <- function(n, inhibitionFactor = 1, owin){
  ps = spatstat::rSSI(inhibitionFactor / sqrt(n), n, win = owin)
  while(ps$n != n){
    inhibitionFactor = inhibitionFactor - .01
    ps = spatstat::rSSI(inhibitionFactor / sqrt(n), n, win = owin)
  }

  return(cbind(x=ps$x,y=ps$y))
}
