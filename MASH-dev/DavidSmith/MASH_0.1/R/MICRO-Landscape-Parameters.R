####################################################################################
#
#   MASH
#   R6-ified
#   MICRO Landscape Class Parameters
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   May 9, 2017
#
####################################################################################


####################################################################################
# Point Clustering Patterns
####################################################################################

#' Generate Poisson Point Pattern for Landscape Sites
#'
#' This function is a low-level utility to generate spatial point patterns for the MICRO-LANDSCAPE point sets.
#'
#' @param n number of points to generate
#' @param xLim a length 2 numeric vector of bounds for the sampling grid
#' @param yLim a length 2 numeric vector of bounds for the sampling grid
#' @return a list with two elements x and y corresponding to the sampled points
#' @examples
#' pointsPoisson(n=10, xLim=c(0,1), yLim=c(0,1))
#' @export
pointsPoisson <- function(n, xLim=c(0,1), yLim=c(0,1)){
  ps = spatstat::rpoispp(lambda = n,win = spatstat::owin(xLim,yLim))
  while(ps$n != n){
    ps = spatstat::rpoispp(lambda = n,win = spatstat::owin(xLim,yLim))
  }

  return(list(x=ps$x,y=ps$y))
}

#' Generate Matern Clustering Point Pattern for Landscape Sites
#'
#' This function is a low-level utility to generate spatial point patterns for the MICRO-LANDSCAPE point sets.
#'
#' @param n number of points to generate
#' @param meanParents intensity of Poisson process for cluster centers
#' @param clusteredness control mean scatter of child points around cluster centers
#' @param xLim a length 2 numeric vector of bounds for the sampling grid
#' @param yLim a length 2 numeric vector of bounds for the sampling grid
#' @return a list with two elements x and y corresponding to the sampled points
#' @examples
#' pointsClustered(n=10, meanParents = 10, clusteredness = .25, xLim=c(0,1), yLim=c(0,1))
#' @export
pointsClustered <- function(n, meanParents = 10, clusteredness = .25, xLim=c(0,1), yLim=c(0,1)){
  meanDist = clusteredness / sqrt(meanParents)
  meanChildren = n / meanParents

  ps = spatstat::rMatClust(meanParents, meanDist, meanChildren, win = spatstat::owin(xLim,yLim))
  while(ps$n != n){
    ps = spatstat::rMatClust(meanParents, meanDist, meanChildren, win = spatstat::owin(xLim,yLim))
  }

  return(list(x=ps$x,y=ps$y))
}

#' Generate Overdispersed (SSI) Point Pattern for Landscape Sites
#'
#' This function is a low-level utility to generate spatial point patterns for the MICRO-LANDSCAPE point sets.
#'
#' @param n number of points to generate
#' @param inhibitionFactor controls level of overdispersion (higher values correspond to a more overdispersed spatial point process)
#' @param xLim a length 2 numeric vector of bounds for the sampling grid
#' @param yLim a length 2 numeric vector of bounds for the sampling grid
#' @return a list with two elements x and y corresponding to the sampled points
#' @examples
#' pointsOverdispersed(n=10, inhibitionFactor = 1, xLim=c(0,1), yLim=c(0,1))
#' @export
pointsOverdispersed <- function(n, inhibitionFactor = 1, xLim=c(0,1), yLim=c(0,1)){
  ps = spatstat::rSSI(inhibitionFactor / sqrt(n), n, win = spatstat::owin(xLim,yLim))
  while(ps$n != n){
    inhibitionFactor = inhibitionFactor - .01
    ps = spatstat::rSSI(inhibitionFactor / sqrt(n), n, win = spatstat::owin(xLim,yLim))
  }

  return(list(x=ps$x,y=ps$y))
}

#' Generate Regular Grid (Lattice) Point Pattern for Landscape Sites
#'
#' This function is a low-level utility to generate spatial point patterns for the MICRO-LANDSCAPE point sets.
#'
#' @param n number of points to generate
#' @param xLim a length 2 numeric vector of bounds for the sampling grid
#' @param yLim a length 2 numeric vector of bounds for the sampling grid
#' @return a list with two elements x and y corresponding to the sampled points
#' @examples
#' pointsLattice(n=10, xLim=c(0,1), yLim=c(0,1))
#' @export
pointsLattice <- function(n, xLim=c(0,1), yLim=c(0,1)){
  Pdim = ceiling(sqrt(n))
  lb.x = (xLim[1] + xLim[2])/Pdim; lb.x = (xLim[1] + lb.x)/2
  lb.y = (yLim[1] + yLim[2])/Pdim; lb.y = (yLim[1] + lb.y)/2
  ub.x = xLim[2] - lb.x
  ub.y = yLim[2] - lb.y

  g1 = seq(lb.x, ub.x, length.out = Pdim)
  g2 = seq(lb.y, ub.y, length.out = Pdim)
  x = as.vector(matrix(g1,Pdim,Pdim))
  y = as.vector(matrix(g1,Pdim,Pdim,byrow = T))
  return(list(x=x,y=y))
}


####################################################################################
# Hazards & Search Weights
####################################################################################

#' Find shape2 (beta) Parameter of Beta Distribution for Given Mean
#'
#' Given a user-specified mean value, \code{betaRootB} uses \code{\link{uniroot}} to find the shape2 (beta)
#' parameter of the distribution that will give that mean. Parameter shape1 may also be given as user input, but has
#' a default value of 1.
#'
#' @param mean the mean of the beta distribution
#' @param alpha = 1 alpha parameter of beta distribution
#' @return numeric value
#' @examples
#' betaRootB(mean = 0.05, alpha = 1)
#' @export
betaRootB <- function(mean, alpha = 1){
  rootOut = uniroot(f = function(x,alpha,mean){
      (alpha/(alpha+x)) - mean
  },interval = c(0,1e12),mean=mean,alpha=alpha)
  return(rootOut$root)
}

#' Find shape1 (alpha) Parameter of Beta Distribution for Given Mean
#'
#' Given a user-specified mean value, \code{betaRootA} uses \code{\link{uniroot}} to find the shape1 (alpha)
#' parameter of the distribution that will give that mean. Parameter shape2 may also be given as user input, but has
#' a default value of 1.
#'
#' @param mean the mean of the beta distribution
#' @param beta beta parameter of the beta distribution
#' @return numeric value
#' @examples
#' betaRootA(mean = 0.95, beta = 20)
#' @export
betaRootA <- function(mean, beta = 1){
  rootOut = uniroot(f = function(x,beta,mean){
      (x/(x+beta)) - mean
  },interval = c(0,1e12),mean=mean,beta=beta)
  return(rootOut$root)
}


####################################################################################
# Parameter Generation Functions
####################################################################################

#' MICRO: Generate Parameters for \code{\link{Landscape}} Object
#'
#' This function is a specific instantiation of a generic system to generate parameters for a
#' chosen landscape. Any user-specified function can be written to generate parameters, as long as the
#' return list is in the same format.
#'
#' @param nFeed number of feeding sites
#' @param nAqua number of aquatic habitats
#' @param pointGen character to select spatial point pattern generation function
#'  * "poisson": \code{\link{pointsPoisson}}
#'  * "clustered": \code{\link{pointsClustered}}
#'  * "overdispersed": \code{\link{pointsOverdispersed}}
#'  * "lattice": \code{\link{pointsLattice}}
#' @param module character
#'  * "emerge": initialize parameters for Emerge module of Aquatic Ecology
#'  * "EL4P": initialize parameters for EL4P module of Aquatic Ecology
#' @param modulePars additional list of named parameters to be passed to Aquatic Ecology module specific parameter generating functions
#'  * Emerge: should be named list of parameters for \code{\link{makeLambda_MicroEmerge}}
#'  * EL4P: NULL
#' @param hazV mean value for feeding site vegetation landing hazard (if 0 it is set to 0 for all sites)
#' @param hazW mean value for feeding site outside wall landing hazard (if 0 it is set to 0 for all sites)
#' @param hazI mean value for feeding site indoor wall landing hazard (if 0 it is set to 0 for all sites)
#' @param haz mean value for aquatic habitat landing hazard (if 0 it is set to 0 for all sites)
#' @param searchFeed vector of searchWt for feeding sites (if \code{NULL} initialize to Gamma(1,1) distribution)
#' @param searchAqua vector of searchWt for aquatic habitats (if \code{NULL} initialize to Gamma(1,1) distribution)
#' @param enterP vector of house entry probabilities or single numeric value for all sites (if \code{NULL} initialize to Beta(9,1) distribution)
#' @param xLim x-axis bounds for simulated points
#' @param yLim y-axis bounds for simulated points
#' @param aquaSD standard deviation of aquatic habitat scatter around feeding sites
#' @param ... additional named arguments for pointGen()
#' @return a named list of parameters
#' * FeedingSite_PAR: see \code{\link{Landscape.Feeding.Parameters}} for details
#' * AquaticSite_PAR: see \code{\link{Landscape.Aqua.Parameters}} for details
#' @md
#' @examples
#'
#' @export
Landscape.Parameters <- function(
    nFeed,
    nAqua,
    pointGen = "poisson",
    module,
    modulePars,
    hazV = 0,
    hazW = 0,
    hazI = 0,
    haz = 0,
    searchFeed = NULL,
    searchAqua = NULL,
    enterP = NULL,
    xLim = c(0,1),
    yLim = c(0,1),
    aquaSD = 0.025,
    ...
  ){

    # Feeding Sites
    FeedingSite_PAR = Landscape.Feeding.Parameters(nFeed=nFeed,pointGen=pointGen,searchWt=searchFeed,enterP=enterP,hazV=hazV,hazW=hazW,hazI=hazI,...)

    # Aquatic Habitats
    aquaIx = sample(x = nFeed,size = nAqua,replace = TRUE)
    aquaXY = list(
        x = rnorm(n = nAqua,mean = FeedingSite_PAR$siteXY$x[aquaIx],sd = aquaSD),
        y = rnorm(n = nAqua,mean = FeedingSite_PAR$siteXY$y[aquaIx],sd = aquaSD)
      )

    AquaticSite_PAR = Landscape.Aqua.Parameters(nAqua=nAqua,siteXY=aquaXY,module=module,modulePars=modulePars,searchW=searchAqua,haz=haz)

    Landscape_PAR = list(
      FeedingSite_PAR = FeedingSite_PAR,
      AquaticSite_PAR = AquaticSite_PAR
    )

    return(Landscape_PAR)
}


#' MICRO: Generate Parameters for \code{\link{Landscape}} \code{\link{FeedingSite}}
#'
#' This function generates a named list of parameters to initialize all \code{\link{AquaticSite}} objects on a MICRO \code{\link{Landscape}}.
#'
#' @param nFeed number of feeding sites
#' @param pointGen character to select spatial point pattern generation function
#'  * "poisson": \code{\link{pointsPoisson}}
#'  * "clustered": \code{\link{pointsClustered}}
#'  * "overdispersed": \code{\link{pointsOverdispersed}}
#'  * "lattice": \code{\link{pointsLattice}}
#' @param searchWt vector of searchWt (if \code{NULL} initialize to Gamma(1,1) distribution)
#' @param enterP vector of house entry probabilities or single numeric value for all sites (if \code{NULL} initialize to Beta(9,1) distribution)
#' @param hazV mean value for feeding site vegetation landing hazard (if 0 it is set to 0 for all sites)
#' @param hazW mean value for feeding site outside wall landing hazard (if 0 it is set to 0 for all sites)
#' @param hazI mean value for feeding site indoor wall landing hazard (if 0 it is set to 0 for all sites)
#' @param ... additional named arguments to be passed to the pointGen(nFeed, ...) function
#' @return return a list
#' @md
#' @export
Landscape.Feeding.Parameters <- function(nFeed, pointGen = "poisson", searchWt = NULL, enterP = NULL, hazV = 0, hazW = 0, hazI = 0, ...){

  Landscape_Feeding_PAR = list()
  Landscape_Feeding_PAR$nFeed = nFeed

  switch(pointGen,
    "poisson" = Landscape_Feeding_PAR$siteXY <- pointsPoisson(nFeed, ...),
    "clustered" = Landscape_Feeding_PAR$siteXY <- pointsClustered(nFeed, ...),
    "overdispersed" = Landscape_Feeding_PAR$siteXY <- pointsOverdispersed(nFeed, ...),
    "lattice" = Landscape_Feeding_PAR$siteXY <- pointsLattice(nFeed, ...)
  )

  # Search Weights and Landing Hazards
  if(is.null(searchWt)){
    Landscape_Feeding_PAR$searchWt = rgamma(n=nFeed,1,1)
  } else {
    Landscape_Feeding_PAR$searchWt = searchWt
  }

  if(hazV!=0){ # vegetation landing hazards
    beta = betaRootB(mean = hazV, alpha = 1)
    Landscape_Feeding_PAR$hazV = rbeta(n = nFeed, shape1 = 1, shape2 = beta)
  } else {
    Landscape_Feeding_PAR$hazV = rep(0,nFeed)
  }
  if(hazW!=0){ # outside wall landing hazards
    beta = betaRootB(mean = hazW, alpha = 1)
    Landscape_Feeding_PAR$hazW = rbeta(n = nFeed, shape1 = 1, shape2 = beta)
  } else {
    Landscape_Feeding_PAR$hazW = rep(0,nFeed)
  }
  if(hazI!=0){ # indoor wall landing hazards
    beta = betaRootB(mean = hazI, alpha = 1)
    Landscape_Feeding_PAR$hazI = rbeta(n = nFeed, shape1 = 1, shape2 = beta)
  } else {
    Landscape_Feeding_PAR$hazI = rep(0,nFeed)
  }

  # sugar
  Landscape_Feeding_PAR$sugar = rgamma(n = nFeed,1,1)

  # enter house
  if(is.null(enterP)){
    Landscape_Feeding_PAR$enterP = rbeta(nFeed,9,1)
  } else if(length(enterP)==nFeed){
    Landscape_Feeding_PAR$enterP = enterP
  } else if(is.numeric(enterP)){
    Landscape_Feeding_PAR$enterP = rep(enterP,nFeed)
  } else {
    stop("enterP is invalid")
  }

  return(Landscape_Feeding_PAR)
}


#' MICRO: Generate Parameters for \code{\link{Landscape}} \code{\link{AquaticSite}}
#'
#' This function generates a named list of parameters to initialize all \code{\link{AquaticSite}} objects on a MICRO \code{\link{Landscape}}.
#'
#' @param nAqua number of aquatic habitats
#' @param siteXY two element list of \code{x} and \code{y} coordinates of aquatic habitats
#' @param module character
#'  * "emerge": initialize parameters for Emerge module of Aquatic Ecology
#'  * "EL4P": initialize parameters for EL4P module of Aquatic Ecology
#' @param modulePars additional list of named parameters to be passed to Aquatic Ecology module specific parameter generating functions
#'  * Emerge: should be named list of parameters for \code{\link{makeLambda_MicroEmerge}}
#'  * EL4P: NULL
#' @param searchWt vector of searchWt (if \code{NULL} initialize to Gamma(1,1) distribution)
#' @param haz mean value of landing hazards (if \code{!= 0} use \code{\link{betaRootA}} to find alpha parameter of beta distribution to give that mean value and produce Beta distributed hazards)
#' @return return a list
#' @md
#' @export
Landscape.Aqua.Parameters <- function(nAqua, siteXY, module , modulePars, searchWt = NULL, haz = 0){

  Landscape_Aqua_PAR = list()
  Landscape_Aqua_PAR$nAqua = nAqua
  Landscape_Aqua_PAR$siteXY = siteXY

  # Search Weights and Landing Hazards
  if(is.null(searchWt)){
    Landscape_Aqua_PAR$searchWt = rgamma(n=nAqua,1,1)
  } else {
    Landscape_Aqua_PAR$searchWt = searchWt
  }

  if(haz!=0){
    alpha = betaRootA(mean = haz, beta = 1)
    Landscape_Aqua_PAR$haz = rbeta(n = nAqua, shape1 = alpha, shape2 = 1)
  } else {
    Landscape_Aqua_PAR$haz = rep(0,nAqua)
  }

  # Aquatic Ecology modules
  Landscape_Aqua_PAR$module = module
  if(module == "emerge"){
    Landscape_Aqua_PAR$lambda = makeLambda_MicroEmerge(modulePars)
  }

  return(Landscape_Aqua_PAR)
}
