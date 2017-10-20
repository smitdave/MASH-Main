#################################################################
#
#   MASH
#   R6-ified
#   HumanPop Parameters
#   David Smith, Hector Sanchez, Sean Wu
#   May 26, 2017
#
#################################################################


#################################################################
# HumanPop generation methods
#################################################################

#' Generate Ages for \code{HumanPop}
#'
#' Generate numeric vector of ages. This function is called in \code{\link{sitePops}}
#'
#' @param N size of population (may refer to household \code{\link{FeedingSite}} or an individual \code{\link{MacroPatch}}) patch
#' @return numeric vector
#' @examples
#' hhAges(N = 10)
#' @export
siteAges <- function(N){
  a = runif(1,20,40)*365
  a =  c(a, rexp(N-1,1/20/365))
  ix = which(a > 60*365)
  while(length(ix > 0)){
    a[ix] = rexp(length(ix), 1/30/365)
    ix = which(a > 60*365)
  }
  round(a)
}

#' Generate Site Sizes for \code{HumanPop}
#'
#' Site may be defined as a MICRO \code{\link{FeedingSite}} or a MACRO \code{\link{MacroPatch}}.
#'
#' @param N number of sites (may refer to household or a \code{\link{MacroPatch}} patch)
#' @param siteSize average size of population at sites
#' @param siteMin minimum size of population at sites
#' @return list:
#' * nHumans: total human population size
#' * sitePops: vector of population size at each site
#' * siteHumanID: list of human IDs at each site
#' * homeHumanID: vector of home site ID for each human
#' * siteAges: list of human ages at each site; calculated from \code{\link{siteAges}}
#' @md
#' @examples
#' sitePops(N = 10)
#' @export
sitePops <- function(N, siteSize = 10, siteMin = 2){

  sitePops = siteMin + rpois(n=N,lambda=siteSize-siteMin)
  nHumans = sum(sitePops)

  siteHumanID = vector(mode="list",length=N)
  siteAges = vector(mode="list",length=N)
  iterID = 0
  for(ix in 1:N){
    siteHumanID[[ix]] = (iterID+1):(iterID+sitePops[ix])
    siteAges[[ix]] = siteAges(N=sitePops[ix])
    iterID = iterID + sitePops[ix]
  }

  homeHumanID = rep(1:N,times=vapply(X = siteHumanID,FUN = length,FUN.VALUE = integer(1)))

  return(
    list(
      nHumans = nHumans,
      sitePops = sitePops,
      siteHumanID = siteHumanID,
      homeHumanID = homeHumanID,
      siteAges = siteAges
    )
  )
}


#################################################################
# HumanPop Parameter Generation
#################################################################

#' MICRO: Initialize HumanPop Parameters for \code{HumanPop}
#'
#' make a list of pars for \code{\link{HumanPop}}
#'
#' @param nSite number of sites (either \code{\link{FeedingSite}} or \code{link{MacroPatch}})
#' @param bWeight numeric value (if \code{NULL} biting weights are Gamma(1,1) distributed)
#' @param siteSize average size of population at sites (passed to \code{\link{sitePops}})
#' @param siteMin minimum size of population at sites (passed to \code{\link{sitePops}})
#' @return return a list \code{HumanPop_PAR}
#' @examples
#' HumanPop.Parameters(nSite = 5)
#'  * nHumans: total human population size
#'  * sitePops: vector of population at each site
#'  * tStart: time to begin simulation
#'  * humanIDs: vector of human IDs
#'  * siteHumanIDs: list of human IDs by site
#'  * homeIDs: vector of site IDs of each human
#'  * siteAges: list of ages of humans by site
#'  * bDay: vector of birthdays of each human
#'  * bWeight: biting weight on humans
#' @md
#' @export
HumanPop.Parameters <- function(nSite, bWeight = NULL, siteSize = 10, siteMin = 2){

  demographics = sitePops(N=nSite,siteSize=siteSize,siteMin=siteMin)

  if(is.null(bWeight)){
    bWeight = rgamma(demographics$nHumans,1,1)
  } else {
    bWeight = rep(bWeight,demographics$nHumans)
  }

  list(
    nHumans = demographics$nHumans,
    sitePops = demographics$sitePops,
    tStart = 0,
    humanIDs = Reduce(f = c,x = demographics$siteHumanID),
    siteHumanIDs = demographics$siteHumanID,
    homeIDs = demographics$homeHumanID,
    steAges = demographics$siteAges,
    bDay = -Reduce(f = c,x = demographics$siteAges),
    bWeight = bWeight
  )
}
