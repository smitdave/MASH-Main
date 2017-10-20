#################################################################
#
#   MASH
#   M-BITES
#   Host Choosing Methods
#   David Smith, Hector Sanchez, Sean Wu
#   July 28, 2017
#
#################################################################

#' MBITES-Generic: Host Choosing for \code{\link{MicroMosquitoFemale}}
#'
#' Choose a human or animal host at a site.
#'  * This method is bound to \code{MicroMosquitoFemale$chooseHost()}.
#' @md
mbitesGeneric_chooseHost <- function(){

  if(private$inPointSet != "f"){ #check M is in a feeding site
    stop(paste0("chooseHost error; mosy ",M$id," inPointSet: ",M$inPointSet," , not in a feeding site"))
  }

  # this can probably eventually be put into C++; see the wiki
  whoIx = private$LandscapePointer$get_FeedingSites(private$ix)$get_RiskQ()$get_who()
  pTmIx = private$LandscapePointer$get_FeedingSites(private$ix)$get_RiskQ()$get_pTm()
  wIx = private$LandscapePointer$get_FeedingSites(private$ix)$get_RiskQ()$get_w()

  # non-human hosts
  nO = private$LandscapePointer$get_FeedingSites(private$ix)$get_RiskQ()$get_nOther()
  if(nO > 0){
    otherHosts = private$LandscapePointer$get_FeedingSites(private$ix)$get_RiskQ()$get_OtherHost()
    for(i in 1:nO){
      pTmIx = c(pTmIx,1)
      whoIx = c(whoIx,otherHosts$typeID[i])
      wIx = c(wIx,otherHosts$otherW[i])
    }
  }

  private$hostID = sampleIx_utility(x = whoIx,size = 1,prob = wIx*pTmIx) #select a host
}

#' MBITES-Cohort: Host Choosing for \code{\link{MicroMosquitoFemale}}
#'
#' Choose a human or animal host for MBITES-Cohort algorithms (moquito life-cycle simulations).
#'  * This method is bound to \code{MicroMosquitoFemale$chooseHost()}.
#' @md
mbitesCohort_chooseHost <- function(){

  if(private$inPointSet != "f"){ #check M is in a feeding site
    stop(paste0("chooseHost error; mosy ",M$id," inPointSet: ",M$inPointSet," , not in a feeding site"))
  }

  # human host
  if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("Q")){
    private$hostID = 1L
  # zoo host
  } else {
    private$hostID = -1L
  }
}

#' Sample Indices
#'
#' Wrapper for \code{\link{sample}} that will not lead to unexpected behavior when \code{x} is of length 1.
#'  * This function is used in \code{\link{mbitesGeneric_chooseHost}}
#' @md
#' @export
sampleIx_utility <- function(x, ...){
  return(
    x[sample.int(length(x), ...)]
  )
}
