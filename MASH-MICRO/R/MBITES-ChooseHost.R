###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES: ChooseHost
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################

#' MBITES-Generic: Host Choosing for \code{\link{MosquitoFemale}}
#'
#' Choose a human or animal host at a site.
#'  * This method is bound to \code{MosquitoFemale$chooseHost()}.
#'
mbites_chooseHost <- function(){

  if(private$pSetNow != "f"){ #check M is in a feeding site
    cat("chooseHost error; mosy ",private$id," inPointSet: ",private$pSetNow," , not in a feeding site\n",sep="")
  }

  # this can probably eventually be put into C++; see the wiki
  whoIx = private$LandscapePointer$get_FeedingSites(private$locNow)$get_RiskQ()$get_who()
  pTmIx = private$LandscapePointer$get_FeedingSites(private$locNow)$get_RiskQ()$get_pTm()
  wIx = private$LandscapePointer$get_FeedingSites(private$locNow)$get_RiskQ()$get_w()

  # non-human hosts
  nO = private$LandscapePointer$get_FeedingSites(private$locNow)$get_RiskQ()$get_nOther()
  if(nO > 0){
    otherHosts = private$LandscapePointer$get_FeedingSites(private$locNow)$get_RiskQ()$get_OtherHost()
    for(i in 1:nO){
      pTmIx = c(pTmIx,1)
      whoIx = c(whoIx,otherHosts$typeID[i])
      wIx = c(wIx,otherHosts$otherW[i])
    }
  }

  private$hostID = sampleIx_utility(x = whoIx,size = 1,prob = wIx*pTmIx) #select a host
}

#' MBITES-Cohort: Host Choosing for \code{\link{MosquitoFemale}}
#'
#' Choose a human or animal host for MBITES-Cohort algorithms (moquito life-cycle simulations).
#'  * This method is bound to \code{MosquitoFemale$chooseHost()}.
#'
mbitesCohort_chooseHost <- function(){

  if(private$pSetNow != "f"){ #check M is in a feeding site
    cat("chooseHost error; mosy ",private$id," inPointSet: ",private$pSetNow," , not in a feeding site\n",sep="")
  }

  # human host
  if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("Q")){
    private$hostID = "human"
  # zoo host
  } else {
    private$hostID = "zoo"
  }
}
