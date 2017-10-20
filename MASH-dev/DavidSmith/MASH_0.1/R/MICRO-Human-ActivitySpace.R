#################################################################
#
#   MASH
#   R6-ified
#   MICRO: Human ActivitySpace Simulation and Methods
#   David Smith, Hector Sanchez, Sean Wu
#   June 1, 2017
#
#################################################################

# see: MICRO-Human-Setup.R: MICRO.Humans.Setup() for initialization to proper classes

#################################################################
# Getters & Setters
#################################################################

#' MICRO: Get \code{\link{Human}} ActivitySpace
#'
#' This function is bound to \code{Human$get_ActivitySpace()}
#'
get_MicroHuman_ActivitySpace <- function(){
  return(private$ActivitySpace)
}

#' MICRO: Set \code{\link{Human}} ActivitySpace
#'
#' This function is bound to \code{Human$set_ActivitySpace()}
#'
#' @param nDaily average daily number of other sites visited
#' @param Nplaces number of places visited
#' @param p weight on proportion of time spent at home
#' @param loc vector of other sites visited
#'
set_MicroHuman_ActivitySpace <- function(nDaily, Nplaces, p, loc){
  private$ActivitySpace$nDaily = nDaily
  private$ActivitySpace$Nplaces = Nplaces
  private$ActivitySpace$p = p
  private$ActivitySpace$loc = loc
}


#################################################################
# Initialize Activity Space
#################################################################

#' MICRO \code{\link{HumanPop}} Method: Initialize Activity Space
#'
#' This function is bound to \code{HumanPop$init_ActivitySpace()}
#'
#' @param nDaily average daily number of other sites visited
#'
init_MicroHumanPop_ActivitySpace <- function(nDaily = 1.4){

  for(ixH in 1:self$nHumans){
    private$pop[[ixH]]$init_ActivitySpace(nDaily = nDaily)
  }

}

#' MICRO \code{\link{Human}} Method: Initialize Activity Space
#'
#' This function is bound to \code{Human$init_ActivitySpace()}
#'
#' @param nDaily average daily number of other sites visited
#'
init_MicroHuman_ActivitySpace <- function(nDaily){

  Nplaces = 1 + rpois(n=1,lambda=3)
  Nplaces = min(Nplaces,(private$LandscapePointer$FeedingSitesN-1L))
  p = rbeta(n=1,shape1=100,shape2=6)
  loc = sample(x = (1:private$LandscapePointer$FeedingSitesN)[-private$hhID],size = Nplaces)

  # set my ActivitySpace
  private$ActivitySpace$nDaily = nDaily
  private$ActivitySpace$Nplaces = Nplaces
  private$ActivitySpace$p = p
  private$ActivitySpace$loc = loc

}


#################################################################
# Simulate Activity Space
#################################################################

#' MICRO \code{\link{HumanPop}} Method: Simulate Activity Space
#'
#' This function is bound to \code{HumanPop$sim_ActivitySpace()}
#'
sim_MicroHumanPop_ActivitySpace <- function(){

  # zero out all risk queues
  private$LandscapePointer$clear_RiskQ()

  # simulate ActivitySpace for each human
  for(ixH in 1:self$nHumans){
    private$pop[[ixH]]$sim_ActivitySpace()
  }

}

#' MICRO \code{\link{Human}} Method: Initialize Activity Space
#'
#' This function is bound to \code{Human$sim_ActivitySpace()}
#'
sim_MicroHuman_ActivitySpace <- function(){

  # add risk at home site
  pD = rbeta(n=1,shape1=100,shape2=100*(1-private$ActivitySpace$p)/private$ActivitySpace$p) # proportion of time at home site today
  private$LandscapePointer$get_FeedingSites(private$hhID)$get_RiskQ()$add_HumanHost(who_new = private$myID, pTm_new = pD, w_new = private$bWeight) # add home site risk to Landscape$FeedingSite

  # add risk from visited sites
  nD = min(rpois(n=1,lambda=private$ActivitySpace$nDaily),private$ActivitySpace$Nplaces) # draw random number of other sites visited
  if(nD > 0){
    # sample sites in loc vector to visit
    fD = sample(x = private$ActivitySpace$Nplaces, size = nD, replace = FALSE)
    for(ixS in 1:nD){
      private$LandscapePointer$get_FeedingSites(private$ActivitySpace$loc[fD[ixS]])$get_RiskQ()$add_HumanHost(who_new = private$myID, pTm_new = (1-pD)/nD, w_new = private$bWeight)
    }
  }

}
