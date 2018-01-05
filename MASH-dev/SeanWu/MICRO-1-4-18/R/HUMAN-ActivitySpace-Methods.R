###############################################################################
#
#       __  ____  ____  ______    _   __
#      / / / / / / /  |/  /   |  / | / /
#     / /_/ / / / / /|_/ / /| | /  |/ /
#    / __  / /_/ / /  / / ___ |/ /|  /
#   /_/ /_/\____/_/  /_/_/  |_/_/ |_/
#
#   MASH-MICRO
#   HUMAN: Activity Space Methods
#   MASH-MICRO Team
#   September 10, 2017
#
###############################################################################


###############################################################################
# Getters & Setters
###############################################################################

#' Get \code{\link[MASHmacro]{Human}} ActivitySpace
#'
#' This function is bound to \code{Human$get_ActivitySpace()}
#'
get_ActivitySpace_Human <- function(){
  return(private$ActivitySpace)
}

#' Set \code{\link[MASHmacro]{Human}} ActivitySpace
#'
#' This function is bound to \code{Human$set_ActivitySpace()}
#'
#' @param nDaily integer average daily number of other sites visited
#' @param Nplaces integer number of places visited
#' @param p weight numeric on proportion of time spent at home
#' @param loc integer vector of other sites visited
#'
set_ActivitySpace_Human <- function(nDaily, Nplaces, p, loc){
  private$ActivitySpace$nDaily = nDaily
  private$ActivitySpace$Nplaces = Nplaces
  private$ActivitySpace$p = p
  private$ActivitySpace$loc = loc
}


###############################################################################
# Initialize Activity Space
###############################################################################

#' Initialize Activity Space for \code{\link[MASHmacro]{HumanPop}}
#'
#' This function is bound to \code{HumanPop$init_ActivitySpace()}
#'
#' @param nDaily average daily number of other sites visited
#'
init_ActivitySpace_HumanPop <- function(nDaily = 1.4){

  # clear out activity space
  private$LandscapePointer$clear_RiskQ()

  private$pop$apply(tag="init_ActivitySpace",returnVal=FALSE,nDaily=nDaily)

}

#' Initialize Activity Space for \code{\link[MASHmacro]{Human}}
#'
#' This function is bound to \code{Human$init_ActivitySpace()}
#'
#' @param nDaily average daily number of other sites visited
#'
init_ActivitySpace_Human <- function(nDaily){

  Nplaces = 1 + rpois(n=1,lambda=3)
  Nplaces = min(Nplaces,(private$LandscapePointer$get_FeedingSitesN()-1L))
  p = rbeta(n=1,shape1=100,shape2=6)
  loc = sample(x = (1:private$LandscapePointer$get_FeedingSitesN())[-private$houseID],size = Nplaces)

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
sim_ActivitySpace_HumanPop <- function(){

  # zero out all risk queues
  private$LandscapePointer$clear_RiskQ()

  # simulate ActivitySpace for each human
  private$pop$apply(tag="sim_ActivitySpace",returnVal=FALSE)

}

#' MICRO \code{\link{Human}} Method: Initialize Activity Space
#'
#' This function is bound to \code{Human$sim_ActivitySpace()}
#'
sim_ActivitySpace_Human <- function(){

  # add risk at home site
  pD = rbeta(n=1,shape1=100,shape2=100*(1-private$ActivitySpace$p)/private$ActivitySpace$p) # proportion of time at home site today
  private$LandscapePointer$get_FeedingSites(private$houseID)$get_RiskQ()$add_HumanHost(who_new = private$myID, pTm_new = pD, w_new = private$bWeight) # add home site risk to Landscape$FeedingSite

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



#################################################################
# Check Activity Space
#################################################################

#' Return Human Activity Space on a Tile
#'
#' Return the human activity space for each site.
#'  * This function is bound to \code{Tile$get_ActivitySpace()}
#'
get_ActivitySpace_Tile <- function(){

  TileActivity = vector(mode="list",length=private$Landscape$get_FeedingSitesN())

  for(i in 1:length(TileActivity)){
    TileActivity[[i]]$siteID = i
    TileActivity[[i]]$human = private$Landscape$get_FeedingSites(i)$get_RiskQ()$get_HumanHost()
    TileActivity[[i]]$zoo = private$Landscape$get_FeedingSites(i)$get_RiskQ()$get_OtherHost()
  }

  return(TileActivity)
}


#' Write Human Activity Space on a Tile
#'
#' Write the human activity space for each site to JSON.
#'  * This function is bound to \code{Tile$write_ActivitySpace()}
#'
write_ActivitySpace_Tile <- function(){

  con = file(description=paste0(private$HumanDirectory,"ActivitySpace_Run",private$runID,"_Time",private$tNow,".json"),open="wt")
  writeLines(text = jsonlite::toJSON(x = self$get_ActivitySpace(),pretty = TRUE),con = con)
  close(con)

}
