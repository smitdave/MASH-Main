###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   M-BITES: Resting Spot 
#   MASH-MICRO Team
#   September 2017
#
###############################################################################

#################################################################
#  M-BITES: Post-bout Landing, House Entering, and Resting
#
#  HOUSE ENTERING & RESTING BEHAVIOR:
#  At the end of the search bout, attempt bout, or after Egg
#  laying a mosquito has entered the area around a feeding
#  station and either rested or attempted to rest:
#    l) Leave the area
#    r) Reattempt Without Resting;
#    v) Rest on vegetation
#    w) Rest on the Outside wall of a structure
#    i) Rest on the Inside wall of a structure
#
#################################################################

#' M-BITES: Return Site Type \code{MosquitoFemale}
#'
#' Method to return integer corresponding to site type of \code{\link{MicroSite}} this mosquito is currently at.
#'  Site Types:
#'  * 1: peri-domestic site
#'  * 0: not peri-domestic site
#'
#'  * This method is bound to \code{MosquitoFemale$get_MySiteType()}.
#'
#' @return vector of landing spot weights
mbites_get_MySiteType <- function(){
  switch(private$pSetNow,
    f = {return(private$LandscapePointer$get_FeedingSites(private$locNow)$get_siteType())},
    l = {return(private$LandscapePointer$get_AquaSites(private$locNow)$get_siteType())},
    m = {return(private$LandscapePointer$get_MatingSites(private$locNow)$get_siteType())},
    s = {return(private$LandscapePointer$get_SugarSites(private$locNow)$get_siteType())},
    {stop("illegal point set for M-BITES")}
  )
}

#' M-BITES: Return Landing Spot Weights for \code{MosquitoFemale}
#'
#' Method for return landing spot weights based on behavioral state of mosquito.
#'  * This method is bound to \code{MosquitoFemale$get_WTS()}.
#'
#' @return vector of landing spot weights
mbites_get_WTS <- function(){
  switch(private$state,
    F = private$FemalePopPointer$get_MBITES_PAR("F_wts"),
    B = private$FemalePopPointer$get_MBITES_PAR("B_wts"),
    R = private$FemalePopPointer$get_MBITES_PAR("R_wts"),
    L = private$FemalePopPointer$get_MBITES_PAR("L_wts"),
    O = private$FemalePopPointer$get_MBITES_PAR("O_wts"),
    M = private$FemalePopPointer$get_MBITES_PAR("M_wts"),
    S = private$FemalePopPointer$get_MBITES_PAR("S_wts"),
    {stop(cat("illegal behavioral state: ",private$state,"\n",sep=""))}
  )
}

#' M-BITES: Generate New Landing Spot for \code{MosquitoFemale}
#'
#' Method for return a new landing spot based on behavioral state of mosquito and weights from \code{\link{mbites_get_WTS}}.
#' New landing spots generated at the end of the search bout, attempt bout, or after oviposition a mosquito has entered
#' the area around a feeding site and either rested or attempted to rest. If the site is not a \code{\link{FeedingSite}} or peri-domestic the mosquito always rests on vegetation.
#'  * i: rest on the inside wall of a structure
#'  * w: rest on the outside wall of a structure
#'  * v: rest on vegetation
#'  * r: reattempt without resting
#'  * l: leave the area
#'
#'  * This method is bound to \code{MosquitoFemale$newSpot()}.
#'
#' @return integer value corresponding to new landing spot
mbites_newSpot <- function(){
  if(self$get_MySiteType() == 1){
    probs = private$FemalePopPointer$get_MBITES_PAR("InAndOut")[private$lspot,] * self$get_WTS()
    sample(x = private$FemalePopPointer$get_MBITES_PAR("lspot"),size = 1,prob = probs)
  } else {
    return("v")
  }
}

#' M-BITES: Attempt to Enter a House for \code{MosquitoFemale}
#'
#' Method to simulate attempted house entry for mosquito, and call appropriate events if the mosquito enters.
#'  * This method is bound to \code{MosquitoFemale$enterHouse()}.
#'
mbites_enterHouse <- function(){
  if(runif(1) < private$LandscapePointer$get_FeedingSites(private$locNow)$get_enterP()){
    # mosquito is inside of house
  } else {
    # mosquito is not inside of house
    private$lspot = self$newSpot()
    self$surviveFlight()
    if(private$lspot == "i"){
      Recall()
    }
  }
}

#' M-BITES: Land After Flight \code{MosquitoFemale}
#'
#' Mosquito lands after a flight (choose a landing spot), which may cause various events.
#' This function always calls \code{\link{mbites_newSpot}} and may call \code{\link{mbites_enterHouse}}
#' Landing spots include:
#'  * i: 1 rest on the inside wall of a structure
#'  * w: 2 rest on the outside wall of a structure
#'  * v: 3 rest on vegetation
#'  * r: 4 reattempt without resting
#'  * l: 5 leave the area
#'
#'  * This method is bound to \code{MosquitoFemale$restingSpot()}.
#'
mbites_restingSpot <- function(){
  if(self$isActive()){
    if(self$searchFail()){
      private$lspot = "l"
      private$search = TRUE
    } else {
      oldSpot = private$lspot
      private$lspot = self$newSpot() # choose new lspot
      if(oldSpot != "i" & private$lspot == "i"){
        self$enterHouse() # enterHouse
      }
    }
  }
}

#' M-BITES: Check State Transition for Resting Spot \code{MosquitoFemale}
#'
#' Return \code{TRUE} if mosquito stays in "L" or "F" state.
#'
#'  * This method is bound to \code{MosquitoFemale$searchFail()}.
#'
mbites_searchFail <- function(){
  age = private$tNow - private$bDay
  if(age > 0){
    # mosquitoes only leave an area if they failed their bout
    if(private$boutFail){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

