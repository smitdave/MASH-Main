###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES-Male: Bouts
#   MASH-MICRO Team
#   September 18, 2017
#
###############################################################################


###############################################################################
# MBITES-Male: Timing
###############################################################################

#' MBITES-Male: Exponential Timing for \code{MosquitoMale}
#'
#' Method for exponentially-distributed bout lengths (model mosquitoes as a Markov process).
#'  * This method is bound to \code{MosquitoMale$timingExponential()}.
#'
mbitesMale_timingExponential <- function(){
  if(self$isActive()){
    duration = switch(private$state,
      M = {private$MalePopPointer$get_MBITES_PAR("M_time")},
      S = {private$MalePopPointer$get_MBITES_PAR("S_time")},
      R = {private$MalePopPointer$get_MBITES_PAR("R_time")}
    )
    private$tNext = private$tNow + rexp(n=1,rate=1/duration)
  }
}

#' MBITES-Male: Gamma Timing for \code{MosquitoMale}
#'
#' Method for Gamma-distributed bout lengths (model mosquitoes as a semi-Markov process).
#'  * This method is bound to \code{MosquitoMale$timingExponential()}.
#'
mbitesMale_timingGamma <- function(){
  if(self$isActive()){
    duration = switch(private$state,
      M = {private$MalePopPointer$get_MBITES_PAR("M_time")},
      S = {private$MalePopPointer$get_MBITES_PAR("S_time")},
      R = {private$MalePopPointer$get_MBITES_PAR("R_time")}
    )
    private$tNext = private$tNow + rgamma(n=1,shape=private$MalePopPointer$get_MBITES_PAR("gammaShape"),rate=(1/duration)*private$MalePopPointer$get_MBITES_PAR("gammaShape"))
  }
}


###############################################################################
#  MBITES-Male: Post-bout Landing, House Entering, and Resting
###############################################################################

#' MBITES-Male: Return Site Type \code{MosquitoMale}
#'
#' Method to return integer corresponding to site type of \code{\link{MicroSite}} this mosquito is currently at.
#'  Site Types:
#'  * 1: peri-domestic site
#'  * 0: not peri-domestic site
#'
#'  * This method is bound to \code{MosquitoMale$get_MySiteType()}.
#'
#' @return vector of landing spot weights
mbitesMale_get_MySiteType <- function(){
  switch(private$inPointSet,
    m = {return(private$LandscapePointer$get_FeedingSites(private$ix)$get_siteType())},
    s = {return(private$LandscapePointer$get_AquaSites(private$ix)$get_siteType())},
    {stop("illegal point set for MBITES-Male")}
  )
}

#' MBITES-Male: Return Landing Spot Weights for \code{MosquitoMale}
#'
#' Method for return landing spot weights based on behavioral state of mosquito.
#'  * This method is bound to \code{MosquitoMale$get_WTS()}.
#'
#' @return vector of landing spot weights
mbitesMale_get_WTS <- function(){
  switch(private$state,
    M = private$MalePopPointer$get_MBITES_PAR("M_wts"),
    S = private$MalePopPointer$get_MBITES_PAR("S_wts"),
    R = private$MalePopPointer$get_MBITES_PAR("R_wts")
  )
}

#' MBITES-Male: Generate New Landing Spot for \code{MosquitoMale}
#'
#' Method for return a new landing spot based on behavioral state of mosquito and weights from \code{\link{mbitesMale_get_WTS}}.
#' If the site is not peri-domestic the mosquito always rests on vegetation.
#'  * i: rest on the inside wall of a structure
#'  * w: rest on the outside wall of a structure
#'  * v: rest on vegetation
#'  * r: reattempt without resting
#'  * l: leave the area
#'
#'  * This method is bound to \code{MosquitoMale$newSpot()}.
#'
#' @return integer value corresponding to new landing spot
mbitesMale_newSpot <- function(){
  if(self$get_MySiteType() == 1){
    probs = private$MalePopPointer$get_MBITES_PAR("InAndOut")[private$lspot,] * self$get_WTS()
    sample(x = private$MalePopPointer$get_MBITES_PAR("lspot"),size = 1,prob = probs)
  } else {
    return("v")
  }
}

#' MBITES-Male: Attempt to Enter a House for \code{MosquitoMale}
#'
#' Method to simulate attempted house entry for mosquito, and call appropriate events if the mosquito enters.
#'  * This method is bound to \code{MosquitoMale$enterHouse()}.
#'
mbitesMale_enterHouse <- function(){
  if(runif(1) < private$LandscapePointer$get_FeedingSites(private$ix)$get_enterP()){
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

#' MBITES-Male: Land After Flight \code{MosquitoMale}
#'
#' Mosquito lands after a flight (choose a landing spot), which may cause various events.
#' This function always calls \code{\link{mbitesMale_newSpot}} and may call \code{\link{mbitesMale_enterHouse}}
#' Landing spots include:
#'  * i: 1 rest on the inside wall of a structure
#'  * w: 2 rest on the outside wall of a structure
#'  * v: 3 rest on vegetation
#'  * r: 4 reattempt without resting
#'  * l: 5 leave the area
#'
#'  * This method is bound to \code{MosquitoMale$landingSpot()}.
#'
mbitesMale_landingSpot <- function(){
  if(self$isActive()){
    oldSpot = private$lspot
    private$lspot = self$newSpot() # choose new lspot
    if(oldSpot != "i" & private$lspot == "i"){
      self$enterHouse() # enterHouse
    }
  }
}
