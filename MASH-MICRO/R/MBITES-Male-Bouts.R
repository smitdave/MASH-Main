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
#' @return character
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
#'  * i: rest on the inside wall of a structure
#'  * w: rest on the outside wall of a structure
#'  * v: rest on vegetation
#'  * r: reattempt without resting
#'  * l: leave the area
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


###############################################################################
# MBITES-Male: Mating Bout
###############################################################################

#' MBITES-Male: Male Mating Bout \code{MosquitoMale}
#'
#' A male mosquito performs a mating bout. If successful (governed by parameter M.s.m), the mosquito enters a mating queue (see \code{\link[MASHcpp]{MatingQ}}).
#'  * This method is bound to \code{MosquitoMale$boutM()}.
#'
mbitesMale_boutM <- function(){

  if(self$isAlive() & runif(1) < private$MalePopPointer$get_MBITES_PAR("M.s.m")){
    private$LandscapePointer$get_MatingSites(private$ix)$get_MatingQ()$add_male2Q(private$id,private$mateFitness,private$genotype)
    private$stateNew = "R"
  }

}


###############################################################################
# MBITES-Male: Sugar Feeding Bout
###############################################################################

#' MBITES-Male: Male Sugar Feeding Bout \code{MosquitoMale}
#'
#' A male mosquito attempts to sugar feed. If successful (governed by parameter S.s.m), the mosquito energy is topped off, otherwise the mosquito's energy is unchanged.
#'  * This method is bound to \code{MosquitoMale$boutS()}.
#'
mbitesMale_boutS <- function(){

  if(self$isAlive()){
    if(runif(1) < private$MalePopPointer$get_MBITES_PAR("S.s.m")){
      private$energy = 1
      private$stateNew = "R"
    } else {
      private$stateNew = "R"
    }
  }

}


###############################################################################
# MBITES-Male: Resting Bout
###############################################################################

#' MBITES-Male: Male Sugar Feeding Bout \code{MosquitoMale}
#'
#' A male mosquito attempts to sugar feed. If successful (governed by parameter R.s.m), the mosquito energy is topped off, otherwise the mosquito's energy is unchanged.
#'  * This method is bound to \code{MosquitoMale$boutS()}.
#'
mbitesMale_boutR <- function(){

  if(self$isAlive()){
    if(runif(1) < private$MalePopPointer$get_MBITES_PAR("R.s.m")){
      private$stateNew = "M"
    } else {
      private$stateNew = "D"
    }
  }

}


###############################################################################
# MBITES-Male: Generic Bout
###############################################################################

#' MBITES-Male: One Bout \code{MosquitoMale}
#'
#' Mosquito behavior has a finite set of states (state space of model), within which there are certain biological functions that are always evaluated.
#' A bout is the actions taken by a mosquito between a launch and landing; \code{mbitesMale_oneBout} handles all the biological imperatives that occur during a bout,
#' while specialized bout action methods handle the events that occur due to the purpose of the bout.
#'  * \code{\link{mbitesMale_boutM}}: male mating bout
#'  * \code{\link{mbitesMale_boutS}}: male sugar feeding bout
#'  * \code{\link{mbitesMale_boutR}}: male resting bout
#'
#' The generic bout runs necessary updates of timing, state, survival, energetics, and queue checks prior to calling the nested
#' specific bout action, and checks that the mosquito is alive/active before calling the bout. It updates \code{tNext} and \code{stateNew}.
#'
#' This corresponds to the following Gillespie-style algorithm:
#'
#' 1. tNow is set to tNext from previous bout
#' 2. moveMe: movement between point classes (if needed)
#' 3. boutFun: run bout function
#' 4. run energetics and check if alive
#' 5. run landingSpot and check if alive
#' 6. run surviveResting/surviveFlight and check if alive
#' 7. update tNext
#' 8. update state to stateNew which is determined in the bout
#'
#'  * This method is bound to \code{MosquitoMale$oneBout()}.
#'
#'
mbitesMale_oneBout <- function(){

  # update time and state
  private$tNow = private$tNext # update time
  private$state = private$stateNew # update current state
  self$timing() # update tNext

  # movement
  self$moveMe()             # SEARCH-Kernel-Methods.R

  # landing spot
  self$landingSpot()        # MBITES-Male-Bouts.R

  # bout
  switch(private$state,
    M = {self$boutM()},
    S = {self$boutS()},
    R = {self$boutR()},
    {stop("illegal behavioral state for MBITES-Male")}
  )

  # energetics
  self$sugarEnergetics()    # MBITES-Male-Energetics.R

  # survival
  self$surviveResting()     # MBITES-Male-Survival.R
  self$surviveFlight()      # MBITES-Male-Survival.R

  # log history
  private$history$historyTrack(privateEnv = private, alive = self$isAlive())

}


###############################################################################
# MBITES-Male: Simulation
###############################################################################

#' MBITES-Male: Run Simulation for \code{\link{MosquitoMale}}
#'
#' Run the M-BITES life cycle simulation algorithm while alive and has not overrun time in enclosing \code{\link{MicroTile}}.
#' This method calls \code{\link{mbitesMale_oneBout}} to simulate each life stage.
#'  * This method is bound to \code{MosquitoMale$MBITES()}.
#'
#' @md
mbitesMale_oneMosquito_MBITES <- function(){

  # run algorithm while alive and has not overrun tile time
  while(private$tNext < private$TilePointer$get_tNow() & private$stateNew != "D"){
    self$oneBout()
  }

  # if mosquito is dead output data if asked and remove it from the enclosing storage object
  if(private$stateNew == "D" & private$get_MBITES_PAR("maleHistory")){
    self$writeAndDelete(conHist = private$TilePointer$get_MaleHistoryCon())
  }

}

#' MBITES-Male: Run Simulation for \code{\link{MosquitoPopMale}}
#'
#' Run the M-BITES life cycle simulation algorithm while alive and has not overrun time in enclosing \code{\link{MicroTile}}.
#' This method calls \code{\link{mbitesMale_oneMosquito_MBITES}} one each living mosquito sequentially.
#' Prior to running simulation, this function calls \code{\link{clear_MatingQ_Landscape}} to clear mating queues.
#'  * This method is bound to \code{MosquitoPopMale$MBITES()}.
#'
#' @md
mbitesMale_Pop_MBITES <- function(){

  private$LandscapePointer$clear_MatingQ()

  private$pop$apply(tag="MBITES",returnVal=FALSE)

}
