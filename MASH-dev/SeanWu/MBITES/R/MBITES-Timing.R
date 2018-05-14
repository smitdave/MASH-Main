###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MBITES-Timing
#   MASH Team
#   March 2018
#
###############################################################################

#' MBITES: Timing
#'
#' @section Time to Event Sampling:
#' In MBITES a distribution of waiting times to the next launch, \code{tNext} (a launch signifies the start of a new attempt or search bout)
#' is given by a function in the global parameters object. The function when defined must be given the parameters of the
#' distribution and return a closure that takes a single argument \code{t}, the current local time of the mosquito.
#'
#' \code{\link{mbites_timing}} is used to sample the waiting time distribution during \code{\link{mbites_updateState}} after
#' energetics and survival are calculated and new states or searches are potentially queued.
#'
#' Within the timing function, after a new time to launch is sampled, time-dependent events are potentially checked
#' depending on model specification. This includes mating behavior in \code{\link{mbites_findSwarm}} and estivation
#' in \code{\link{mbites_checkEstivation1}} or \code{\link{mbites_checkEstivation2}}
#'
#' @name MBITES-Timing
NULL
#> NULL


###############################################################################
# Time to Event Sampling
###############################################################################

#' MBITES: Time to Launch Sampling
#'
#' Sample my time of next launch.
#'  * This method is bound to \code{Mosquito$timing}.
#'
mbites_timing <- function(){

  if(private$state != 'D'){

    # # sample time to next launch, conditional on search and behavioral state
    # if(private$search){
    #   switch(private$state, # time to search bout
    #     B = {private$tNext = MBITES:::Parameters$ttEvent$BoutBs(private$tNow)},
    #     O = {private$tNext = MBITES:::Parameters$ttEvent$BoutOs(private$tNow)},
    #     S = {private$tNext = MBITES:::Parameters$ttEvent$BoutSs(private$tNow)}
    #   )
    # } else {
    #   switch(private$state, # time to attempt bout
    #     B = {private$tNext = MBITES:::Parameters$ttEvent$BoutB(private$tNow)},
    #     O = {private$tNext = MBITES:::Parameters$ttEvent$BoutO(private$tNow)},
    #     S = {private$tNext = MBITES:::Parameters$ttEvent$BoutS(private$tNow)}
    #   )
    # }

    # irrelevant to check if search or not because we update timing on what the mosy did in the current bout
    switch(private$state, # time to attempt bout
      B = {private$tNext = MBITES:::Parameters$ttEvent$BoutB(private$tNow)},
      O = {private$tNext = MBITES:::Parameters$ttEvent$BoutO(private$tNow)},
      S = {private$tNext = MBITES:::Parameters$ttEvent$BoutS(private$tNow)}
    )
  }
}


###############################################################################
# Find Mating Swarms
###############################################################################

#' MBITES: Check Mating Event
#'
#' If timing of next launch occurs during a period when mating swarms are emerging, go into mating bout
#' if immature.
#'  * This method is bound to \code{Mosquito_Female$timing}.
#'
mbites_findSwarm <- function(){
  if(!private$mature){
    # tSwarm is a time of day
    tSwarm = MBITES:::Parameters$get_tSwarm()
    T1 = private$tNow - floor(private$tNow)
    T2 = private$tNext - private$tNow
    if((T1<tSwarm) & (T1+T2 > tSwarm)){
      private$state = "M"
      private$tNext = floor(private$tNow) + tSwarm
    }
    if((tSwarm>T1) & (T1+T2 > 1+tSwarm)){
      private$state = "M"
      private$tNext = floor(private$tNow) + tSwarm
    }
  }
}

#' MBITES: Null Check Mating Event
#'
#' If mating is turned off, do nothing.
#'  * This method is bound to \code{Mosquito_Female$findSwarm}.
#'
mbites_findSwarmNull <- function(){
  # dont do anything if mating is off
}


###############################################################################
# Estivation
###############################################################################

# estivation model 1: probabilistic entry to 'estivating' state

#' MBITES: Daily Probability of Estivation
#'
#' Get the daily probability of entering estivation stage of life cycle.
#'  * This method is bound to \code{Mosquito$prEstivate}
#'
mbites_prEstivate <- function(){
  pmax(0, cos(2*pi*(MBITES:::Globals$get_tNow()-MBITES:::Parameters$get_Emax())/365) - MBITES:::Parameters$get_Eb())
}

#' MBITES: Random Wake-up Time
#'
#' Randomly sample a time to wake-up from estivation.
#'  * This method is bound to \code{Mosquito$wakeUpTime}
#'
mbites_wakeUpTime <- function(){
  rnorm(1, MBITES:::Parameters$get_eEndm(), MBITES:::Parameters$get_eEndSd())
}

#' MBITES: Probabilistic Estivation
#'
#' Queue estivation based on daily probability to enter estivation stage of the life cycle,
#' if estivation is queued, the next launch is set to be at random time in the future.
#'
#'  * This method is bound to \code{Mosquito$checkEstivation}
#'
mbites_checkEstivation1 <- function(){
  if(runif(1) < self$prEstivate()){ #attempt to estivate; if succeed set a wake up time
    if(runif(1) < MBITES:::Parameters$get_Ep()){ # survive estivation?
      private$tNext = wakeUpTime()
    } else {
      private$state = "D"
    }
  }
}

# estivation model 2: hard cut-off

#' MBITES: Estivation based on Hard Cut-off
#'
#' The number estivationDay is a day of the year. This
#' method checks to see if tNow < estivationDay < tNext.
#' If so then, the mosquito estivates, which sets
#' tNext to a random number in the future, ttEstivate()
#'
#'  * This method is bound to \code{Mosquito$checkEstivation}
#'
mbites_checkEstivation2 <- function(){
  #estivationDay is a day of the year, 0 <= estivationDay  <= 365
  estivationDay = MBITES:::Parameters$get_estivationDay()
  T1 = private$tNow%%365
  T2 = private$tNext - private$tNow
  if(T1<estivationDay & T1+T2 > estivationDay){
    private$tNext =  private$tNext + MBITES:::Parameters$ttEvent$Estivate()
  }
}

# null estivation

#' MBITES: Null Estivation
#'
#' If estivation is turned off, do nothing.
#'  * This method is bound to \code{Mosquito$checkEstivation}
mbites_checkEstivationNull <- function(){
  # dont do anything if estivation is off.
}
