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
#' write about me!
#'
#'
#'
#' @name Timing
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
    # NOTE :: this updates tNow

    # sample time to next launch, conditional on search and behavioral state
    if(private$search){
      switch(private$state, # time to search bout
        B = {private$tNext = MBITES:::Parameters$ttEvent_BoutBs(private$tNow)},
        O = {private$tNext = MBITES:::Parameters$ttEvent_BoutOs(private$tNow)},
        S = {private$tNext = MBITES:::Parameters$ttEvent_BoutSs(private$tNow)}
      )
    } else {
      switch(private$state, # time to attempt bout
        B = {private$tNext = MBITES:::Parameters$ttEvent_BoutB(private$tNow)},
        O = {private$tNext = MBITES:::Parameters$ttEvent_BoutO(private$tNow)},
        S = {private$tNext = MBITES:::Parameters$ttEvent_BoutS(private$tNow)}
      )
    }

    # flag time-dependent events
    self$findSwarm()
    self$checkEstivation()
  }
}

###############################################################################
# Event Flags
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





#prEstivate gives the daily probability of entering estivation stage of life cycle
prEstivate <- function(){
  pmax(0, cos(2*pi*(MBITES:::Globals$get_tNow()-MBITES:::Parameters$get_Emax())/365) - MBITES:::Parameters$get_Eb())
}

#wakeUpTime gives a random wake up time
wakeUpTime <- function(){
  rnorm(1, MBITES:::Parameters$get_eEndm(), MBITES:::Parameters$get_eEndV())
}

#queueEstivation queues estivation if the mosy would estivate
mbites_checkEstivation1 <- function(){
  if(env$dhmP$ESTIVATE & isAlive(M)){
    if(rbinom(1,1,prEstivate(M$tnow,env=env))){ #attempt to estivate; if succeed set a wake up time
      private$tNext = wakeUpTime()
    } else { #otherwise immediately die
      M$bStateNew = "D"
    }
  }
  return(M)
}




#' M-BITES: Simulates estivation \code{MosquitoFemale}
#'
#' The number estivationDay is a day of the year. This
#' method checks to see if tNow < estivationDay < tNext.
#' If so then, the mosquito estivates, which sets
#' tNext to a random number in the future, ttEstivate()
#'
#'  * This method is bound to \code{MosquitoFemale$timing()}.
#'
mbites_checkEstivation2 <- function(){
  #estivationDay is a day of the year, 0 <= estivationDay  <= 365
  estivationDay = private$FemalePopPointer$get_MBITES_PAR("estivationDay")
  T1 = private$tNow%%365
  T2 = private$tNext - private$tNow
  if(T1<estivationDay & T1+T2 > estivationDay){
    ttEstivate = private$FemalePopPointer$get_MBITES_PAR("ttEvent_Estivate")
    private$tNext =  private$tNext + ttEstivate()
  }
}
