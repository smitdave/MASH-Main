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


###############################################################################
# Time to Event Sampling
###############################################################################

#' M-BITES: Timing for \code{MosquitoFemale}
#'
#' Sample my time of next launch.
#'  * This method is bound to \code{MosquitoFemale$timing()}.
#'
mbites_timing <- function(){

  if(private$state != 'D'){
    # NOTE :: this updates tNow
    self$checkPostPrandial()

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

#' M-BITES: Simulates the post-prandial resting period for \code{MosquitoFemale}
#'
#' Method checks to see if the mosquito has bloodfed and is in a
#' post prandial state; if so, it resets tNow and tNext to
#' tNow + ttEvent_ppRest()
#'
#'  * This method is bound to \code{MosquitoFemale$timing()}.
#'
mbites_checkPostPrandial <- function(){
  if(private$bloodfed){
    ppRest = MBITES:::Parameters$ttEvent_ppRest()
    private$tNext = private$tNow = private$tNow + ppRest
    private$bloodfed = FALSE
  }
}

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
