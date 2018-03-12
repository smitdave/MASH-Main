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
      switch(private$state,
        B = {private$t_next = MBITES:::Parameters$ttEvent_BoutBs(private$t_now)},
        O = {private$t_next = MBITES:::Parameters$ttEvent_BoutOs(private$t_now)},
        S = {private$t_next = MBITES:::Parameters$ttEvent_BoutSs(private$t_now)}
      )
    } else {
      switch(private$state,
        B = {private$t_next = MBITES:::Parameters$ttEvent_BoutB(private$t_now)},
        O = {private$t_next = MBITES:::Parameters$ttEvent_BoutO(private$t_now)},
        S = {private$t_next = MBITES:::Parameters$ttEvent_BoutS(private$t_now)}
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
