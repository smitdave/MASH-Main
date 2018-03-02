###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Bout
#     MBITES Team
#     February 2018
#
###############################################################################


###############################################################################
# Search Bout
###############################################################################

#' MBITES: Search Bout
#'
#' The generic search bout for a mosquito is called if the boolean field \code{search = TRUE}.
#'  1. call \code{\link[MBITES]{mbites_move}} to find a new site
#'      * a mosquito leaves to initiate a new search
#'      * a mosquito stays (a success) and attenpts to land
#'
mbites_search <- function(){
  self$move()
  p_success = switch(private$state,
    B = MBITES:::Parameters$get_Bs_succeed(),
    O = MBITES:::Parameters$get_Os_succeed(),
    S = MBITES:::Parameters$get_Ss_succeed(),
    M = MBITES:::Parameters$get_Ms_succeed()
  )
  if(runif(1) < p_success){
    private$search = FALSE # the next bout will be an attempt
    # private$rspot = "l"         # initialize restingSpot()
  }
}


###############################################################################
# Attempt Bout: Blood Feeding
###############################################################################

#' MBITES: Blood Feeding Attempt Bout
#'
#' The blood feeding attempt bout has the following structure:
#'
#'    * call \code{\link[MBITES]{mbites_chooseHost}} to choose a host:
#'      * if the host is human, simulate a human encounter (see \code{\link[MBITES]{mbites_humanEncounter}})
#'      * if the host is not human, simulate a zoonotic encounter (see \code{\link[MBITES]{mbites_zooEncounter}})
#'      * if the mosquito chooses a trap, simulate the outcome
#'        NOTE: a CDC light trap is one kind of trap
#'      * a null host is for a failed attempt
#'
#' NOTE: host encounters are found in MBITES-HostEncounter.R
#'       bloodtrap() is found in ...
#'
mbites_boutB <- function(){
  # check success
  if(runif(1) < MBITES:::Parameters$B_succeed()){
    self$chooseHost() # MBITES-ChooseHost.R
  } else {
    private$hostID = 0L
  }

  if(private$hostID > 0L){
    self$humanEncounter()
  } else if(private$hostID == -1L){
    self$zooEncounter()
  } else if(private$hostID == -2L){
    self$bloodtrap()
  } else if(private$hostID == 0L){
    return(NULL)
  } else {
    stop("illegal hostID value")
  }
}


###############################################################################
# Attempt Bout: Oviposition
###############################################################################

#' MBITES: Oviposition Attempt Bout
#'
#' The egg laying attempt bout has the following structure:
#'
#'    1) choose a habitat;
#'    2a) lay eggs in a habitat
#'    2b) if a mosuqito chooses an ovitrap, simulate the outcome
#'    2c) a null habitat is for a failed attempt
#'
#'
#'
mbites_boutO <- function(){
  if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("O_succeed")){
    self$chooseHabitat() # MBITES-EggLaying.R
  } else {
    private$habitatID = 0L
  }

  if(private$habitatID > 0){
    self$layEggs() # MBITES-EggLaying.R
  } else if(private$habitatID == -1){
    #print("OVI Present")
    self$ovitrap()
  } else if(private$habitatID == 0){
    return(NULL)
  } else {
    stop("illegal hostID value")
  }
}
