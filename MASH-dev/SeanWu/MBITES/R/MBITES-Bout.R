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
# Generic Bout
###############################################################################

#' MBITES: One Bout \code{MosquitoFemale}
#'
#' Mosquito behavior has a finite set of states (state space of model), within which there are certain biological functions that are always evaluated.
#' A bout is the actions taken by a mosquito between a launch and landing; \code{mbites_oneBout} handles all the biological imperatives that occur during a bout,
#' while specialized bout action methods handle the events that occur due to the intent of the mosquito during the bout. The two classes of bouts are SEARCH and ATTEMPT.
#' Search bouts are similar enough that there is a generic version of it.
#'  * \code{\link{mbites_boutB}}: blood feeding attempt bout
#'  * \code{\link{mbites_boutO}}: egg laying attempt bout
#'  * \code{\link{mbites_boutS}}: sugar feeding attempt bout
#'  * \code{\link{mbites_boutM}}: mating bout
#'  * \code{\link{mbites_boutSearch}}: a search bout for any
#'
#' The generic bout runs necessary updates of timing, state, survival, energetics, and queue checks prior to calling the nested
#' specific bout action, and checks that the mosquito is alive/active before calling the bout. It updates \code{tNext} and \code{stateNew}.
#'
#' This corresponds to the following Gillespie-style algorithm:
#'
#' 1. tNow is set to tNext from previous bout
#' 2. Launch: an attempt or search bout
#' 3. Land: restingSpot is called to find a microsite for resting
#' 4. Rest: update the mosquito's state
#' 5. Log events
#'
#'  * This method is bound to \code{MosquitoFemale$oneBout()}.
#'
#' @include MBITES-Mosquito.R
mbites_bout <- function(){

  # launch at the previously scheduled launch time
  private$t_now = private$t_next

  # launch and try
  if(private$search){
    self$searchBout() # search
  } else {
    # attempt an action
    switch(private$state,
      B = {self$attempt_B()},
      O = {self$attempt_O()},
      M = {self$attempt_M()},
      S = {self$attempt_S()},
      {stop("mosquito ",private$id," calling illegal behavioral state: ",private$state,"\n")}
    )
  }

  # land
  self$restingSpot()

  # rest
  self$updateState()

  # log history
  self$trackHistory()
}


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
#'
mbites_searchBout <- function(){
  p = switch(private$state,
    B = MBITES:::Parameters$get_Bs_succeed(),
    O = MBITES:::Parameters$get_Os_succeed(),
    S = MBITES:::Parameters$get_Ss_succeed(),
    M = MBITES:::Parameters$get_Ms_succeed()
  )
  if(runif(1) < p){
    self$move()
  }
}

Mosquito$set(which = "public",name = "searchBout",
    value = mbites_searchBout, overwrite = TRUE
)

#' MBITES: Move
#'
#' If successful, the mosquito moves to a new \code{\link[MBITES]{Site}} object from querying
#' the current site by \code{\link[MBITES]{move_mosquito_Site}}. This method is called from \code{\link[MBITES]{mbites_searchBout}}
#'      * binding: \code{Mosquito$move}
#'
mbites_move <- function(){
  private$site = private$site$move_mosquito()
  private$search = FALSE
}

Mosquito$set(which = "public",name = "move_mosquito",
    value = move_mosquito_Site, overwrite = TRUE
)


# ###############################################################################
# # Attempt Bout: Blood Feeding
# ###############################################################################
#
# #' MBITES: Blood Feeding Attempt Bout
# #'
# #' The blood feeding attempt bout has the following structure:
# #'
# #'    * call \code{\link[MBITES]{mbites_chooseHost}} to choose a host:
# #'      * if the host is human, simulate a human encounter (see \code{\link[MBITES]{mbites_humanEncounter}})
# #'      * if the host is not human, simulate a zoonotic encounter (see \code{\link[MBITES]{mbites_zooEncounter}})
# #'      * if the mosquito chooses a trap, simulate the outcome
# #'        NOTE: a CDC light trap is one kind of trap
# #'      * a null host is for a failed attempt
# #'
# #' NOTE: host encounters are found in MBITES-HostEncounter.R
# #'       bloodtrap() is found in ...
# #'
# mbites_attempt_B <- function(){
#   # check success
#   if(runif(1) < MBITES:::Parameters$B_succeed()){
#     self$chooseHost() # MBITES-ChooseHost.R
#   } else {
#     private$hostID = 0L
#   }
#
#   if(private$hostID > 0L){
#     self$humanEncounter()
#   } else if(private$hostID == -1L){
#     self$zooEncounter()
#   } else if(private$hostID == -2L){
#     self$bloodtrap()
#   } else if(private$hostID == 0L){
#     return(NULL)
#   } else {
#     stop("illegal hostID value")
#   }
# }
#
#
# ###############################################################################
# # Attempt Bout: Oviposition
# ###############################################################################
#
# #' MBITES: Oviposition Attempt Bout
# #'
# #' The egg laying attempt bout has the following structure:
# #'
# #'    1) choose a habitat;
# #'    2a) lay eggs in a habitat
# #'    2b) if a mosuqito chooses an ovitrap, simulate the outcome
# #'    2c) a null habitat is for a failed attempt
# #'
# #'
# #'
# mbites_attempt_O <- function(){
#   if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("O_succeed")){
#     self$chooseHabitat() # MBITES-EggLaying.R
#   } else {
#     private$habitatID = 0L
#   }
#
#   if(private$habitatID > 0){
#     self$layEggs() # MBITES-EggLaying.R
#   } else if(private$habitatID == -1){
#     #print("OVI Present")
#     self$ovitrap()
#   } else if(private$habitatID == 0){
#     return(NULL)
#   } else {
#     stop("illegal hostID value")
#   }
# }











#################################################################
# Rest
#################################################################

#' MBITES: Update the Behavioral State at the End of a Bout for \code{\link{MosquitoFemale}}
#'
#' After landing, during the resting period, the mosquito's
#' behavioral state and other state variables are updated.
#'
#' M-BITES checks the state variables during the resting
#' period to determine what the next state will be. There is
#' a natural hierarchy: a dead mosquito can never be revived;
#' a starved mosquito will seek sugar; a gravid mosquito will
#' tend to lay eggs, though it might decide to top up with
#' blood; if nothing else, a mosquito will seek blood.
#'
mbites_updateState <- function(){

  self$energetics()    # MBITES-Energetics.R
  self$survival()      # MBITES-Survival.R

  # The states in priority order
  if(private$state == "D"){
		private$state = "D"
  } else {
    if(private$starved){
			private$state = "S"
    } else {
  		if(private$gravid) {
    		self$checkRefeed()  # MBITES-Oogenesis.R
      } else {
  			private$state = "B"
      }
    }
  }

  # The states in priority order
  self$timing()  #MBITES-Timing.R
                 #NOTE: timing() can set state = 'M'

  # if there are no resources of the required type present, set
  # search = TRUE
  self$checkForResources()
}


#################################################################
# checkForResources: Check if search required
#################################################################

#' MBITES: If the required resource is not here, initiate a search \code{\link{MosquitoFemale}}
#'
#' After running a bout, this code checks the mosquito's
#' behavioral state agains the local resources to see if a search is
#' required
#'
mbites_checkForResources <- function(){
    switch(private$state,
      B = {self$BloodFeedingSearchCheck()},
      O = {self$OvipositSearchCheck()},
      M = {private$MatingSearchCheck()},
      S = {private$SugarSearchCheck()},
      {stop("illegal behavioral state: ",private$state,"\n")}
    )
}

#' MBITES: Check for Oviposit Search Bout
#'
#' During the resting period \code{\link[MBITES]{mbites_updateState}}, check if the local site has an
#' aquatic habitat present for oviposition.
#'  * this method is bound to \code{MosquitoFemale$OvipositSearchCheck}
mbites_OvipositSearchCheck <- function(){

  if(private$site$has_feed()){
    private$state = "O"
  } else {
    private$search = TRUE
  }

}

#' MBITES: Check for Blood Feeding Search Bout
#'
#' During the resting period \code{\link[MBITES]{mbites_updateState}}, check if the local site has a
#' blood hosts present.
#'  * this method is bound to \code{MosquitoFemale$OvipositSearchCheck}
mbites_BloodFeedingSearchCheck <- function(){

  if(private$site$has_feed()){
    private$state = "O"
  } else {
    private$search = TRUE
  }

}
