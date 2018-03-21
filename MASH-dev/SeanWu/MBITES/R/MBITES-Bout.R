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

#' MBITES: Bout
#'
#' In MBITES, mosquito behavioral bouts consist of a cycle of events: launch -> try -> land -> rest.
#'
#'
#'
#' @name Bout
NULL
#> NULL


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
mbites_oneBout <- function(){

  # launch at the previously scheduled launch time
  private$tNow = private$tNext

  # launch and try
  if(private$search){
    self$attempt_search() # search
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

# set methods
Mosquito$set(which = "public",name = "oneBout",
    value = mbites_oneBout, overwrite = TRUE
)


###############################################################################
# Search Attempt
###############################################################################

#' MBITES: Search Attempt
#'
#' The generic search bout for a mosquito is called if the search flag is \code{TRUE}.
#'  1. call \code{\link{mbites_move}} to find a new site, this search can succeed or fail resulting in:
#'      * a mosquito leaves (a failure) to initiate a new search
#'      * a mosquito stays (a success) and attenpts to land
#'
#'  2. If the search is successful, the search flag is set to \code{FALSE} and the mosquito samples an appropriate
#'     resource from the current site it is at.
#'
mbites_attempt_search <- function(){
  # move to a new site
  self$move()
  # P(succeed)
  p = switch(private$state,
    B = MBITES:::Parameters$get_Bs_succeed(),
    O = MBITES:::Parameters$get_Os_succeed(),
    S = MBITES:::Parameters$get_Ss_succeed(),
    M = MBITES:::Parameters$get_Ms_succeed()
  )
  if(runif(1) < p){
    private$search = FALSE # next bout will be an attempt
  }
}

#' MBITES: Move to a new Site
#'
#' If successful, the mosquito moves to a new \code{\link{Site}} object from querying
#' the current site by \code{\link{move_mosquito_Site}}. This method is called from \code{\link{mbites_attempt_search}}
#'      * binding: \code{Mosquito$move}
#'
mbites_move <- function(){
  private$site = private$site$move_mosquito() # see LANDSCAPE-Site.R
  private$boutFail = 0L # bout fail counter resets at a new site
  private$rspot = "l" # initialize resting spot behavior when i get to a new site
}

# set methods
Mosquito$set(which = "public",name = "attempt_search",
    value = mbites_attempt_search, overwrite = TRUE
)

Mosquito$set(which = "public",name = "move_mosquito",
    value = move_mosquito_Site, overwrite = TRUE
)


#################################################################
# Resting
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
  		if(private$gravid){
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

  # call pathogen dynamics last so that EIP uses the tNext (time of next launch)
  # to increment its incubation period now. this ensures that at the start of a bout,
  # the pathogen is referencing the correct day.
  self$pathogenDynamics()
}

# set methods
Mosquito$set(which = "public",name = "updateState",
    value = mbites_updateState, overwrite = TRUE
)


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
      M = {self$MatingSearchCheck()},
      S = {self$SugarSearchCheck()},
      {stop("illegal behavioral state: ",private$state,"\n")}
    )
}

#' MBITES: Check for Blood Feeding Search Bout
#'
#' During the resting period \code{\link{mbites_updateState}}, check if the local site has a
#' blood hosts present.
#'  * this method is bound to \code{MosquitoFemale$BloodFeedingSearchCheck}
mbites_BloodFeedingSearchCheck <- function(){
  # if no resources here, go search
  if(!private$site$has_feed()){
    private$search = TRUE
  }
}

#' MBITES: Check for Oviposit Search Bout
#'
#' During the resting period \code{\link{mbites_updateState}}, check if the local site has an
#' aquatic habitat present for oviposition.
#'  * this method is bound to \code{MosquitoFemale$OvipositSearchCheck}
mbites_OvipositSearchCheck <- function(){
  # if no resources here, go search
  if(!private$site$has_aqua()){
    private$search = TRUE
  }
}

#' MBITES: Check for Sugar Search Bout
#'
#' During the resting period \code{\link{mbites_updateState}}, check if the local site has a
#' sugar feeding resource present.
#'  * this method is bound to \code{MosquitoFemale$SugarSearchCheck}
mbites_SugarSearchCheck <- function(){
  # if no resources here, go search
  if(!private$site$has_sugar()){
    private$search = TRUE
  }
}

#' MBITES: Check for Oviposit Search Bout
#'
#' During the resting period \code{\link{mbites_updateState}}, check if the local site has an
#' aquatic habitat present for oviposition.
#'  * this method is bound to \code{MosquitoFemale$OvipositSearchCheck}
mbites_MatingSearchCheck <- function(){
  # if no resources here, go search
  if(!private$site$has_mate()){
    private$search = TRUE
  }
}

# set methods
Mosquito$set(which = "public",name = "checkForResources",
    value = mbites_checkForResources, overwrite = TRUE
)

Mosquito$set(which = "public",name = "BloodFeedingSearchCheck",
    value = mbites_BloodFeedingSearchCheck, overwrite = TRUE
)

Mosquito$set(which = "public",name = "OvipositSearchCheck",
    value = mbites_OvipositSearchCheck, overwrite = TRUE
)

Mosquito$set(which = "public",name = "SugarSearchCheck",
    value = mbites_SugarSearchCheck, overwrite = TRUE
)

Mosquito$set(which = "public",name = "MatingSearchCheck",
    value = mbites_MatingSearchCheck, overwrite = TRUE
)


###############################################################################
# Attempt Bout: Blood Feeding
###############################################################################

#' MBITES: Blood Feeding Attempt Bout
#'
#' The blood feeding attempt bout has the following structure:
#'
#'    * call \code{\link{mbites_chooseHost}} to choose a host:
#'      * if the host is human, simulate a human encounter (see \code{\link{mbites_humanEncounter}})
#'      * if the host is not human, simulate a zoonotic encounter (see \code{\link{mbites_zooEncounter}})
#'      * if the mosquito chooses a trap, simulate the outcome
#'        NOTE: a CDC light trap is one kind of trap
#'      * a null host is for a failed attempt
#'
#' NOTE: host encounters are found in MBITES-HostEncounter.R
#'       bloodtrap() is found in ...
#'
mbites_attempt_B <- function(){
  # bout success
  if(runif(1) < MBITES:::Parameters$get_B_succeed()){

    # sample resources and hosts
    private$feed_res = private$site$sample_feed() # sample feeding resources
    self$chooseHost() # MBITES-HostEncounter.R

    # specific host encounter routines
    if(private$hostID > 0L){
      self$humanEncounter()
    } else if(private$hostID == -1L){
      self$zooEncounter()
    } else if(private$hostID == -2L){
      self$bloodtrap()
    } else if(private$hostID == 0L){
      # an empty risk queue implies a failure to get a blood meal, thus the bout failed
    } else {
      stop("illegal hostID value")
    }

  }

  # bout failure
  if(!bloodfed){
    # if i did not succeed my bout, increment failure counter
    private$boutFail = private$boutFail + 1L
  } else {
    # reset flags
    private$boutFail = 0L
  }
}

# set methods
Mosquito$set(which = "public",name = "attempt_B",
    value = mbites_attempt_B, overwrite = TRUE
)


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
mbites_attempt_O <- function(){
  # bout success
  if(runif(1) < MBITES:::Parameters$get_O_succeed()){

    # sample resources
    self$chooseHabitat() # MBITES-Oviposition.R

    # check habitat type
    if(private$habitatID > 0L){
      self$layEggs() # MBITES-Oviposition.R
    } else if(private$habitatID == -1L){
      self$ovitrap()
    } else {
      stop("illegal habitatID value")
    }
  }

  # bout failure
  if(private$batch > 0){
    # if i did not succeed my bout, increment failure counter
    private$boutFail = private$boutFail + 1L
  } else {
    # reset flags
    private$boutFail = 0L
  }
}

# set methods
Mosquito$set(which = "public",name = "attempt_O",
    value = mbites_attempt_O, overwrite = TRUE
)


###############################################################################
# Attempt Bout: Mating
###############################################################################

#' M-BITES: Mating Bout (M) \code{MosquitoFemale}
#'
#' A mosquito performs mating bout.
#'
#'    1) choose a mate from the swarmingQ ;
#'    2a) mate
#'    2b) the swarm has been sprayed.
#'    2c) a null mate is for a failed attempt
#
#'  * This method is bound to \code{MosquitoFemale$boutM()}.
#'
mbites_attempt_M <- function(){
  # bout success
  if(runif(1) < MBITES:::Parameters$get_Ms_succeed()){
    self$chooseMate()

    # found a mate
    if(private$mateID > 0L){
      self$mating()
    # empty queue
    } else if(private$mateID == 0L){
      # nothing yet.
    # bad mateID
    } else {
      stop("illegal mateID value")
    }
  }

  # bout failure
  if(!private$mated){
    # if i did not succeed my bout, increment failure counter
    private$boutFail = private$boutFail + 1L
  } else {
    # reset flags
    private$boutFail = 0L
  }
}

# set methods
Mosquito$set(which = "public",name = "attempt_M",
    value = mbites_attempt_M, overwrite = TRUE
)


###############################################################################
# Attempt Bout: Sugar Feeding
###############################################################################

#' M-BITES: Sugar Feeding Bout (S) \code{MosquitoFemale}
#'
#'
#' The sugar feeding attempt bout has the following structure:
#'
#'    1) choose a sugar source;
#'    2a) take a sugar meal
#'    2b) if a mosuqito chooses an atsb, simulate the outcome
#'    2c) a null source is for a failed attempt
#'
#'  * This method is bound to \code{MosquitoFemale$boutS()}.
#'
mbites_attempt_S <- function(){
  # bout success
  if(runif(1) < MBITES:::Parameters$get_S_succeed()){

    # sample resources
    self$chooseSugarSource() # MBITES-Energetics.R

    if(private$sugarID > 0L){
      self$sugarMeal() # MBITES-Energetics.R
    } else if(private$sugarID == -1L){
      # self$atsb()
    } else {
      stop("illegal sugarID value")
    }
  }

  # bout failure
  if(private$energy < 1){
    # if i did not succeed my bout, increment failure counter
    private$boutFail = private$boutFail + 1L
  } else {
    # reset flags
    private$boutFail = 0L
  }
}

# set methods
Mosquito$set(which = "public",name = "attempt_S",
    value = mbites_attempt_S, overwrite = TRUE
)
