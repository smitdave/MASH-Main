###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   M-BITES: Bouts
#   MASH-MICRO Team
#   September 2017
#
###############################################################################

#################################################################
#
# M-BITES: Bouts
#
#################################################################

#################################################################
# M-BITES: The Generic Search Bout is called if search=TRUE
#################################################################

#' M-BITES: Generic Search Bout \code{MosquitoFemale}
#'
#' The generic search bout:
#'   1) call moveMe() to find a new site;
#'   2a) a mosquito leaves to initiate a new search;
#'   2b) a mosquito stays (a success) and attenpts to land.
#'
mbites_boutSearch <- function(){
  self$moveMe()
  pr_success = switch(private$state,
    B = private$FemalePopPointer$get_MBITES_PAR("Bs_succeed")
    O = private$FemalePopPointer$get_MBITES_PAR("Os_succeed")
    S = private$FemalePopPointer$get_MBITES_PAR("Ss_succeed")
    M = private$FemalePopPointer$get_MBITES_PAR("Ms_succeed")
  )
  if(runif(1) < pr_success){
    private$search = FALSE      # the next bout will be an attempt
    private$lspot = "l"         # initialize restingSpot()
  }
}


#################################################################
# M-BITES: Blood Feeding Attempt Bout :: B
#################################################################

#' M-BITES: Blood Feeding Attempt Bout (B) \code{MosquitoFemale}
#'
#' The blood feeding attempt bout has the following structure:
#'
#'    1) choose a host;
#'    2a) if the host is human, simulate a human encounter
#'    2b) if the host is not human, simulate a zoonotic encounter
#'    2c) if the mosquito chooses a trap, simulate the outcome
#'        NOTE: a CDC light trap is one kind of trap
#'    2d) a null host is for a failed attempt
#'
#' NOTE: host encounters are found in MBITES-HostEncounter.R
#'       bloodtrap() is found in ...
#'
mbites_boutB <- function(){
  # check success
  if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("B_succeed")){
    self$chooseHost() # MBITES-ChooseHost.R
  } else {
    private$hostID = 0L
  }

  if(private$hostID > 0){
    self$humanEncounter() # MBITES-HostEncounter.R
  } else if(private$hostID == -1){
    self$zooEncounter() # MBITES-HostEncounter.R
  } else if(private$hostID == -2){
    self$bloodtrap()
  } else if(private$hostID == 0){
    return(NULL)
  } else {
    stop("illegal hostID value")
  }
}

mbites_bloodtrap <-function(){
   # move me!
   # write me!
}

#################################################################
# M-BITES: Egg Laying Attempt Bout :: O
#################################################################

#' M-BITES: Egg Laying Attempt Bout (O) \code{MosquitoFemale}
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

#################################################################
# M-BITES: Sugar Feeding Attempt Bout :: S
#################################################################

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
mbites_boutS <- function(){
  if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("S_succeed")){
    self$chooseSugarSource()
  } else {
    private$sugarID = 0L
  }

  if(private$sugarID > 0){
    self$sugarMeal()   # MBITES-Energetics.R
  } else if(private$sugarID == -1){
    self$atsb()
  } else if(private$sugarID == 0){
    return(NULL)
  } else {
    stop("illegal sugarID value")
  }
}


mbites_atsb <- function(){
# write me!
}

#################################################################
# M-BITES: Mating Bout :: M
#################################################################

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
mbites_boutM <- function(){
  if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("M_succeed")){
    self$chooseMate()
  } else {
    private$mateID = 0L
  }

  if(private$sugarID > 0){
    #self$mating()
    private$mated = TRUE
  } else if(private$mateID == -1){
    self$swarmSpray()
  } else if(private$mateID == 0){
    return(NULL)
  } else {
    stop("illegal sugarID value")
  }


  #### SwarmSpray #############################################################################################
  swarmSpray=private$LandscapePointer$get_MatingSites(private$locNow)$get_swarmSpray()
  if(is.null(swarmSpray)==FALSE){
      private$stateNew = swarmSpray$mosquitoKillEncounter(private$stateNew,interventionType="SwarmSpray")
      private$lspot = swarmSpray$mosquitoRepelEncounter(private$lspot,interventionType="SwarmSpray")
  }

}


#################################################################
# M-BITES: Generic Bout
#################################################################

#' M-BITES: One Bout \code{MosquitoFemale}
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
mbites_oneBout <- function(){

  # tNext is when the next launch will occur.
  # a bout is not executed if contingent events
  # have not been updated.
  private$tNow = private$tNext

  # LAUNCH & TRY
  if(private$search==TRUE){
    # The Generic Search Bout
    self$boutSearch()
  } else {
    # The Attempt Bouts
    switch(private$state,
      B = {self$boutB()},
      O = {self$boutO()},
      M = {self$boutM()},
      S = {self$boutS()},
      {stop(cat("illegal behavioral state: ",private$state,"\n",sep=""))}
    )
  }

  # LAND
  self$restingSpot() # MBITES-Restingspot.R
                     # NOTE: restingSpot() can set leave=TRUE
  # REST
  self$updateState()

  # log history
  private$history$historyTrack(privateEnv = private, alive = self$isAlive())
}

#################################################################
# M-BITES: Update State
#################################################################

#' M-BITES: Update the Behavioral State at the End of a Bout for \code{\link{MosquitoFemale}}
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
  if(private$state == 'D'){
		private$state = 'D'}else{
    if(private$starved == TRUE){
			private$state = 'S'}else{
  		if(private$gravid == TRUE) {
    		self$checkRefeed()}else{     #MBITES-Oogenesis.R
    			private$state = 'B'}}}

  # The states in priority order
  self$timing()  #MBITES-Timing.R
                 #NOTE: timing() can set state = 'M'

  # if there are no resources of the required type present, set
  # search = TRUE
  self$checkForResources()
}

#################################################################
# M-BITES: Check to see if a search is required
#################################################################

#' M-BITES: If the required resource is not here, initiate a search \code{\link{MosquitoFemale}}
#'
#' After running a bout, this code checks the mosquito's
#' behavioral state agains the local resources to see if a search is
#' required
#'
mbites_checkForResources <- function(){
    switch(private$state,
      B = {private$search=TRUE},
      O = {self$OvipositSearchCheck()},
      M = {private$search=TRUE},
      S = {private$search=TRUE},
      {stop(cat("illegal behavioral state: ",private$state,"\n",sep=""))}
    )
}

#' M-BITES: Check for Oviposit Search Bout \code{MosquitoFemale}
#'
#' During a resting bout \code{\link{mbites_boutR}}, the mosquito checks if she is in a
#'  * this method is bound to \code{MosquitoFemale$OvipositSearchCheck}
mbites_OvipositSearchCheck <- function(){
  if(private$pSetNow=="f"){
    # check for peri-domestic breeding sites
    if(!is.null(private$LandscapePointer$get_FeedingSites(private$locNow)$get_periDomestic())){
      # has peri-domestic breeding site
      private$stateNew = "O"
      private$periDomestic = TRUE
    } else {
      # does not have peri-domestic breeding site
      private$search = TRUE
    }
  } else {
    private$search=TRUE
  }
}

#################################################################
# M-BITES: Simulation
#################################################################

#' M-BITES: Run Simulation for \code{\link{MosquitoFemale}}
#'
#' Run the M-BITES life cycle simulation algorithm while alive and has not overrun time in enclosing \code{\link{Tile}}.
#' This method calls \code{\link{mbites_oneBout}} to simulate each life stage.
#'  * This method is bound to \code{MosquitoFemale$MBITES()}.
#'
mbites_oneMosquito_MBITES <- function(){

  # run algorithm while alive and has not overrun tile time
  while(private$tNext < private$TilePointer$get_tNow() & private$state != 'D'){
    self$oneBout()
  }

  # if mosquito is dead output data if asked and remove it from the enclosing storage object
  if(private$state == 'D'){
    self$writeAndDelete(conHist = private$TilePointer$get_FemaleHistoryCon(), conPath = private$TilePointer$get_MosquitoPathogenCon())
  }

}

#' M-BITES: Run Simulation for \code{\link{MosquitoPopFemale}}
#'
#' Run the M-BITES life cycle simulation algorithm while alive and has not overrun time in enclosing \code{\link{Tile}}.
#' This method calls \code{\link{mbites_oneMosquito_MBITES}} one each living mosquito sequentially.
#'  * This method is bound to \code{MosquitoPopFemale$MBITES()}.
#'
mbites_Pop_MBITES <- function(){
  private$pop$apply(tag="MBITES",returnVal=FALSE)

}
