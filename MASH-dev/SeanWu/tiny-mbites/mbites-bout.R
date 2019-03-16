###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES - main simulation structure
#     Sean Wu
#     March 2019
#
###############################################################################

# main simulation loop for ABM
# tnow = get("globals",.GlobalEnv)$get_tnow()
MBITES <- function(mosy,tnow){
  # the main simulation loop
  while(mosy$tnext < tnow & mosy$statenext != "D"){

    # enter the new state; update my local time and state
    mosy$tnow <- mosy$tnext
    mosy$state <- mosy$statenext

    switch(mosy$state,
      F = {activity_BFSB(mosy)},
      B = {activity_BFAB(mosy)},
      L = {activity_ELSB(mosy)},
      O = {activity_ELAB(mosy)},
      # M = {self$activity_MAB()},
      # S = {self$activity_SFAB()},
      {stop("mosquito ",mosy$id," calling illegal behavioral state: ",mosy$state,"\n")}
    )

    # land and rest
    restingspot(mosy)

    # energetics
    energetics(mosy)

    # survival
    survival(mosy)

    # oogenesis & refeeding
    oogenesis(mosy)

    #

    # search
    checkleave(mosy)

    # timing
    timing(mosy)

    # 'just before' i jump out of this state (including to the dead state)
    track_history(mosy)
  }
}

# activity bouts

# the blood feeding search bout
activity_BFSB <- function(mosy){

  # move the mosquito
  search(mosy)

  mosy$fail <- 0L # failure counter resets
  mosy$rspot <- "v" # by default, land on vegetation

  # if successful, i'll try to blood feed next time
  p <- get("parameters",.GlobalEnv)$BFAB_succeed
  if(runif(1) < p){
    mosy$statenext <- "B"
  }

}

# the blood feeding attempt bout
activity_BFAB <- function(mosy){

  # bout success
  p <- get("parameters",.GlobalEnv)$BFAB_succeed
  if(runif(1) < p){

    choosehost(mosy)

    if(mosy$hostID > 0L){
      humanEncounter(mosy)
    } else if(mosy$hostID == -1L){
      zooEncounter(mosy)
    } else if(mosy$hostID == 0L){
      # didn't find a host; don't do anything
    } else {
      stop("mosquito ",mosy$id," has illegal hostID value: ",mosy$hostID,"\n")
    }

  }

  # check if the mosquito managed to blood feed
  if(mosy$bloodfed){
    mosy$fail <- mosy$fail + 1L
  } else {
    mosy$fail <- 0L
  }

}


# the egg laying search bout
activity_ELSB <- function(mosy){

  # move the mosquito
  search(mosy)

  mosy$fail <- 0L # failure counter resets
  mosy$rspot <- "v" # by default, land on vegetation

  # if successful, i'll try to oviposit next time
  p <- get("parameters",.GlobalEnv)$ELSB_succeed
  if(runif(1) < p){
    mosy$statenext <- "O"
  }

}

# the egg laying attempt bout
activity_ELAB <- function(){

  # bout success
  p <- get("parameters",.GlobalEnv)$BFAB_succeed
  if(runif(1) < p){

    choosehabitat(mosy)

    # check habitat type
    if(mosy$habitatID > 0L){
      self$layEggs()
    } else if(mosy$habitatID == -1L){
      # ovitrap
    } else {
      stop("mosquito ",mosy$id," has illegal habitatID value: ",mosy$habitatID,"\n")
    }

    mosy$fail <- 0L

  } else {
    mosy$fail <- mosy$fail + 1L
  }

}

# the post-prandial resting (pseudo)bout
activity_ppr <- function(mosy){
  if(mosy$statenext != "D"){

    # ppr is a short pseudo-bout; so we update the time
    timing_ppr(mosy)

    # track_rest(mosy) # if you want
    p <- survive_ppr(mosy)
    if(runif(1) < p){
      mosy$statenext <- "D"
      mosy$hist$cod <- "ppr"
    }

  }
}

# P(survive the post-prandial rest)
survive_ppr <- function(bm){
  ppr_a = get("parameters",.GlobalEnv)$ppr_a
  ppr_b = get("parameters",.GlobalEnv)$ppr_b
  exp(ppr_a * bm)/(ppr_b + exp(ppr_a * bm))
}



### checkleave needs to combine the functionality of 'mbites_boutFailCheck' and 'checkForResources'

# mbites_boutFailCheck <- function(){
#   # if no failures, return FALSE
#   if(private$boutFail < 1){
#     return(FALSE)
#   # start checking failure distribution
#   } else {
#     # because each time probability to leave is a bernoulli trial, the overall P(leave on kth failure) ~ geometric(p)
#     # success! I leave.
#     if(runif(1) < MBITES:::Parameters$get_boutFail_p()){
#       return(TRUE)
#     # fail! I stay.
#     } else {
#       return(FALSE)
#     }
#   }
# }
#
# frustrated <- function(mosy){
#   if(mosy$fail == 0){
#
#   }
# }
