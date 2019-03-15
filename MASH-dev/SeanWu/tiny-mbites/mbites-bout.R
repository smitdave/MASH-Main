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

  private$fail <- 0L # failure counter resets
  private$rspot <- "v" # by default, land on vegetation

  # if successful, i'll try to oviposit next time
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
  if(fequal(mosy$bmSize,0)){
    mosy$fail <- mosy$fail + 1L
  } else {
    mosy$fail <- 0L
  }

}




# the egg laying search bout
activity_ELSB <- function(mosy){

  # move the mosquito
  search(mosy)

  private$fail <- 0L # failure counter resets
  private$rspot <- "v" # by default, land on vegetation

  # if successful, i'll try to oviposit next time
  p <- get("parameters",.GlobalEnv)$ELSB_succeed
  if(runif(1) < p){
    mosy$statenext <- "O"
  }

}

# the egg laying attempt bout
activity_ELAB <- function(){

}


# the post-prandial resting (pseudo)bout
activity_ppr <- function(mosy){

}
