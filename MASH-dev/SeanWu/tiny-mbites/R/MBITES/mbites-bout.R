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


###############################################################################
# MBITES: simulate for a pop
###############################################################################

MBITES_population <- function(mpop){

  tnow <- get("globals")$get_tnow()

  # simulate
  invisible(eapply(env = mpop,FUN = function(m,t){
      MBITES(m,t)

      # track history
      if(m$statenext == "D"){

        if(m$tnow > m$tnext){
          m$tnext <- m$tnow
        }

        mbites_exit_female(m)
        rm(list = as.character(m$id),envir = pop)
      }
    },t=tnow,USE.NAMES = FALSE)
  )

}


###############################################################################
# MBITES: main simulation loop for one mosquito
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

    # oogenesis
    oogenesis(mosy)

    # update states
    updatestate(mosy)

    # search
    checkleave(mosy)

    # timing
    timing(mosy)

    # 'just before' i jump out of this state (including to the dead state)
    track_history(mosy)
  }

}


###############################################################################
# logic to sample the "statenext" (where I jump to in behavioral state space next)
###############################################################################

# update the next state
updatestate <- function(mosy){
  if(mosy$starved){
    mosy$statenext <- "S"
  } else {
    if(mosy$gravid){
      refeed(mosy)
    } else {
      mosy$statenext <- "B"
    }
  }
}

# this is described as "frustration" in the manuscript
boutfail_check <- function(mosy){
  # check failure
  if(mosy$fail > 0){
    p <- get("parameters",.GlobalEnv)$boutFail_p
    if(runif(1) < p){
      mosy$search <- TRUE
    }
  }
}

# check leave
checkleave <- function(mosy){

  # if failure has occured, check for frustration (just leave)
  boutfail_check(mosy)

  # if the mosquito isn't primed to search, make sure it has what it needs
  if(!mosy$search){
    switch(mosy$statenext,
      B = {
        if(!get("landscape",.GlobalEnv)[[mosy$site]]$has_f){
          mosy$search <- TRUE
          mosy$statenext <- "F"
        } else {
          p <- get("parameters",.GlobalEnv)$disperse
          if(runif(1) < p){
            mosy$search <- TRUE
            mosy$statenext <- "F"
          }
        }
      },
      O = {
        if(!get("landscape",.GlobalEnv)[[mosy$site]]$has_l){
          mosy$search <- TRUE
          mosy$statenext <- "L"
        } else {
          p <- get("parameters",.GlobalEnv)$disperse
          if(runif(1) < p){
            mosy$search <- TRUE
            mosy$statenext <- "L"
          }
        }
      }
    )
  } else {
    switch(mosy$statenext,
      B = {
        mosy$search <- TRUE
        mosy$statenext <- "F"
      },
      O = {
        mosy$search <- TRUE
        mosy$statenext <- "L"
      }
    )
  }

}


###############################################################################
# Activity bouts
###############################################################################

# the blood feeding search bout
activity_BFSB <- function(mosy){

  # move the mosquito
  search(mosy)

  mosy$fail <- 0L # failure counter resets
  mosy$rspot <- "v" # by default, land on vegetation

  # if successful, i'll try to blood feed next time
  p <- get("parameters",.GlobalEnv)$BFAB_succeed
  if(runif(1) < p){
    mosy$search <- FALSE
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
      cat("warning: mosquito ",mosy$id," couldn't find a host at their site\n")
    } else {
      stop("mosquito ",mosy$id," has illegal hostID value: ",mosy$hostID,"\n")
    }

  }

  # check if the mosquito managed to blood feed
  if(!mosy$bloodfed){
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
    mosy$search <- FALSE
  }

}

# the egg laying attempt bout
activity_ELAB <- function(mosy){

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

  }

  if(mosy$gravid){
    mosy$fail <- mosy$fail + 1L
  } else {
    mosy$fail <- 0L
  }

}

# the post-prandial resting (pseudo)bout
activity_ppr <- function(mosy){

  # ppr is a short pseudo-bout; so we update the time
  timing_ppr(mosy)

  # history tracking
  if(mosy$hist$rest){
    track_rest(mosy)
  }

  p <- survive_ppr(mosy)
  if(runif(1) < p){
    mosy$statenext <- "D"
    mosy$hist$cod <- "ppr"
  }

}

# P(survive the post-prandial rest)
survive_ppr <- function(bm){
  ppr_a = get("parameters",.GlobalEnv)$ppr_a
  ppr_b = get("parameters",.GlobalEnv)$ppr_b
  exp(ppr_a * bm)/(ppr_b + exp(ppr_a * bm))
}
