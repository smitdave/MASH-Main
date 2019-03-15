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
