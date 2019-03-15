# main simulation loop for ABM
# tnow = get("globals",.GlobalEnv)$get_tnow()
MBITES <- function(mosy,tnow){

  # dont simulate on dead mosquitos
  while(mosy$tnext < tnow & mosy$statenext != "D"){

    mosy$tnow <- mosy$tnext
    mosy$state <- mosy$statenext



  }

  if(mosy$statenext == "D"){
    # have the mosquito log itself tag for deletion/replacement
  }

}
