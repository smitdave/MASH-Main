# main simulation loop for ABM
# tnow = get("globals",.GlobalEnv)$get_tnow()
MBITES <- function(mosy,tnow){
  # dont simulate on dead mosquitos
  if(mosy$alive){

    while(mosy$tnext < tnow & mosy$alive){

    }

    if(!mosy$alive){
      # have the mosquito log itself and delete somehow
    }

  }
}
