###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES - landing & resting
#     Sean Wu
#     March 2019
#
###############################################################################

restingspot <- function(mosy){

  if(mosy$statenext != "D"){

    if(mosy$state == "B" & mosy$bloodfed){
      activity_ppr(mosy) # mbites-bout.R
    }

    if(mosy$statenext != "D"){

      old_spot <- private$rspot
      newSpot(mosy)
      if(old_spot != "i" & mosy$rspot == "i"){
        enterHouse(mosy)
      }

    }

  }

}

# choose a new spot
newSpot <- function(mosy){

  probs <- get("parameters",.GlobalEnv)$InAndOut[mosy$rspot,] * get("parameters",.GlobalEnv)$rwts[mosy$state,]
  mosy$rspot <- sample(x=get("parameters",.GlobalEnv)$rspot,size=1,prob=probs)

}

# attempt to enter a house
enterHouse <- function(mosy){

  p <- get("landscape",.GlobalEnv)[[mosy$site]]$enterP
  if(runif(1) < p){
    # mosquito is inside of the house
  } else {
    # mosquito is not inside of house
    newSpot(mosy)
    # if i decided to go inside again; call recursively
    if(mosy$rspot == "i"){
      Recall()
    }
  }
}
