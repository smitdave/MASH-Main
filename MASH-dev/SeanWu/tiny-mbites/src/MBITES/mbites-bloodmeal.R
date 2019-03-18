###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES - everything related to the blood meal
#     Sean Wu
#     March 2019
#
###############################################################################

# the blood meal event
bloodmeal <- function(mosy){

  mosy$bmSize <- rbloodmeal()
  mosy$bloodfed <- TRUE

  # overfeeding(mosy) # to disable overfeeding just comment this out
  bloodEnergetics(mosy)

}

# size of the blood meal
rbloodmeal <- function(){
  bm_a <- get("parameters",.GlobalEnv)$bm_a
  bm_b <- get("parameters",.GlobalEnv)$bm_b
  rbeta(n=1,bm_a,bm_b)
}

# overfeeding
overfeeding <- function(mosy){

  p <- pOverFeed(mosy$bmSize)
  if(runif(1) < p){
    mosy$statenext <- "D"
    mosy$hist$cod <- "overfeeding"
  }

}

pOverFeed <- function(bm){
  of_a <- get("parameters",.GlobalEnv)$of_a
  of_b <- get("parameters",.GlobalEnv)$of_b
  exp(of_a * bm) / (of_b + exp(of_a * bm))
}


# blood energetics
bloodEnergetics <- function(mosy){

  if(mosy$statenext != "D"){

    topup <- mosy$bmSize * get("parameters",.GlobalEnv)$energyFromBlood
    mosy$energy <- max(1,mosy$energy+topup)

    if(!mosy$mature){

      mosy$energyPreG <- mosy$energyPreG - topup
      if(mosy$energyPreG <= 0 & mosy$mated){
        mosy$mature <- TRUE
      }

    }

  }

}
