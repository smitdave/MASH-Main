###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES - survival
#     Sean Wu
#     March 2019
#
###############################################################################

# survival function
survival <- function(mosy){
  surviveFlight(mosy)
  surviveHazards(mosy)
}


# did i survive this bout
surviveFlight <- function(mosy){

  # WingTattering(mosy)

  p <- baselineSurvival(mosy)
  p <- p * pEnergySurvival(mosy$energy)

  # p <- p * pWingTattering(mosy$energy) # comment out to disable
  # p <- p * pSenesce(mosy) # comment out to disable

  if(runif(1) < 1-p){
    mosy$statenext <- "D"
    mosy$hist$cod <- "surviveFlight"
  }
}

# basline survival
baselineSurvival <- function(mosy){
  switch(mosy$state,
    F = {get("parameters",.GlobalEnv)$F_surv},
    B = {get("parameters",.GlobalEnv)$B_surv},
    L = {get("parameters",.GlobalEnv)$L_surv},
    O = {get("parameters",.GlobalEnv)$O_surv}
  )
}

pEnergySurvival <- function(energy){
  S_a = get("parameters",.GlobalEnv)$S_a
  S_b = get("parameters",.GlobalEnv)$S_b
  exp(S_a * energy)/(S_b + exp(S_a * energy))
}

# accumulative wing damage
WingTattering <- function(mosy){
  ttsz_p <- get("parameters",.GlobalEnv)$ttsz_p
  if(runif(1) < ttsz_p){
    ttsz_a <- get("parameters",.GlobalEnv)$ttsz_a
    ttsz_b <- get("parameters",.GlobalEnv)$ttsz_b
    mosy$damage <- mosy$damage + rbeta(n=1,ttsz_a,ttsz_b)
  }
}

pWingTattering <- function(damage){
  ttr_a = get("parameters",.GlobalEnv)$ttr_a
  ttr_b = get("parameters",.GlobalEnv)$ttr_b
  (2+ttr_b)/(1+ttr_b) - exp(damage*ttr_a)/(ttr_b + exp(damage*ttr_a))
}

pSenesce <- function(mosy){
  age <- mosy$tNow - mosy$bDay
  sns_a <- get("parameters",.GlobalEnv)$sns_a
  sns_b <- get("parameters",.GlobalEnv)$sns_b
  (2+sns_b)/(1+sns_b) - exp(sns_a*age)/(sns_b + exp(sns_a*age))
}


###############################################################################
# Local Hazards Survival
###############################################################################

surviveHazards <- function(mosy){

  if(mosy$statenext != "D"){
    p <- get("landscape",.GlobalEnv)[[mosy$site]]$haz
    if(runif(1) < p){
      mosy$statenext <- "D"
      mosy$hist$cod <- "surviveHazards"
    }
  }

}
