###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   M-BITES: Blood & Sugar Energetics
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################


###############################################################################
# Energetics
###############################################################################

#' MBITES-Generic: Blood Energetics for \code{\link{MosquitoFemale}}
#'
#' Add to energy from blood feeding
#'  * This method is bound to \code{MosquitoFemale$BloodMeal()}.
#'
mbites_bloodEnergetics <- function(){ # called from MBITES-Bloodmeal.R
  energyFromBlood = private$FemalePopPointer$get_MBITES_PAR("energyFromBlood")
  topUp = energyFromBlood(private$bmSize)
  private$energy = min(1, private$energy + topUp)
  if(!private$mature){
    private$energyPreG = private$energyPreG - topUp
    if(private$energyPreG <= 0 & private$mated == TRUE){
      private$mature = TRUE
    }
  }
}

#' M-BITES: Choose a sugar source \code{MosquitoFemale}
#'
#'  * This method is bound to \code{MosquitoFemale$boutS()}.
#'
mbites_chooseSugarSource <- function(){
  # write me
}


#' MBITES-Generic: Sugar Energetics for \code{\link{MosquitoFemale}}
#'
#' Add to energy from blood feeding
#'  * This method is bound to \code{MosquitoFemale$BloodMeal()}.
#'

mbites_sugarMeal <- function(){ # called from MBITES-Bouts.R, boutS
  private$energy = 1

  if(!private$mature){
    private$energyPreG = private$energyPreG - private$FemalePopPointer$get_MBITES_PAR("preGsugar")
    if(private$energyPreG <= 0){
      private$mature = TRUE
    }
  }
}


#' MBITES-Generic: Energetics for \code{\link{MosquitoFemale}}
#'
#' Handle energy dependent mortality and sugar bout queueing as function of current mosquito energy levels.
#'  * This method is bound to \code{MosquitoFemale$sugarEnergetics()}.
#'
mbites_energetics <- function(){
  if(self$isAlive()){
    flightBurnEnergy = private$FemalePopPointer$get_MBITES_PAR("flightBurnEnergy"))
    private$energy = max(0, private$energy - flightBurnEnergy())
    self$queueSugarBout()
  }
}


### MODULARIZE
mbites_flightBurnEnergy <- function(){
  private$energy = max(0,private$energy - private$FemalePopPointer$get_MBITES_PAR("S.u"))
}

#' MBITES-Generic: Mean Flight Energetics for \code{\link{MosquitoFemale}}
#'
#' Reduce this mosquito's energy reserves by mean amount averaged over possible flight distances.
#'  * This method is bound to \code{MosquitoFemale$flightEnergetics()}.
#'
mbites_flightEnergetics_Mean <- function(){
  private$energy = max(0,private$energy - private$FemalePopPointer$get_MBITES_PAR("S.u"))
}

#' MBITES-Generic: Exact Flight Energetics for \code{\link{MosquitoFemale}}
#'
#' Reduce this mosquito's energy reserves by Beta-distributed random variable as a function of flight distance.
#'  * This method is bound to \code{MosquitoFemale$flightEnergetics()}.
#'
mbites_flightEnergetics_Exact <- function(){
  cat("write me! i should give beta-distributed energy consumption as function of flight distance\n")
  stop()
}

#' MBITES-Generic: Queue Sugar Bout due to Energy Reserves for \code{\link{MosquitoFemale}}
#'
#' Potentially queue a sugar bout \code{\link{mbites_boutS}} as a function of energy reserves.
#'  * This method is bound to \code{MosquitoFemale$queueSugarBout()}.
#'
mbites_queueSugarBout <- function(){
  if(runif(1) < self$pSugarBout()){
    private$starved = TRUE
  }
}

#' MBITES-Generic: Probability to Queue Sugar Bout due to Energy Reserves for \code{\link{MosquitoFemale}}
#'
#' Probability to queue sugar bout as function of energy reserves given by \deqn{ \frac{2+S.sb}{1+S.sb}-\frac{e^{S.sa\times energy}}{S.sb+e^{S.sa\times energy}} }
#'  * This method is bound to \code{MosquitoFemale$pSugarBout()}.
#' @md
mbites_pSugarBout <- function(){
  return(
    (2+private$FemalePopPointer$get_MBITES_PAR("S.sb"))/(1+private$FemalePopPointer$get_MBITES_PAR("S.sb"))-exp(private$FemalePopPointer$get_MBITES_PAR("S.sa")*private$energy)/(private$FemalePopPointer$get_MBITES_PAR("S.sb")+exp(private$FemalePopPointer$get_MBITES_PAR("S.sa")*private$energy))
  )
}
