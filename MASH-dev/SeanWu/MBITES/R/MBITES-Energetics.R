###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Energetics (blood and sugar energetics)
#     MBITES Team
#     March 2018
#
###############################################################################

#' MBITES: Energetics
#'
#' write about me!
#'
#'
#'
#' @name Energetics
NULL
#> NULL


###############################################################################
# Blood Energetics
###############################################################################

#' MBITES: Blood Energetics
#'
#' Add energy derived from blood to the mosquito's fuel tank. This method is called from \code{\link{mbites_BloodMeal}}.
#'  * This method is bound to \code{MosquitoFemale$bloodEnergetics}.
#'
mbites_bloodEnergetics <- function(){ # called from MBITES-Bloodmeal.R
  topUp = self$energyFromBlood() # energy derived from blood meal is function of meal size
  private$energy = min(1, private$energy + topUp)
  if(!private$mature){
    private$energyPreG = private$energyPreG - topUp
    if(private$energyPreG <= 0 & private$mated){
      private$mature = TRUE
    }
  }
}


mbites_energyFromBlood <- function(){
  bmSize / (MBITES:::Parameters$get_energyFromBlood_b() + bmSize)
}


###############################################################################
# Sugar Energetics
###############################################################################

#' MBITES: Choose Sugar Source
#'
#' If the mosquito passes the check for a successful sugar feeding attempt bout, it proceeds to choose a sugar source
#' at the \code{\link{Site}} it is at.
#'  * this method is bound to \code{Mosquito$chooseSugarSource}
mbites_chooseSugarSource <- function(){
  # when atsb, etc exist, do the checks here
  private$sugar_res = private$site$sample_sugar() # sample resources
  private$sugarID = 1L # normal sugar source

}

#' MBITES: Sugar Energetics
#'
#' Add energy derived from sugar to the mosquito's fuel tank. This method is called from \code{\link{mbites_boutS}}.
#' This function resets the \code{boutFail} counter back to 0.
#'  * This method is bound to \code{MosquitoFemale$sugarMeal}.
#'
mbites_sugarMeal <- function(){ # called from MBITES-Bouts.R, boutS
  private$boutFail = 0L # reset bout failure to 0
  private$energy = 1 # always top up to full
  if(!private$mature){
    private$energyPreG = private$energyPreG - MBITES:::Parameters$get_preGsugar()
    if(private$energyPreG <= 0 & private$mated){
      private$mature = TRUE
    }
  }
}


#' MBITES: Energetics for \code{\link{MosquitoFemale}}
#'
#' Handle energy dependent mortality and sugar bout queueing as function of current mosquito energy levels.
#'  * This method is bound to \code{MosquitoFemale$sugarEnergetics()}.
#'
mbites_energetics <- function(){
  if(private$state != "D"){
    self$flightBurnEnergy()
    self$queueSugarBout()
  }
}

mbites_flightBurnEnergy <- function(){
  private$energy = max(0,private$energy - MBITES:::Parameters$get_S_u())
}

mbites_queueSugarBout <- function(){
  if(runif(1) < self$pSugarBout()){
    private$starved = TRUE
  }
}

mbites_pSugarBout <- function(){
  S_sb = MBITES:::Parameters$get_S_sb()
  S_sa = MBITES:::Parameters$get_S_sa()
  (2+S_sb)/(1+S_sb)-exp(S_sa*private$energy)/(S_sb+exp(S_sa*private$energy))
}
