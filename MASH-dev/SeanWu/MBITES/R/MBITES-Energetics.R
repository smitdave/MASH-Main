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
#'  * This method is bound to \code{MosquitoFemale$BloodEnergetics}
#'
mbites_BloodEnergetics <- function(){ # called from MBITES-Bloodmeal.R
  topUp = self$energyFromBlood() # energy derived from blood meal is function of meal size
  private$energy = min(1, private$energy + topUp)
  if(!private$mature){
    private$energyPreG = private$energyPreG - topUp
    if(private$energyPreG <= 0 & private$mated){
      private$mature = TRUE
    }
  }
}

#' MBITES: Energy derived from blood
#'
#' write me!
#'  * This method is bound to \code{Mosquito_Female$energyFromBlood}.
#'
mbites_energyFromBlood <- function(){
  bmSize / (MBITES:::Parameters$get_energyFromBlood_b() + bmSize)
}

# set methods
Mosquito$set(which = "public",name = "BloodEnergetics",
    value = mbites_BloodEnergetics, overwrite = TRUE
)

Mosquito$set(which = "public",name = "energyFromBlood",
    value = mbites_energyFromBlood, overwrite = TRUE
)


###############################################################################
# Sugar Energetics
###############################################################################

#' MBITES: Choose Sugar Source
#'
#' During a sugar bout the mosquito samples from available sugar feeding resources
#' at the \code{\link{Site}} it is at.
#'  * this method is bound to \code{Mosquito$chooseSugarSource}
#'
mbites_chooseSugarSource <- function(){
  # when atsb, etc exist, do the checks here
  private$sugar_res = private$site$sample_sugar() # sample resources
  private$sugarID = 1L # normal sugar source

}

#' MBITES: Sugar Energetics
#'
#' Add energy derived from sugar to the mosquito's fuel tank, and if the mosquito is immature, subtract energy
#' derived from this sugar meal from the pre-gonotrophic energy requirement. If the mosquito is mated
#' and has passed the energy requirement, set \code{mature = TRUE}.
#'  * This method is bound to \code{Mosquito_Female$sugarMeal}
#'
mbites_sugarMeal_F <- function(){ # called from MBITES-Bouts.R, boutS
  private$energy = 1 # always top up to full
  if(!private$mature){
    private$energyPreG = private$energyPreG - MBITES:::Parameters$get_preGsugar()
    if(private$energyPreG <= 0 & private$mated){
      private$mature = TRUE
    }
  }
}

#' MBITES: Energetics
#'
#' Calculate energy burned from flight with \code{\link{mbites_flightBurnEnergy}} and potentially queue
#' a sugar bout with \code{\link{mbites_queueSugarBout}}.
#'  * This method is bound to \code{Mosquito$energetics}
#'
mbites_energetics <- function(){
  if(private$state != "D"){
    self$flightBurnEnergy()
    self$queueSugarBout()
  }
}

#' MBITES: Energy burn from flight
#'
#' Calculate energy in the mosquito's fuel tank after a flight.
#'  * This method is bound to \code{Mosquito$flightBurnEnergy()}
#'
mbites_flightBurnEnergy <- function(){
  private$energy = max(0,private$energy - MBITES:::Parameters$get_S_u())
}

#' MBITES: Queue a sugar bout
#'
#' If the mosquito queues a sugar bout (with probability \code{\link{mbites_pSugarBout}}), set \code{starved} flag true.
#'  * This method is bound to \code{Mosquito$queueSugarBout}
#'
mbites_queueSugarBout <- function(){
  if(runif(1) < self$pSugarBout()){
    private$starved = TRUE
  }
}

#' MBITES: Probability of a sugar bout
#'
#' Calculate the probability to queue a sugar bout as a function of the mosquito's current energy levels.
#' The probability is given by \eqn{\frac{2+S_{sb}}{1+S_{sb}} - \frac{e^{S_{sa} \times energy}}{S_{sb}+e^{S_{sa}\times energy}}}
#'  * This method is bound to \code{Mosquito$pSugarBout}
#'
mbites_pSugarBout <- function(){
  S_sb = MBITES:::Parameters$get_S_sb()
  S_sa = MBITES:::Parameters$get_S_sa()
  (2+S_sb)/(1+S_sb)-exp(S_sa*private$energy)/(S_sb+exp(S_sa*private$energy))
}

# set methods
Mosquito$set(which = "public",name = "chooseSugarSource",
    value = mbites_chooseSugarSource, overwrite = TRUE
)

Mosquito_Female$set(which = "public",name = "sugarMeal",
    value = mbites_sugarMeal_F, overwrite = TRUE
)

Mosquito$set(which = "public",name = "energetics",
    value = mbites_energetics, overwrite = TRUE
)

Mosquito$set(which = "public",name = "flightBurnEnergy",
    value = mbites_flightBurnEnergy, overwrite = TRUE
)

Mosquito$set(which = "public",name = "queueSugarBout",
    value = mbites_queueSugarBout, overwrite = TRUE
)

Mosquito$set(which = "public",name = "pSugarBout",
    value = mbites_pSugarBout, overwrite = TRUE
)
