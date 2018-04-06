###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Survival
#     MBITES Team
#     March 2018
#
###############################################################################

#' MBITES: Survival
#'
#' Survival from flight and local hazards is calculated after the mosquito chooses
#' a resting spot, after the "attempt" part of the bout (called in \code{\link{mbites_updateState}}).
#'
#'
#' @name Survival
NULL
#> NULL


###############################################################################
# Per-bout survival
###############################################################################

#' MBITES: Per-bout Survival
#'
#' Simulate flight survival and survival of local hazards.
#'  * This method is bound to \code{Mosquito$survival}
#'
mbites_survival <- function(){
  self$surviveFlight()
  self$surviveHazards()
}

# set methods
Mosquito$set(which = "public",name = "survival",
    value = mbites_survival, overwrite = TRUE
)


###############################################################################
# Flight Survival
###############################################################################

#' MBITES: Get Baseline Flight Survival Probability
#'
#' Get baseline survival probability for \code{\link{mbites_surviveFlight}}.
#'  * This method is bound to \code{Mosquito$get_surviveFlightProb}.
#'
mbites_get_surviveFlightBaseProb <- function(){

  if(private$search){
    switch(private$search,
      B = {return(MBITES:::Parameters$get_Bs_surv())},
      O = {return(MBITES:::Parameters$get_Os_surv())},
      M = {return(MBITES:::Parameters$get_Ms_surv())},
      S = {return(MBITES:::Parameters$get_Ss_surv())},
      {stop("illegal behavioral state for M-BITES")}
    )
  } else {
    switch(private$state,
      B = {return(MBITES:::Parameters$get_B_surv())},
      O = {return(MBITES:::Parameters$get_O_surv())},
      M = {return(MBITES:::Parameters$get_M_surv())},
      S = {return(MBITES:::Parameters$get_S_surv())},
      {stop("illegal behavioral state for M-BITES")}
    )
  }
}

#' MBITES: Flight Survival
#'
#' Run generic flight survival probailities for bouts (launch to launch).
#' Survival is calculated as:
#'  1. Get baseline flight survival probability
#'  2. Multiply by the effect due to the blood meal during post-prandial flight
#'  3. Multiply by the effect due to energy levels
#'  4. Multiply by the effect due to chemical damage
#'  5. Multiply by the effect due to wing tattering and physical damage
#'  6. Multiply by the effect due to senescence
#'
#' Depending on settings from M-BITES parameters, senescence and/or tattering may also be simulated.
#'  * This method is bound to \code{Mosquito$surviveFlight}.
#'
mbites_surviveFlight <- function(){
  if(private$state != "D"){
    p = self$get_surviveFlightBaseProb()
    p = p * self$pPPRFlight()
    p = p * self$pEnergySurvival()
    p = p * self$pChem()

    # tattering
    if(MBITES:::Parameters$get_TATTER()){
      private$damage_physical = private$damage_physical + self$WingTattering()
      p = p * self$pTatter()
    }
    # senescence
    if(MBITES:::Parameters$get_SENESCE()){
      p = p * self$pSenesce()
    }
    if(runif(1) < 1-p){
      private$state = "D"
    }
  }
}

#' MBITES: Probability of Death due to the Blood Meal during the Post Prandial Flight
#'
#' Incremental mortality as a function of being laden during the post-prandial flight \eqn{ \frac{e^{S.a\times energy}}{S.b+e^{S.a\times energy}} }
#'  * This method is bound to \code{Mosquito$pEnergySurvival}.
#'
mbites_pPPRFlight <- function(){
  PPR_a = MBITES:::Parameters$get_PPR_a()
  PPR_b = MBITES:::Parameters$get_PPR_b()
  exp(PPR_a*private$bmSize)/(PPR_b + exp(PPR_a*private$bmSize))
}

#' MBITES: Probability of Death due to Energy Reserves
#'
#' Incremental mortality as a function of energy reserves given by \eqn{ \frac{e^{S.a\times energy}}{S.b+e^{S.a\times energy}} }
#'  * This method is bound to \code{Mosquito$pEnergySurvival}.
#'
mbites_pEnergySurvival <- function(){
  S_a = MBITES:::Parameters$get_S_a()
  S_b = MBITES:::Parameters$get_S_b()
  exp(S_a*private$energy)/(S_b + exp(S_a*private$energy))
}

# set methods
Mosquito$set(which = "public",name = "get_surviveFlightBaseProb",
    value = mbites_get_surviveFlightBaseProb, overwrite = TRUE
)

Mosquito$set(which = "public",name = "surviveFlight",
    value = mbites_surviveFlight, overwrite = TRUE
)

Mosquito$set(which = "public",name = "pPPRFlight",
    value = mbites_pPPRFlight, overwrite = TRUE
)

Mosquito$set(which = "public",name = "pEnergySurvival",
    value = mbites_pEnergySurvival, overwrite = TRUE
)


###############################################################################
#  Damage (wing tattering, chemical, physical)
###############################################################################

#' MBITES: Sample Wing Tattering Damage
#'
#' Draw from a zero-inflated Beta distribution for additive wing damage_physical from tattering.
#' Wing damage_physical is given by \deqn{ \left\{\begin{matrix}
#' x=0; P(ttsz.p)
#' \\
#' x\sim Beta(ttsz.a,ttsz.b); P(1-ttsz.p)
#' \end{matrix}\right. }
#'  * This method is bound to \code{Mosquito$WingTattering}.
#'
mbites_WingTattering <- function(){
  if(runif(1) < MBITES:::Parameters$get_ttsz_p()){
    return(0)
  } else {
    return(rbeta(1,MBITES:::Parameters$get_ttsz_a(),MBITES:::Parameters$get_ttsz_b()))
  }
}

# set methods
Mosquito$set(which = "public",name = "WingTattering",
    value = mbites_WingTattering, overwrite = TRUE
)


###############################################################################
# Survive Physical Damage
###############################################################################

#' MBITES: Probability of Death due to Wing Tattering
#'
#' probability of death due to tattering given by \deqn{ \frac{2+ttr.b}{1+ttr.b} - \frac{e^{damage_physical\times ttr.a}}{ttr.b+e^{damage_physical\times ttr.a}} }
#'  * This method is bound to \code{Mosquito$pTatter}.
#'
mbites_pTatter <- function(){
  ttr_b = MBITES:::Parameters$get_ttr_b()
  ttr_a = MBITES:::Parameters$get_ttr_a()
  (2+ttr_b)/(1+ttr_b) - exp(private$damage_physical*ttr_a)/(ttr_b + exp(private$damage_physical*ttr_a))
}

# set methods
Mosquito$set(which = "public",name = "pTatter",
    value = mbites_pTatter, overwrite = TRUE
)


###############################################################################
# Survive Chemical Damage
###############################################################################

#' MBITES: Probability of Death due to Chemical Damage
#'
#' Probability of death due to tattering given by \deqn{
#' \frac{2+chm.b}{1+chm.b} - \frac{e^{damage_physical\times chm.a}}{chm.b+e^{damage_chemical\times chm.a}} }
#'  * This method is bound to \code{Mosquito$pTatter}.
#'
mbites_pChem<- function(){
  chm_a = MBITES:::Parameters$get_chm_a()
  chm_b = MBITES:::Parameters$get_chm_b()
  (2+chm_b)/(1+chm_b) - exp(private$damage_chemicall*chm_a)/(chm_b + exp(private$damage_chemicall*chm_a))
}

# set methods
Mosquito$set(which = "public",name = "pChem",
    value = mbites_pChem, overwrite = TRUE
)


###############################################################################
# Senescence
###############################################################################

#' MBITES: Probability of Death due to Senescence
#'
#' probability of death due to senescence given by \deqn{ \frac{2+sns.b}{1+sns.b} - \frac{e^{sns.a\times age}}{sns.b+e^{sns.a\times age}} }
#'  * This method is bound to \code{Mosquito$pSenesce}.
#'
mbites_pSenesce <- function(){
  age = private$tNow - private$bDay
  sns_a = MBITES:::Parameters$get_sns_a()
  sns_b = MBITES:::Parameters$get_sns_b()
  (2+sns_b)/(1+sns_b) - exp(sns_a*age)/(sns_b + exp(sns_a*age))
}

# set methods
Mosquito$set(which = "public",name = "pSenesce",
    value = mbites_pSenesce, overwrite = TRUE
)


###############################################################################
# Local Hazards Survival
###############################################################################

#' MBITES: Survive Local Hazards
#'
#' Run generic local hazard survival probailities for bouts (launch to launch).
#'  * This method is bound to \code{Mosquito$surviveHazards}
#'
mbites_surviveHazards <- function(){
  if(private$state != "D"){
    if(runif(1) < private$site$get_haz()){
      private$state = "D"
    }
  }
}

# set methods
Mosquito$set(which = "public",name = "surviveHazards",
    value = mbites_surviveHazards, overwrite = TRUE
)
