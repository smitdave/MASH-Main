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

###############################################################################
# Per-bout survival
###############################################################################

#' M-BITES: Simulate survival, per bout \code{\link{MosquitoFemale}}
#'
#' Get baseline survival probability for \code{\link{mbites_surviveFlight}}.
#'  * This method is bound to \code{MosquitoFemale$survival}.
#'
mbites_survival <- function(){
  self$surviveFlight()
  self$surviveHazards()
}

###############################################################################
# Flight Survival
###############################################################################

#' M-BITES: Get Baseline Survival Probability for \code{\link{MosquitoFemale}}
#'
#' Get baseline survival probability for \code{\link{mbites_surviveFlight}}.
#'  * This method is bound to \code{MosquitoFemale$get_surviveFlightProb()}.
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

#' MBITES-Generic: Flight Survival for \code{\link{MosquitoFemale}}
#'
#' Run generic flight survival probailities for bouts (launch to launch).
#' Depending on settings from M-BITES parameters, senescence and/or tattering may also be simulated.
#'  * This method is bound to \code{MosquitoFemale$surviveFlight()}.
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

#' MBITES-Generic: Probability of Death due to the Blood Meal during the Post Prandial Flight  \code{\link{MosquitoFemale}}
#'
#' Incremental mortality as a function of being laden during the post-prandial flight \deqn{ \frac{e^{S.a\times energy}}{S.b+e^{S.a\times energy}} }
#'  * This method is bound to \code{MosquitoFemale$pEnergySurvival()}.
#'
mbites_pPPRFlight <- function(){
  PPR_a = MBITES:::Parameters$get_PPR_a()
  PPR_b = MBITES:::Parameters$get_PPR_b()
  exp(PPR_a*private$bmSize)/(PPR_b + exp(PPR_a*private$bmSize))
}

#' MBITES-Generic: Probability of Death due to Energy Reserves for \code{\link{MosquitoFemale}}
#'
#' Incremental mortality as a function of energy reserves given by \deqn{ \frac{e^{S.a\times energy}}{S.b+e^{S.a\times energy}} }
#'  * This method is bound to \code{MosquitoFemale$pEnergySurvival()}.
#'
mbites_pEnergySurvival <- function(){
  S_a = MBITES:::Parameters$get_S_a()
  S_b = MBITES:::Parameters$get_S_b()
  exp(S_a*private$energy)/(S_b + exp(S_a*private$energy))

}


###############################################################################
#  Damage (wing tattering, chemical, physical)
###############################################################################

#' MBITES-Generic: Mean Wing Tattering for \code{\link{MosquitoFemale}}
#'
#' Draw from a zero-inflated Beta distribution for additive wing damage_physical from tattering.
#' Wing damage_physical is given by \deqn{ \left\{\begin{matrix}
#' x=0; P(ttsz.p)
#' \\
#' x\sim Beta(ttsz.a,ttsz.b); P(1-ttsz.p)
#' \end{matrix}\right. }
#'  * This method is bound to \code{MosquitoFemale$WingTattering()}.
#'
mbites_WingTattering <- function(){
  if(runif(1) < MBITES:::Parameters$get_ttsz_p()){
    return(0)
  } else {
    return(rbeta(1,MBITES:::Parameters$get_ttsz_a(),MBITES:::Parameters$get_ttsz_b()))
  }
}

###############################################################################
# Survive Physical Damage
###############################################################################

#' MBITES-Generic: Probability of Death due to Wing Tattering for \code{\link{MosquitoFemale}}
#'
#' probability of death due to tattering given by \deqn{ \frac{2+ttr.b}{1+ttr.b} - \frac{e^{damage_physical\times ttr.a}}{ttr.b+e^{damage_physical\times ttr.a}} }
#'  * This method is bound to \code{MosquitoFemale$pTatter()}.
#'
mbites_pTatter <- function(){
  ttr_b = MBITES:::Parameters$get_ttr_b()
  ttr_a = MBITES:::Parameters$get_ttr_a()
  (2+ttr_b)/(1+ttr_b) - exp(private$damage_physical*ttr_a)/(ttr_b + exp(private$damage_physical*ttr_a))
}

###############################################################################
# Survive Chemical Damage
###############################################################################

#' MBITES-Generic: Probability of Death due to Chemical Damage for \code{\link{MosquitoFemale}}
#'
#' probability of death due to tattering given by \deqn{
#' \frac{2+chm.b}{1+chm.b} - \frac{e^{damage_physical\times chm.a}}{chm.b+e^{damage_chemical\times chm.a}} }
#'  * This method is bound to \code{MosquitoFemale$pTatter()}.
#' @md
mbites_pChem<- function(){
  chm_a = MBITES:::Parameters$get_chm_a()
  chm_b = MBITES:::Parameters$get_chm_b()
  (2+chm_b)/(1+chm_b) - exp(private$damage_chemicall*chm_a)/(chm_b + exp(private$damage_chemicall*chm_a))
}

###############################################################################
# Senescence
###############################################################################

#' MBITES-Generic: Probability of Death due to Senescence for \code{\link{MosquitoFemale}}
#'
#' probability of death due to senescence given by \deqn{ \frac{2+sns.b}{1+sns.b} - \frac{e^{sns.a\times age}}{sns.b+e^{sns.a\times age}} }
#'  * This method is bound to \code{MosquitoFemale$pSenesce()}.
#' @md
mbites_pSenesce <- function(){
  age = private$tNow - private$bDay
  sns_a = MBITES:::Parameters$get_sns_a()
  sns_b = MBITES:::Parameters$get_sns_b()
  (2+sns_b)/(1+sns_b) - exp(sns_a*age)/(sns_b + exp(sns_a*age))
}


###############################################################################
# Resting Survival
###############################################################################

#' MBITES-Generic: Resting Survival for \code{\link{MosquitoFemale}}
#'
#' Run generic resting survival probailities for bouts (launch to launch).
#'  * This method is bound to \code{MosquitoFemale$surviveResting()}.
#' @md
mbites_surviveResting <- function(){
  if(private$state != "D"){
    if(runif(1) < private$site$get_haz()){
      private$state = "D"
    }
  }
}
