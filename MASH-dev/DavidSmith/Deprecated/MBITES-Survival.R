###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   M-BITES: Survival
#   MASH-MICRO Team
#   September 2017
#
###############################################################################



#' M-BITES: Simulate survival, per bout \code{\link{MosquitoFemale}}
#'
#' Get baseline survival probability for \code{\link{mbites_surviveFlight}}.
#'  * This method is bound to \code{MosquitoFemale$get_surviveFlightProb()}.
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

  if(private$search == TRUE){
    switch(private$search,
      B = {return(private$FemalePopPointer$get_MBITES_PAR("Bs_surv"))},
      O = {return(private$FemalePopPointer$get_MBITES_PAR("Os_surv"))},
      M = {return(private$FemalePopPointer$get_MBITES_PAR("Ms_surv"))},
      S = {return(private$FemalePopPointer$get_MBITES_PAR("Ss_surv"))},
      {stop("illegal behavioral state for M-BITES")}
    )} else{
    switch(private$state,
      B = {return(private$FemalePopPointer$get_MBITES_PAR("B_surv"))},
      O = {return(private$FemalePopPointer$get_MBITES_PAR("O_surv"))},
      M = {return(private$FemalePopPointer$get_MBITES_PAR("M_surv"))},
      S = {return(private$FemalePopPointer$get_MBITES_PAR("S_surv"))},
      {stop("illegal behavioral state for M-BITES")}
    )
  }
}

#' MBITES-Generic: Flight Survival for \code{\link{MosquitoFemale}}
#'
#' Run generic flight survival probailities for bouts (launch to launch).
#' Depending on settings from M-BITES parameters, senescence and/or tattering may also be simulated.
#'  * This method is bound to \code{MosquitoFemale$surviveFlight()}.
#' @md
mbites_surviveFlight <- function(){
  if(self$isActive()){
    p = self$get_surviveFlightBaseProb()
    p = p * self$pPPRFlight()
    p = p * self$pEnergySurvival()
    p = p * self$pChem()
    if(private$FemalePopPointer$get_MBITES_PAR("TATTER")){
      private$damage_physical = private$damage_physical + self$WingTattering()
      p = p * self$pTatter()
    }
    if(private$FemalePopPointer$get_MBITES_PAR("SENESCE")){
      p = p * self$pSenesce()
    }
    if(runif(1) < 1-p){
      private$stateNew = "D"
    }
  }
}

#' MBITES-Generic: Probability of Death due to the Blood Meal during the Post Prandial Flight  \code{\link{MosquitoFemale}}
#'
#' Incremental mortality as a function of being laden during the post-prandial flight \deqn{ \frac{e^{S.a\times energy}}{S.b+e^{S.a\times energy}} }
#'  * This method is bound to \code{MosquitoFemale$pEnergySurvival()}.
#' @md
mbites_pPPRFlight <- function(){
  return(
    exp(private$FemalePopPointer$get_MBITES_PAR("PPR.a")*private$bmSize)/(private$FemalePopPointer$get_MBITES_PAR("PPR.b") + exp(private$FemalePopPointer$get_MBITES_PAR("PPR.a")*private$bmSize))
  )
}


#' MBITES-Generic: Probability of Death due to Energy Reserves for \code{\link{MosquitoFemale}}
#'
#' Incremental mortality as a function of energy reserves given by \deqn{ \frac{e^{S.a\times energy}}{S.b+e^{S.a\times energy}} }
#'  * This method is bound to \code{MosquitoFemale$pEnergySurvival()}.
#' @md
mbites_pEnergySurvival <- function(){
  return(
    exp(private$FemalePopPointer$get_MBITES_PAR("S.a")*private$energy)/(private$FemalePopPointer$get_MBITES_PAR("S.b") + exp(private$FemalePopPointer$get_MBITES_PAR("S.a")*private$energy))
  )
}

#################################################################
# Wing Tattering
#################################################################

#' MBITES-Generic: Exact Wing Tattering for \code{\link{MosquitoFemale}}
#'
#' write me!
#'
#'  * This method is bound to \code{MosquitoFemale$WingTattering()}.
#'
mbites_WingTattering_Exact <- function(){
  cat("write me! i should give beta-distributed wing tattering as function of flight distance\n")
  stop()
}


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
mbites_WingTattering_Mean <- function(){
  if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("ttsz.p")){
    return(0)
  } else {
    return(rbeta(1,private$FemalePopPointer$get_MBITES_PAR("ttsz.a"),private$FemalePopPointer$get_MBITES_PAR("ttsz.b")))
  }
}

#################################################################
# Survive Physical Damage
#################################################################

#' MBITES-Generic: Probability of Death due to Wing Tattering for \code{\link{MosquitoFemale}}
#'
#' probability of death due to tattering given by \deqn{ \frac{2+ttr.b}{1+ttr.b} - \frac{e^{damage_physical\times ttr.a}}{ttr.b+e^{damage_physical\times ttr.a}} }
#'  * This method is bound to \code{MosquitoFemale$pTatter()}.
#' @md
mbites_pTatter <- function(){
  return((2+private$FemalePopPointer$get_MBITES_PAR("ttr.b"))/(1+private$FemalePopPointer$get_MBITES_PAR("ttr.b")) - exp(private$damage_physical*private$FemalePopPointer$get_MBITES_PAR("ttr.a"))/(private$FemalePopPointer$get_MBITES_PAR("ttr.b") + exp(private$damage_physical*private$FemalePopPointer$get_MBITES_PAR("ttr.a"))))
}

#################################################################
# Survive Chemical Damage
#################################################################

#' MBITES-Generic: Probability of Death due to Chemical Damage for \code{\link{MosquitoFemale}}
#'
#' probability of death due to tattering given by \deqn{
\frac{2+chm.b}{1+chm.b} - \frac{e^{damage_physical\times chm.a}}{chm.b+e^{damage_chemical\times chm.a}} }
#'  * This method is bound to \code{MosquitoFemale$pTatter()}.
#' @md
mbites_pChem<- function(){
  return((2+private$FemalePopPointer$get_MBITES_PAR("chm.b"))/(1+private$FemalePopPointer$get_MBITES_PAR("chm.b")) - exp(private$damage_chemicall*private$FemalePopPointer$get_MBITES_PAR("chm.a"))/(private$FemalePopPointer$get_MBITES_PAR("chm.b") + exp(private$damage_chemicall*private$FemalePopPointer$get_MBITES_PAR("chm.a"))))
}

#################################################################
# Senescence
#################################################################

#' MBITES-Generic: Probability of Death due to Senescence for \code{\link{MosquitoFemale}}
#'
#' probability of death due to senescence given by \deqn{ \frac{2+sns.b}{1+sns.b} - \frac{e^{sns.a\times age}}{sns.b+e^{sns.a\times age}} }
#'  * This method is bound to \code{MosquitoFemale$pSenesce()}.
#' @md
mbites_pSenesce <- function(){
  age = private$tNow - private$bDay
  return((2+private$FemalePopPointer$get_MBITES_PAR("sns.b"))/(1+private$FemalePopPointer$get_MBITES_PAR("sns.b")) - exp(private$FemalePopPointer$get_MBITES_PAR("sns.a")*private$age)/(private$FemalePopPointer$get_MBITES_PAR("sns.b") + exp(private$FemalePopPointer$get_MBITES_PAR("sns.a")*private$age)))
}


###############################################################################
# Resting Survival
###############################################################################

#' M-BITES: Get Resting Hazards for \code{\link{MosquitoFemale}}
#'
#' Get resting hazards for \code{\link{mbites_surviveResting}}.
#'  * This method is bound to \code{MosquitoFemale$get_restHaz()}.
#'
mbites_get_restHaz <- function(){
  switch(private$pSetNow,
    f = {return(private$LandscapePointer$get_FeedingSites(private$locNow)$get_hazLspot(private$lspot))},
    l = {return(private$LandscapePointer$get_AquaSites(private$locNow)$get_haz())},
    m = {return(private$LandscapePointer$get_MatingSites(private$locNow)$get_haz())},
    s = {return(private$LandscapePointer$get_SugarSites(private$locNow)$get_haz())},
    {stop("illegal point set for M-BITES")}
  )
}

#' MBITES-Generic: Resting Survival for \code{\link{MosquitoFemale}}
#'
#' Run generic resting survival probailities for bouts (launch to launch).
#'  * This method is bound to \code{MosquitoFemale$surviveResting()}.
#' @md
mbites_surviveResting <- function(){
  if(self$isActive()){
    if(runif(1) < self$get_restHaz()){
      private$stateNew = "D"
    }
  }
}
