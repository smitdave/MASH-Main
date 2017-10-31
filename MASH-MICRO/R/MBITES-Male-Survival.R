###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES-Male: Survival
#   MASH-MICRO Team
#   September 18, 2017
#
###############################################################################


###############################################################################
# Resting Surival
###############################################################################

#' MBITES-Male: Resting Survival for \code{\link{MosquitoMale}}
#'
#' Run generic resting survival probailities for bouts (launch to launch).
#'  * This method is bound to \code{MosquitoMale$surviveResting()}.
#'
mbitesMale_surviveResting <- function(){
  if(self$isAlive()){
    if(runif(1) < self$get_restHaz()){
      private$stateNew = "D"
    }
  }
}

#' MBITES-Male: Get Resting Hazards for \code{\link{MosquitoMale}}
#'
#' Get resting hazards for \code{\link{mbitesGeneric_surviveResting}}.
#'  * This method is bound to \code{MosquitoMale$get_restHaz()}.
#'
mbitesMale_get_restHaz <- function(){
  switch(private$pSetNow,
    m = {return(private$LandscapePointer$get_MatingSites(private$locNow)$get_haz())},
    s = {return(private$LandscapePointer$get_SugarSites(private$locNow)$get_haz())},
    {stop(cat("illegal point set for MBITES-Male: ",private$pSetNow,"\n",sep=""))}
  )
}


###############################################################################
# Flight Survival
###############################################################################

#' MBITES-Male: Flight Survival for \code{\link{MosquitoMale}}
#'
#' Run generic flight survival probailities for bouts (launch to launch).
#' Depending on settings from M-BITES parameters, senescence and/or tattering may also be simulated.
#'  * This method is bound to \code{MosquitoMale$surviveFlight()}.
#'
mbitesMale_surviveFlight <- function(){
  if(self$isAlive()){
    p = self$get_surviveFlightProb()
    if(private$MalePopPointer$get_MBITES_PAR("TATTER")){
      private$damage = private$damage + self$WingTattering()
      p = p * self$pTatter()
    }
    if(private$MalePopPointer$get_MBITES_PAR("SENESCE")){
      p = p * self$pSenesce()
    }
    if(runif(1) < 1-p){
      private$stateNew = "D"
    }
  }
}

#' MBITES-Male: Probability of Death due to Senescence for \code{\link{MosquitoMale}}
#'
#' probability of death due to senescence given by \deqn{ \frac{2+sns.b}{1+sns.b} - \frac{e^{sns.a\times age}}{sns.b+e^{sns.a\times age}} }
#'  * This method is bound to \code{MosquitoMale$pSenesce()}.
#'
mbitesMale_pSenesce <- function(){
  age = private$tNow - private$bDay
  return((2+private$MalePopPointer$get_MBITES_PAR("sns.b"))/(1+private$MalePopPointer$get_MBITES_PAR("sns.b")) - exp(private$MalePopPointer$get_MBITES_PAR("sns.a")*private$age)/(private$MalePopPointer$get_MBITES_PAR("sns.b") + exp(private$MalePopPointer$get_MBITES_PAR("sns.a")*private$age)))
}

#' MBITES-Generic: Wing Tattering for \code{\link{MosquitoFemale}}
#'
#' Draw from a zero-inflated Beta distribution for additive wing damage from tattering.
#' Wing damage is given by \deqn{ \left\{\begin{matrix}
#' x=0; P(ttsz.p)
#' \\
#' x\sim Beta(ttsz.a,ttsz.b); P(1-ttsz.p)
#' \end{matrix}\right. }
#'  * This method is bound to \code{MosquitoMale$WingTattering()}.
#' @md
mbitesMale_WingTattering <- function(){
  if(runif(1) < private$MalePopPointer$get_MBITES_PAR("ttsz.p")){
    return(0)
  } else {
    return(rbeta(1,private$MalePopPointer$get_MBITES_PAR("ttsz.a"),private$MalePopPointer$get_MBITES_PAR("ttsz.b")))
  }
}

#' MBITES-Generic: Probability of Death due to Wing Tattering for \code{\link{MosquitoFemale}}
#'
#' probability of death due to tattering given by \deqn{ \frac{2+ttr.b}{1+ttr.b} - \frac{e^{damage\times ttr.a}}{ttr.b+e^{damage\times ttr.a}} }
#'  * This method is bound to \code{MosquitoFemale$pTatter()}.
#' @md
mbitesMale_pTatter <- function(){
  return((2+private$MalePopPointer$get_MBITES_PAR("ttr.b"))/(1+private$MalePopPointer$get_MBITES_PAR("ttr.b")) - exp(private$damage*private$MalePopPointer$get_MBITES_PAR("ttr.a"))/(private$MalePopPointer$get_MBITES_PAR("ttr.b") + exp(private$damage*private$MalePopPointer$get_MBITES_PAR("ttr.a"))))
}

#' MBITES-Male: Get Baseline Survival Probability for \code{\link{MosquitoMale}}
#'
#' Get baseline flight survival probability for \code{\link{mbitesGeneric_surviveFlight}}.
#'  * This method is bound to \code{MosquitoMale$get_surviveFlightProb()}.
#'
mbitesMale_get_surviveFlightProb <- function(){
  switch(private$state,
    M = {return(private$MalePopPointer$get_MBITES_PAR("M_surv"))},
    S = {return(private$MalePopPointer$get_MBITES_PAR("S_surv"))},
    R = {return(private$MalePopPointer$get_MBITES_PAR("R_surv"))},
    {stop("illegal behavioral state for MBITES-Male")}
  )
}
