###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES: Survival
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################

#################################################################
# Flight Survival
#################################################################

#' MBITES-Generic: Flight Survival for \code{\link{MosquitoFemale}}
#'
#' Run generic flight survival probailities for bouts (launch to launch).
#' Depending on settings from M-BITES parameters, senescence and/or tattering may also be simulated.
#'  * This method is bound to \code{MosquitoFemale$surviveFlight()}.
#' @md
mbitesGeneric_surviveFlight <- function(){
  if(self$isActive()){
    p = self$get_surviveFlightProb()
    if(private$FemalePopPointer$get_MBITES_PAR("TATTER")){
      private$damage = private$damage + self$rTatterSize()
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


#################################################################
# Wing Tattering
#################################################################

mbitesGeneric_rTatterSize_Exact <- function(){
  
}


#' MBITES-Generic: Wing Tattering for \code{\link{MosquitoFemale}}
#'
#' Draw from a zero-inflated Beta distribution for additive wing damage from tattering.
#' Wing damage is given by \deqn{ \left\{\begin{matrix}
#' x=0; P(ttsz.p)
#' \\
#' x\sim Beta(ttsz.a,ttsz.b); P(1-ttsz.p)
#' \end{matrix}\right. }
#'  * This method is bound to \code{MosquitoFemale$rTatterSize()}.
#' @md
mbitesGeneric_rTatterSize <- function(){
  if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("ttsz.p")){
    return(0)
  } else {
    return(rbeta(1,private$FemalePopPointer$get_MBITES_PAR("ttsz.a"),private$FemalePopPointer$get_MBITES_PAR("ttsz.b")))
  }
}

#' MBITES-Generic: Probability of Death due to Wing Tattering for \code{\link{MosquitoFemale}}
#'
#' probability of death due to tattering given by \deqn{ \frac{2+ttr.b}{1+ttr.b} - \frac{e^{damage\times ttr.a}}{ttr.b+e^{damage\times ttr.a}} }
#'  * This method is bound to \code{MosquitoFemale$pTatter()}.
#' @md
mbitesGeneric_pTatter <- function(){
  return((2+private$FemalePopPointer$get_MBITES_PAR("ttr.b"))/(1+private$FemalePopPointer$get_MBITES_PAR("ttr.b")) - exp(private$damage*private$FemalePopPointer$get_MBITES_PAR("ttr.a"))/(private$FemalePopPointer$get_MBITES_PAR("ttr.b") + exp(private$damage*private$FemalePopPointer$get_MBITES_PAR("ttr.a"))))
}


#################################################################
# Senescence
#################################################################

#' MBITES-Generic: Probability of Death due to Senescence for \code{\link{MosquitoFemale}}
#'
#' probability of death due to senescence given by \deqn{ \frac{2+sns.b}{1+sns.b} - \frac{e^{sns.a\times age}}{sns.b+e^{sns.a\times age}} }
#'  * This method is bound to \code{MosquitoFemale$pSenesce()}.
#' @md
mbitesGeneric_pSenesce <- function(){
  age = private$tNow - private$bDay
  return((2+private$FemalePopPointer$get_MBITES_PAR("sns.b"))/(1+private$FemalePopPointer$get_MBITES_PAR("sns.b")) - exp(private$FemalePopPointer$get_MBITES_PAR("sns.a")*private$age)/(private$FemalePopPointer$get_MBITES_PAR("sns.b") + exp(private$FemalePopPointer$get_MBITES_PAR("sns.a")*private$age)))
}


##########################################
# Resting Survival
##########################################

#' MBITES-Generic: Resting Survival for \code{\link{MosquitoFemale}}
#'
#' Run generic resting survival probailities for bouts (launch to launch).
#'  * This method is bound to \code{MosquitoFemale$surviveResting()}.
#' @md
mbitesGeneric_surviveResting <- function(){
  if(self$isActive()){
    if(runif(1) < self$get_restHaz()){
      private$stateNew = "D"
    }
  }
}
