###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MICRO
#   M-BITES: Timing
#   MASH Team
#   January 2018
#
###############################################################################

#################################################################
# M-BITES: Dwell Times
#################################################################

#' M-BITES: Exponential Timing for \code{MosquitoFemale}
#'
#' Method for exponentially-distributed bout lengths (model mosquitoes as a Markov process).
#'  * This method is bound to \code{MosquitoFemale$timing()}.
#'
mbites_timingExponential <- function(){
  if(self$isActive()){
    duration = switch(private$state,
      F = {private$FemalePopPointer$get_MBITES_PAR("F_time")},
      B = {private$FemalePopPointer$get_MBITES_PAR("B_time")},
      R = {private$FemalePopPointer$get_MBITES_PAR("R_time")},
      L = {private$FemalePopPointer$get_MBITES_PAR("L_time")},
      O = {private$FemalePopPointer$get_MBITES_PAR("O_time")},
      M = {private$FemalePopPointer$get_MBITES_PAR("M_time")},
      S = {private$FemalePopPointer$get_MBITES_PAR("S_time")}
    )
    private$tNext = private$tNow + rexp(n=1,rate=1/duration)
  }
}

#' M-BITES: Gamma Timing for \code{MosquitoFemale}
#'
#' Method for Gamma-distributed bout lengths (model mosquitoes as a semi-Markov process).
#'  * This method is bound to \code{MosquitoFemale$timing()}.
#'
mbites_timingGamma <- function(){
  if(self$isActive()){
    duration = switch(private$state,
      F = {private$FemalePopPointer$get_MBITES_PAR("F_time")},
      B = {private$FemalePopPointer$get_MBITES_PAR("B_time")},
      R = {private$FemalePopPointer$get_MBITES_PAR("R_time")},
      L = {private$FemalePopPointer$get_MBITES_PAR("L_time")},
      O = {private$FemalePopPointer$get_MBITES_PAR("O_time")},
      M = {private$FemalePopPointer$get_MBITES_PAR("M_time")},
      S = {private$FemalePopPointer$get_MBITES_PAR("S_time")}
    )
    private$tNext = private$tNow + rgamma(n=1,shape=private$FemalePopPointer$get_MBITES_PAR("gammaShape"),rate=(1/duration)*private$FemalePopPointer$get_MBITES_PAR("gammaShape"))
  }
}
