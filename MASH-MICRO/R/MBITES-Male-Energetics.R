###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES-Male: Energetics
#   MASH-MICRO Team
#   September 18, 2017
#
###############################################################################

###############################################################################
# Sugar Energetics
###############################################################################

#' MBITES-Male: Sugar Energetics for \code{\link{MosquitoMale}}
#'
#' Handle energy dependent mortality and sugar bout queueing as function of current mosquito energy levels.
#'  * This method is bound to \code{MosquitoMale$sugarEnergetics()}.
#'
mbitesMale_sugarEnergetics <- function(){
  if(self$isAlive()){
    private$energy = max(0,private$energy - private$MalePopPointer$get_MBITES_PAR("S.u.m"))

    if(runif(1) < 1-self$pEnergySurvival()){
      private$stateNew = "D"
    } else {
      self$queueSugarBout()
    }

  }
}

#' MBITES-Male: Probability of Death due to Energy Reserves for \code{\link{MosquitoMale}}
#'
#' Calculate mortality probability as a function of energy reserves.
#'  * This method is bound to \code{MosquitoMale$pEnergySurvival()}.
#'
mbitesMale_pEnergySurvival <- function(){
  return(
    exp(private$MalePopPointer$get_MBITES_PAR("S.a.m")*private$energy) / (private$MalePopPointer$get_MBITES_PAR("S.b.m") + exp(private$MalePopPointer$get_MBITES_PAR("S.a.m")*private$energy))
  )
}

#' MBITES-Male: Probability to Queue Sugar Bout due to Energy Reserves for \code{\link{MosquitoMale}}
#'
#' Calculate sugar bout probability as a function of energy reserves.
#'  * This method is bound to \code{MosquitoMale$pSugarBout()}.
#'
mbitesMale_pSugarBout <- function(){
  return(
    (2+private$MalePopPointer$get_MBITES_PAR("S.sb.m"))/(1+private$MalePopPointer$get_MBITES_PAR("S.sb.m"))-exp(private$MalePopPointer$get_MBITES_PAR("S.sa.m")*private$energy)/(private$MalePopPointer$get_MBITES_PAR("S.sb.m")+exp(private$MalePopPointer$get_MBITES_PAR("S.sa.m")*private$energy))
  )
}

#' MBITES-Male: Queue Sugar Bout due to Energy Reserves for \code{\link{MosquitoMale}}
#'
#' Potentially queue a sugar bout \code{\link{mbitesMale_boutS}} as a function of energy reserves.
#'  * This method is bound to \code{MosquitoMale$queueSugarBout()}.
#'
mbitesMale_queueSugarBout <- function(){
  if(runif(1) < self$pSugarBout()){
    private$stateNew = "S"
  }
}
