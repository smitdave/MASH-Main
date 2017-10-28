###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES: Energetics
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################


###############################################################################
# Sugar Energetics
###############################################################################

#' MBITES-Generic: Sugar Energetics for \code{\link{MosquitoFemale}}
#'
#' Handle energy dependent mortality and sugar bout queueing as function of current mosquito energy levels.
#'  * This method is bound to \code{MosquitoFemale$sugarEnergetics()}.
#'
mbitesGeneric_sugarEnergetics <- function(){
  if(private$state != "R"){
    if(self$isAlive()){
      self$flightEnergetics()
      if(runif(1) < 1-self$pEnergySurvival()){
        private$stateNew = "D"
      } else {
        self$queueSugarBout()
      }
    }
  }
}

#' MBITES-Generic: Mean Flight Energetics for \code{\link{MosquitoFemale}}
#'
#' Reduce this mosquito's energy reserves by mean amount averaged over possible flight distances.
#'  * This method is bound to \code{MosquitoFemale$flightEnergetics()}.
#'
mbitesGeneric_flightEnergetics_Mean <- function(){
  private$energy = max(0,private$energy - private$FemalePopPointer$get_MBITES_PAR("S.u"))
}

#' MBITES-Generic: Exact Flight Energetics for \code{\link{MosquitoFemale}}
#'
#' Reduce this mosquito's energy reserves by Beta-distributed random variable as a function of flight distance.
#'  * This method is bound to \code{MosquitoFemale$flightEnergetics()}.
#'
mbitesGeneric_flightEnergetics_Exact <- function(){
  cat("write me! i should give beta-distributed energy consumption as function of flight distance\n")
  stop()
}

#' MBITES-Generic: Queue Sugar Bout due to Energy Reserves for \code{\link{MosquitoFemale}}
#'
#' Potentially queue a sugar bout \code{\link{mbitesBROS_boutS}} as a function of energy reserves.
#'  * This method is bound to \code{MosquitoFemale$queueSugarBout()}.
#'
mbitesGeneric_queueSugarBout <- function(){
  if(runif(1) < self$pSugarBout()){
    private$stateNew = "S"
  }
}

#' MBITES-Generic: Probability of Death due to Energy Reserves for \code{\link{MosquitoFemale}}
#'
#' Incremental mortality as a function of energy reserves given by \deqn{ \frac{e^{S.a\times energy}}{S.b+e^{S.a\times energy}} }
#'  * This method is bound to \code{MosquitoFemale$pEnergySurvival()}.
#' @md
mbitesGeneric_pEnergySurvival <- function(){
  return(
    exp(private$FemalePopPointer$get_MBITES_PAR("S.a")*private$energy)/(private$FemalePopPointer$get_MBITES_PAR("S.b") + exp(private$FemalePopPointer$get_MBITES_PAR("S.a")*private$energy))
  )
}

#' MBITES-Generic: Probability to Queue Sugar Bout due to Energy Reserves for \code{\link{MosquitoFemale}}
#'
#' Probability to queue sugar bout as function of energy reserves given by \deqn{ \frac{2+S.sb}{1+S.sb}-\frac{e^{S.sa\times energy}}{S.sb+e^{S.sa\times energy}} }
#'  * This method is bound to \code{MosquitoFemale$pSugarBout()}.
#' @md
mbitesGeneric_pSugarBout <- function(){
  return(
    (2+private$FemalePopPointer$get_MBITES_PAR("S.sb"))/(1+private$FemalePopPointer$get_MBITES_PAR("S.sb"))-exp(private$FemalePopPointer$get_MBITES_PAR("S.sa")*private$energy)/(private$FemalePopPointer$get_MBITES_PAR("S.sb")+exp(private$FemalePopPointer$get_MBITES_PAR("S.sa")*private$energy))
  )
}


###############################################################################
# Blood Energetics
###############################################################################

#' MBITES-Generic: Draw Bloodmeal Size for \code{\link{MosquitoFemale}}
#'
#' Draw a bloodmeal size from Beta(bm.a,bm.b)
#'  * This method is bound to \code{MosquitoFemale$rBloodMealSize()}.
#' @md
mbitesGeneric_rBloodMealSize <- function(){
  return(
    rbeta(n=1,private$FemalePopPointer$get_MBITES_PAR("bm.a"),private$FemalePopPointer$get_MBITES_PAR("bm.b"))
  )
}


###############################################################################
# Overfeed
###############################################################################

#' MBITES-Generic: Probaility of Overfeeding due to Bloodmeal Size for \code{\link{MosquitoFemale}}
#'
#' Probability of death due to overfeeding from bloodmeal size given by \deqn{\frac{e^{of.a\times bmSize}}{of.b+e^{of.a\times bmSize}}}
#'  * This method is bound to \code{MosquitoFemale$pOverFeed()}.
#' @md
mbitesGeneric_pOverFeed <- function(){
  return(
    exp(private$FemalePopPointer$get_MBITES_PAR("of.a")*private$bmSize)/(private$FemalePopPointer$get_MBITES_PAR("of.b") + exp(private$FemalePopPointer$get_MBITES_PAR("of.a")*private$bmSize))
  )
}


###############################################################################
# Refeed
###############################################################################

#' MBITES-Generic: Probaility to re-enter Blood Feeding Cycle from Incomplete Feeding for \code{\link{MosquitoFemale}}
#'
#' Probability to re-enter blood feeding cycle after incomplete blood feeding given by \deqn{ \frac{2+rf.b}{1+rf.b}-\frac{e^{rf.a\times bmSize}}{rf.b+e^{rf.a\times bmSize}} }
#'  * This method is bound to \code{MosquitoFemale$pReFeed()}.
#' @md
mbitesGeneric_pReFeed <- function(){
  return(
    (2+private$FemalePopPointer$get_MBITES_PAR("rf.b"))/(1+private$FemalePopPointer$get_MBITES_PAR("rf.b")) - exp(private$FemalePopPointer$get_MBITES_PAR("rf.a")*private$bmSize)/(private$FemalePopPointer$get_MBITES_PAR("rf.b") + exp(private$FemalePopPointer$get_MBITES_PAR("rf.a")*private$bmSize))
  )
}


###############################################################################
# Egg Batch
###############################################################################

#' MBITES-Generic: Draw Normally-distributed Egg Batch Size for \code{\link{MosquitoFemale}}
#'
#' Draw a egg batch size from Normal(bs.m,bs.v)
#'  * This method is bound to \code{MosquitoFemale$rBatchSize()}.
#' @md
mbitesGeneric_rBatchSizeNorm <- function(){
  return(
    ceiling(rnorm(1, private$FemalePopPointer$get_MBITES_PAR("bs.m"), private$FemalePopPointer$get_MBITES_PAR("bs.v")))
  )
}

#' MBITES-Generic: Egg Batch Size due to Bloodmeal Size for \code{\link{MosquitoFemale}}
#'
#' Give an egg batch size given by \deqn{ bmSize\times maxBatch }
#'  * This method is bound to \code{MosquitoFemale$rBatchSize()}.
#' @md
mbitesGeneric_rBatchSizeBms <- function(){
  return(
    ceiling(private$bmSize*private$FemalePopPointer$get_MBITES_PAR("maxBatch"))
  )
}

#' MBITES-Generic: Normally-distributed Egg Maturation Time for \code{\link{MosquitoFemale}}
#'
#' Draw an egg maturation time from Normal(emt.m,emt.v)
#'  * This method is bound to \code{MosquitoFemale$rEggMaturationTime()}.
#' @md
mbitesGeneric_rEggMaturationTimeNorm <- function(){
  return(
    max(0,rnorm(1, private$FemalePopPointer$get_MBITES_PAR("emt.m"), private$FemalePopPointer$get_MBITES_PAR("emt.v")))
  )
}

#' MBITES-Generic: No Egg Maturation Time for \code{\link{MosquitoFemale}}
#'
#' Instant egg maturation.
#'  * This method is bound to \code{MosquitoFemale$rEggMaturationTime()}.
#' @md
mbitesGeneric_rEggMaturationTimeOff <- function(){
  return(0)
}
