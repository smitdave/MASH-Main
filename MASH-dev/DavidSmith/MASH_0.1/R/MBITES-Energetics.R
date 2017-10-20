#################################################################
#
#   MASH
#   MBITES
#   Sugar feeding, blood feeding, overfeed, refeed, egg batch
#   David Smith, Hector Sanchez, Sean Wu
#   May 8, 2017
#
#################################################################

#################################################################
# Sugar Energetics
#################################################################

#' MBITES-Generic: Probability of Death due to Energy Reserves for \code{\link{MicroMosquitoFemale}}
#'
#' Incremental mortality as a function of energy reserves given by \deqn{ \frac{e^{S.a\times energy}}{S.b+e^{S.a\times energy}} }
#'  * This method is bound to \code{MicroMosquitoFemale$pEnergySurvival()}.
#' @md
mbitesGeneric_pEnergySurvival <- function(){
  return(
    exp(private$FemalePopPointer$get_MBITES_PAR("S.a")*private$energy)/(private$FemalePopPointer$get_MBITES_PAR("S.b") + exp(private$FemalePopPointer$get_MBITES_PAR("S.a")*private$energy))
  )
}

#' MBITES-Generic: Probability to Queue Sugar Bout due to Energy Reserves for \code{\link{MicroMosquitoFemale}}
#'
#' Probability to queue sugar bout as function of energy reserves given by \deqn{ \frac{2+S.sb}{1+S.sb}-\frac{e^{S.sa\times energy}}{S.sb+e^{S.sa\times energy}} }
#'  * This method is bound to \code{MicroMosquitoFemale$pSugarBout()}.
#' @md
mbitesGeneric_pSugarBout <- function(){
  return(
    (2+private$FemalePopPointer$get_MBITES_PAR("S.sb"))/(1+private$FemalePopPointer$get_MBITES_PAR("S.sb"))-exp(private$FemalePopPointer$get_MBITES_PAR("S.sa")*private$energy)/(private$FemalePopPointer$get_MBITES_PAR("S.sb")+exp(private$FemalePopPointer$get_MBITES_PAR("S.sa")*private$energy))
  )
}


#################################################################
# Blood Energetics
#################################################################

#' MBITES-Generic: Draw Bloodmeal Size for \code{\link{MicroMosquitoFemale}}
#'
#' Draw a bloodmeal size from Beta(bm.a,bm.b)
#'  * This method is bound to \code{MicroMosquitoFemale$rBloodMealSize()}.
#' @md
mbitesGeneric_rBloodMealSize <- function(){
  return(
    rbeta(n=1,private$FemalePopPointer$get_MBITES_PAR("bm.a"),private$FemalePopPointer$get_MBITES_PAR("bm.b"))
  )
}


#################################################################
# Overfeed
#################################################################

#' MBITES-Generic: Probaility of Overfeeding due to Bloodmeal Size for \code{\link{MicroMosquitoFemale}}
#'
#' Probability of death due to overfeeding from bloodmeal size given by \deqn{\frac{e^{of.a\times bmSize}}{of.b+e^{of.a\times bmSize}}}
#'  * This method is bound to \code{MicroMosquitoFemale$pOverFeed()}.
#' @md
mbitesGeneric_pOverFeed <- function(){
  return(
    exp(private$FemalePopPointer$get_MBITES_PAR("of.a")*private$bmSize)/(private$FemalePopPointer$get_MBITES_PAR("of.b") + exp(private$FemalePopPointer$get_MBITES_PAR("of.a")*private$bmSize))
  )
}


#################################################################
# Refeed
#################################################################

#' MBITES-Generic: Probaility to re-enter Blood Feeding Cycle from Incomplete Feeding for \code{\link{MicroMosquitoFemale}}
#'
#' Probability to re-enter blood feeding cycle after incomplete blood feeding given by \deqn{ \frac{2+rf.b}{1+rf.b}-\frac{e^{rf.a\times bmSize}}{rf.b+e^{rf.a\times bmSize}} }
#'  * This method is bound to \code{MicroMosquitoFemale$pReFeed()}.
#' @md
mbitesGeneric_pReFeed <- function(){
  return(
    (2+private$FemalePopPointer$get_MBITES_PAR("rf.b"))/(1+private$FemalePopPointer$get_MBITES_PAR("rf.b")) - exp(private$FemalePopPointer$get_MBITES_PAR("rf.a")*private$bmSize)/(private$FemalePopPointer$get_MBITES_PAR("rf.b") + exp(private$FemalePopPointer$get_MBITES_PAR("rf.a")*private$bmSize))
  )
}


#################################################################
# Egg Batch
#################################################################

#' MBITES-Generic: Draw Normally-distributed Egg Batch Size for \code{\link{MicroMosquitoFemale}}
#'
#' Draw a egg batch size from Normal(bs.m,bs.v)
#'  * This method is bound to \code{MicroMosquitoFemale$rBatchSize()}.
#' @md
mbitesGeneric_rBatchSizeNorm <- function(){
  return(
    ceiling(rnorm(1, private$FemalePopPointer$get_MBITES_PAR("bs.m"), private$FemalePopPointer$get_MBITES_PAR("bs.v")))
  )
}

#' MBITES-Generic: Egg Batch Size due to Bloodmeal Size for \code{\link{MicroMosquitoFemale}}
#'
#' Give an egg batch size given by \deqn{ bmSize\times maxBatch }
#'  * This method is bound to \code{MicroMosquitoFemale$rBatchSize()}.
#' @md
mbitesGeneric_rBatchSizeBms <- function(){
  return(
    ceiling(private$bmSize*private$FemalePopPointer$get_MBITES_PAR("maxBatch"))
  )
}

#' MBITES-Generic: Normally-distributed Egg Maturation Time for \code{\link{MicroMosquitoFemale}}
#'
#' Draw an egg maturation time from Normal(emt.m,emt.v)
#'  * This method is bound to \code{MicroMosquitoFemale$rEggMaturationTime()}.
#' @md
mbitesGeneric_rEggMaturationTimeNorm <- function(){
  return(
    max(0,rnorm(1, private$FemalePopPointer$get_MBITES_PAR("emt.m"), private$FemalePopPointer$get_MBITES_PAR("emt.v")))
  )
}

#' MBITES-Generic: No Egg Maturation Time for \code{\link{MicroMosquitoFemale}}
#'
#' Instant egg maturation.
#'  * This method is bound to \code{MicroMosquitoFemale$rEggMaturationTime()}.
#' @md
mbitesGeneric_rEggMaturationTimeOff <- function(){
  return(0)
}

# #' MBITES-Generic: Make an Egg Batch and Oviposit for \code{\link{MicroMosquitoFemale}}
# #'
# #' Make an egg batch and deposit on the landscape.
# #'  * This method is bound to \code{MicroMosquitoFemale$makeBatches()}.
# #' @md
# mbitesGeneric_makeBatches <- function(){
#   addBatch2Q(M$batch, M$ix, M$tNow, M$id, M$sire) # aquaticEcology.R
# }
