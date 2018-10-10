###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   M-BITES: Oogenesis
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################

#' MBITES-oogenesis: Generic oogenesis call for \code{\link{MosquitoFemale}}
#'
#' Two models are currently working.
#'
#' In the first model, egg batch size is proportional to blood
#' meal size, the egg batch incubation period is equal to the
#' post-prandial resting period, but the mosquito can refeed
#' with some probability (depending on egg batch size) with some
#' probability.
#'
#' In the second model, a batch of eggs (of some size) commits to
#' development at the first bloodmeal after hatching or laying.
#' The total blood required for maturation is proportional to the
#' egg batch size. At the end of the post-prandial period after
#' completing the blood requirement, the eggs are mature and the
#' mosquito is gravid.
#'
#'  * This method is bound to \code{MosquitoFemale$rBatchSize()}.
#' @md
mbites_oogenesis <- function(){
   oogModel <- private$FemalePopPointer$get_MBITES_PAR("oogModel"))
   switch(oogModel,
     '1' = {self$oogenesis1()},
     '2' = {self$oogenesis2()},
     {stop(cat("non-existent oogenesis model: ",oogModel,"\n",sep=""))}
   )
}

###############################################################################
# Oogenesis Model #1
###############################################################################

mbites_oogenesis1 <- function(){
  private$batch = self$rBatchSize()
  private$eggT = self$rEggMaturationTime()
  #NOTE: the post prandial resting period is in MBITES-Timing.R
  #NOTE: self$checkRefeed() is called in MBITES-Bouts.R :: updateState()
}

mbites_checkRefeed <- function(){
  if (private$FemalePopPointer$get_MBITES_PAR("oogModel"))=='1')
    if(private$FemalePopPointer$get_MBITES_PAR("REFEED"))
      gravid = ifelse(runif(1) < self$pReFeed(),FALSE, TRUE)
}

###############################################################################
# Refeed
###############################################################################

#' MBITES-Generic: Probaility to re-enter Blood Feeding Cycle from Incomplete Feeding for \code{\link{MosquitoFemale}}
#'
#' Probability to re-enter blood feeding cycle after incomplete blood feeding given by \deqn{ \frac{2+rf.b}{1+rf.b}-\frac{e^{rf.a\times bmSize}}{rf.b+e^{rf.a\times bmSize}} }
#'  * This method is bound to \code{MosquitoFemale$pReFeed()}.
#' @md
mbites_pReFeed <- function(){
  return( (2+private$FemalePopPointer$get_MBITES_PAR("rf.b"))/(1+private$FemalePopPointer$get_MBITES_PAR("rf.b")) - exp(private$FemalePopPointer$get_MBITES_PAR("rf.a")*private$batch)/(private$FemalePopPointer$get_MBITES_PAR("rf.b")
+ exp(private$FemalePopPointer$get_MBITES_PAR("rf.a")*private$batch))
  )
}


###############################################################################
# Oogenesis Model #2
###############################################################################
mbites_oogenesis2 <- function(){
  if(private$batch == 0){
    private$batch = self$rBatchSize()
    private$eggP = private$FemalePopPointer$get_MBITES_PAR("bloodPerEgg")*private$batch
  }
  private$eggP = private$eggP - private$bmSize
  if(private$eggP < 0) private$gravid = TRUE
}


#' MBITES-Generic: Draw Normally-distributed Egg Batch Size for \code{\link{MosquitoFemale}}
#'
#' Draw a egg batch size from Normal(bs.m,bs.v)
#'  * This method is bound to \code{MosquitoFemale$rBatchSize()}.
#' @md
mbites_rBatchSizeNorm <- function(){
  return(
    ceiling(rnorm(1, private$FemalePopPointer$get_MBITES_PAR("bs.m"), private$FemalePopPointer$get_MBITES_PAR("bs.v")))
  )
}

#' MBITES-Generic: Egg Batch Size due to Bloodmeal Size for \code{\link{MosquitoFemale}}
#'
#' Give an egg batch size given by \deqn{ bmSize\times maxBatch }
#'  * This method is bound to \code{MosquitoFemale$rBatchSize()}.
#' @md
mbites_rBatchSizeBms <- function(){
  return(
    ceiling(private$bmSize*private$FemalePopPointer$get_MBITES_PAR("maxBatch"))
  )
}

#' MBITES-Generic: Normally-distributed Egg Maturation Time for \code{\link{MosquitoFemale}}
#'
#' Draw an egg maturation time from Normal(emt.m,emt.v)
#'  * This method is bound to \code{MosquitoFemale$rEggMaturationTime()}.
#' @md
mbites_rEggMaturationTimeNorm <- function(){
  return(
    max(0,rnorm(1, private$FemalePopPointer$get_MBITES_PAR("emt.m"), private$FemalePopPointer$get_MBITES_PAR("emt.v")))
  )
}

#' MBITES-Generic: No Egg Maturation Time for \code{\link{MosquitoFemale}}
#'
#' Instant egg maturation.
#'  * This method is bound to \code{MosquitoFemale$rEggMaturationTime()}.
#' @md
mbites_rEggMaturationTimeOff <- function(){
  return(0)
}
