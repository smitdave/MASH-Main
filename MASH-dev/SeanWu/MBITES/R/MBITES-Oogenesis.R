###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Oogenesis
#     MBITES Team
#     March 2018
#
###############################################################################

#' MBITES: Oogenesis
#'
#' Oogenesis model should be selected in \code{\link{MBITES_Setup}} prior to creating any objects.
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
#'  * This method is bound to \code{Mosquito_Female$oogenesis}.
#' @name oogenesis
NULL
#> NULL


###############################################################################
# Oogenesis Model 1
###############################################################################

#' @rdname oogenesis
mbites_oogenesis1 <- function(){
  private$batch = self$rBatchSize()
  private$eggT = self$rEggMaturationTime()
  #NOTE: the post prandial resting period is in MBITES-Timing.R
  #NOTE: self$checkRefeed() is called in MBITES-Bouts.R :: updateState()
}

#' MBITES: Normally-distributed Egg Maturation Time
#'
#' Draw an egg maturation time from Normal(emt_m,emt_sd)
#'  * This method is bound to \code{Mosquito_Female$rEggMaturationTime}.
#'
mbites_rEggMaturationTimeNorm <- function(){
  max(0,rnorm(1, MBITES:::Parameters$get_emt_m(), MBITES:::Parameters$get_emt_sd()))
}

#' MBITES: No Egg Maturation Time
#'
#' Instant egg maturation.
#'  * This method is bound to \code{Mosquito_Female$rEggMaturationTime}.
#'
mbites_rEggMaturationTimeOff <- function(){
  0
}


###############################################################################
# Oogenesis Model 2
###############################################################################

#' @rdname oogenesis
mbites_oogenesis2 <- function(){
  if(private$batch <= 0){
    private$batch = self$rBatchSize()
    private$eggP = MBITES:::Parameters$get_bloodPerEgg()*private$batch
  }
  # egg provision: after we've fed enough then mosquito is gravid
  private$eggP = private$eggP - private$bmSize
  if(private$eggP < 0){
    private$gravid = TRUE
  }
}

#' MBITES: Draw Normally-distributed Egg Batch Size
#'
#' Draw a egg batch size from Normal(bs_m,bs_sd)
#'  * This method is bound to \code{MosquitoFemale$rBatchSize}.
#'
mbites_rBatchSizeNorm <- function(){
  ceiling(rnorm(1,MBITES:::Parameters$get_bs_m(),MBITES:::Parameters$get_bs_sd()))
}

#' MBITES: Bloodmeal dependent Egg Batch Size
#'
#' Give an egg batch size given by \eqn{ bmSize\times maxBatch }
#'  * This method is bound to \code{MosquitoFemale$rBatchSize}.
#'
mbites_rBatchSizeBms <- function(){
  ceiling(private$bmSize*MBITES:::Parameters$get_maxBatch())
}


###############################################################################
# Refeed
###############################################################################

mbites_checkRefeed <- function(){
  # check refeed
  if(runif(1) < self$pReFeed()){
    private$gravid = FALSE
    private$state = "B" # check with DS
  } else {
    private$gravid = TRUE
    private$state = "O"
  }
}

mbites_checkRefeed_null <- function(){
  # null function
}


#' MBITES-Generic: Probaility to re-enter Blood Feeding Cycle from Incomplete Feeding for \code{\link{MosquitoFemale}}
#'
#' Probability to re-enter blood feeding cycle after incomplete blood feeding given by \deqn{ \frac{2+rf.b}{1+rf.b}-\frac{e^{rf.a\times bmSize}}{rf.b+e^{rf.a\times bmSize}} }
#'  * This method is bound to \code{MosquitoFemale$pReFeed()}.
#'
mbites_pReFeed <- function(){
  rf_a = MBITES:::Parameters$get_rf_a()
  rf_b = MBITES:::Parameters$get_rf_b()
  (2+rf_b)/(1+rf_b) - exp(rf_a*private$batch)/(rf_b + exp(rf_a*private$batch))
}
