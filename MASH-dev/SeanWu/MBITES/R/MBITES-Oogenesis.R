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
#' Oogenesis model should be selected in \code{\link{MBITES_Setup}} prior to creating any objects, note
#' that if using the first model of oogenesis, then \code{\link{mbites_checkRefeed}} should be used
#' as the refeeding model, if using the second model of oogenesis, refeeding behavior
#' should be disabled by selecting \code{\link{mbites_checkRefeed_null}} as the refeed function.
#' Oogenesis occurs during the blood meal, see \code{\link{BloodMeal}}.
#'
#' In the first model (\code{\link{mbites_oogenesis1}}), egg batch size is proportional to blood
#' meal size, the egg batch incubation period is equal to the
#' post-prandial resting period, but the mosquito can refeed
#' with some probability (depending on egg batch size) with some
#' probability.
#'
#' In the second model (\code{\link{mbites_oogenesis2}}), a batch of eggs (of some size) commits to
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
# Oogenesis Model 1 (set in the MBITES_Setup)
###############################################################################

#' @rdname oogenesis
mbites_oogenesis1 <- function(){
  private$batch = self$rBatchSize()
  private$eggT = self$rEggMaturationTime()
  #NOTE: the post prandial resting period is in MBITES-Timing.R
  #NOTE: self$checkRefeed() is called in MBITES-Bouts.R :: updateState()
  # private$bmSize = 0 # check with DS IN mbites_checkPostPrandial
}

#' MBITES: Oogenesis Model 1 Blood Meal Size Reset
#'
#' In the first model of oogenesis blood meal size is reset to 0 during the
#' post-prandial rest (\code{\link{mbites_checkPostPrandial}}).
#'  * This method is bound to \code{Mosquito_Female$PPRbmSize}
mbites_PPRbmSize1 <- function(){
  private$bmSize = 0
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
# Oogenesis Model 2 (set in MBITES_Setup)
###############################################################################

#' @rdname oogenesis
mbites_oogenesis2 <- function(){
  if(private$batch <= 0){
    private$batch = self$rBatchSize()
    private$eggP = MBITES:::Parameters$get_bloodPerEgg()*private$batch
  }
  # egg provision: after we've fed enough then mosquito is gravid
  private$eggP = private$eggP - private$bmSize
  # private$bmSize = max(0,private$bmSize - private$eggP) # check with DS IN mbites_checkPostPrandial
  if(private$eggP < 0){
    private$gravid = TRUE
  }
}

#' MBITES: Oogenesis Model 2 Blood Meal Size Reset
#'
#' In the second model of oogenesis blood meal size is reduced by an amount equal to
#' the amount of blood used in the egg provision, or to 0 if that would induce a negative amount.
#' This occurs during the post-prandial rest (\code{\link{mbites_checkPostPrandial}}).
#'  * This method is bound to \code{Mosquito_Female$PPRbmSize}
mbites_PPRbmSize2 <- function(){
  private$bmSize = max(0,private$bmSize - private$eggP)
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
# Refeed (set in MBITES_Setup)
###############################################################################

#' MBITES: Check Refeeding Behavior
#'
#' During \code{\link{mbites_updateState}}, if the mosquito is gravid, check for refeeding
#' behavior as a function of the batch size. This function should *only* be used with the
#' first model of oogenesis.
#'  * This method is bound to \code{Mosquito_Female$checkRefeed}
#'
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

#' MBITES: Null Refeeding Behavior
#'
#' If using the second model of oogenesis, refeeding should be disabled by using this stand-in function.
#'  * This method is bound to \code{Mosquito_Female$checkRefeed}
#'
mbites_checkRefeed_null <- function(){
  # null function
}

#' MBITES: Probability of Refeeding
#'
#' Probability to re-enter blood feeding cycle after incomplete blood feeding given by \eqn{ \frac{2+rf.b}{1+rf.b}-\frac{e^{rf.a\times batch}}{rf.b+e^{rf.a\times batch}} }
#'  * This method is bound to \code{MosquitoFemale$pReFeed()}.
#'
mbites_pReFeed <- function(){
  rf_a = MBITES:::Parameters$get_rf_a()
  rf_b = MBITES:::Parameters$get_rf_b()
  (2+rf_b)/(1+rf_b) - exp(rf_a*private$batch)/(rf_b + exp(rf_a*private$batch))
}
