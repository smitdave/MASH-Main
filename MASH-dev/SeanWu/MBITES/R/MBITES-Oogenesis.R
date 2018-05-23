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
#' @section Oogenesis Model Choice:
#'
#' Oogenesis model should be selected in \code{\link{MBITES_Setup}} prior to creating any objects, note
#' that if using the first model of oogenesis, then \code{\link{mbites_checkRefeed}} should be used
#' as the refeeding model, if using the second model of oogenesis, refeeding behavior
#' should be disabled by selecting \code{\link{mbites_checkRefeed_null}} as the refeed function.
#' Oogenesis occurs during the blood meal, see \code{\link{MBITES-BloodMeal}}.
#'
#' @section Oogenesis Model 1 (Enter gravid state after egg maturation time has passed):
#'
#' In the first model (\code{\link{mbites_oogenesis1}}), egg batch size is proportional to blood
#' meal size, the egg batch incubation period is equal to the
#' post-prandial resting period, but the mosquito can refeed
#' with some probability (depending on egg batch size) with some
#' probability.
#'
#' @section Oogenesis Model 2 (Enter gravid state after egg provision is fulfilled):
#'
#' In the second model (\code{\link{mbites_oogenesis2}}), a batch of eggs (of some size) commits to
#' development at the first bloodmeal after hatching or laying.
#' The total blood required for maturation is proportional to the
#' egg batch size. At the end of the post-prandial period after
#' completing the blood requirement, the eggs are mature and the
#' mosquito is gravid.
#'
#'  * This method is bound to \code{Mosquito_Female$oogenesis}.
#' @name MBITES-Oogenesis
NULL
#> NULL


###############################################################################
# Egg Maturation (called from updateState in MBITES-Bout.R)
###############################################################################

#' MBITES: Check Egg Maturation
#'
#' This function is called during \code{\link{mbites_checkRefeed}},
#' it checks that the mosquito has passed the egg maturation time and only sets \code{gravid = TRUE}
#' if this condition is filled. If the eggs are not mature, go on another blood search.
#'  * This method is bound to \code{Mosquito_Female$checkEggMaturation}
#'
mbites_checkEggMaturation <- function(){
  # check egg maturation
  if(private$eggT <= private$tNow){
    private$gravid = TRUE
    private$state = "O"
  } else {
    private$gravid = FALSE
    private$state = "B"
  }
}

# set methods
Mosquito_Female$set(which = "public",name = "checkEggMaturation",
          value = mbites_checkEggMaturation, overwrite = TRUE
)


###############################################################################
# Refeed (called from updateState in MBITES-Bout.R)
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
  }
}

#' MBITES: Null Refeeding Behavior
#'
#' If using the second model of oogenesis, refeeding should be disabled by using this stand-in function.
#'  * This method is bound to \code{Mosquito_Female$checkRefeed}
#'
mbites_checkRefeed_null <- function(){
  # null function
  # private$gravid = TRUE (already set in mbites_checkEggMaturation)
  # private$state = "O" (already set in mbites_checkEggMaturation)
}

#' MBITES: Probability of Refeeding
#'
#' Probability to re-enter blood feeding cycle after incomplete blood feeding given by \eqn{ \frac{2+rf_{b}}{1+rf_{b}}-\frac{e^{rf_{a}\times \frac{batch}{batch_{max}}}}{rf_{b}+e^{rf_{a}\times \frac{batch}{batch_{max}}}} }
#' This models mosquito propensity to take more blood if the egg batch is too small.
#'  * This method is bound to \code{MosquitoFemale$pReFeed()}.
#'
mbites_pReFeed <- function(){
  rf_a = MBITES:::Parameters$get_rf_a()
  rf_b = MBITES:::Parameters$get_rf_b()
  batchP = private$batch / MBITES:::Parameters$get_maxBatch()
  (2+rf_b)/(1+rf_b) - exp(rf_a*batchP)/(rf_b + exp(rf_a*batchP))
}

#' MBITES: Null Probability of Refeeding
#'
#' Null probability of refeeding (turn the behavior off)
#'  * This method is bound to \code{MosquitoFemale$pReFeed()}.
#'
mbites_pReFeed_null <- function(){
  0
}


###############################################################################
# Oogenesis Model 1 (set in the MBITES_Setup)
###############################################################################

#' @rdname MBITES-Oogenesis
mbites_oogenesis1 <- function(){
  if(private$alive){

    # dont make a new batch if i am carrying around eggs
    if(private$batch <= 0){
      private$batch = self$rBatchSize()
      private$eggT = private$tNow + self$rEggMaturationTime()
    }
    private$bmSize = 0

  }
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

#' @rdname MBITES-Oogenesis
mbites_oogenesis2 <- function(){
  if(private$alive){

    # dont make a new batch if i am carrying around eggs
    if(private$batch <= 0){
      private$batch = self$rBatchSize()
      private$eggP = MBITES:::Parameters$get_bloodPerEgg()*private$batch
    }
    # egg provision: after we've fed enough then mosquito is gravid
    private$eggP = private$eggP - private$bmSize
    # if the egg provision is fulfilled we can go ahead and get ready for oviposition
    if(private$eggP <= 0){
      private$eggT = 0
      private$gravid = TRUE # now mosquito is gravid
      private$state = "O"
      private$bmSize = max(0,private$bmSize - private$eggP)
    }

  }
}

###############################################################################
# Egg Batch Size
###############################################################################

#' MBITES: Draw Gaussian-distributed Egg Batch Size
#'
#' Draw a egg batch size from Normal(bs_m,bs_sd)
#'  * This method is bound to \code{MosquitoFemale$rBatchSize}.
#'
mbites_rBatchSizeNorm <- function(){
  min(ceiling(rnorm(1,MBITES:::Parameters$get_bs_m(),MBITES:::Parameters$get_bs_sd())),MBITES:::Parameters$get_maxBatch())
}

#' MBITES: Bloodmeal dependent Egg Batch Size
#'
#' Give an egg batch size given by \eqn{ bmSize\times maxBatch }
#'  * This method is bound to \code{MosquitoFemale$rBatchSize}.
#'
mbites_rBatchSizeBms <- function(){
  ceiling(private$bmSize*MBITES:::Parameters$get_maxBatch())
}
