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
#' In MBITES, oogenesis (egg production) occurs during \code{\link{mbites_updateState}} if the mosquito
#' is bloodfed. Related to oogenesis there are two other primary behaviors that must be selected by the user,
#' namely, refeeding model choice and egg batch size model choice.
#'
#' @section Refeed Model Choice:
#'
#' Refeeding may occur if the mosquito is gravid yet requires more blood prior to oviposition. There are two models, plus a null model for turning off refeeding  behavior.
#'
#' @section Egg Batch Refeeding Model:
#'
#' In this model of refeeding (\code{\link{mbites_pReFeed_batch}}), refeeding probability is a function
#' of the egg batch size, as a proportion of the maximum allowable batch size. This model is not compatible
#' with the MBDETES approximation to MBITES.
#'
#' @section Blood Meal Refeeding Model:
#'
#' In this model of refeeding (\code{\link{mbites_pReFeed_bm}}), refeeding probability is a function
#' of the proportion full the mosquito is (the blood meal size). This model is compatible
#' with the MBDETES approximation to MBITES.
#'
#' @section Egg Batch Model Choice:
#'
#' There are two models of egg batch size.
#'
#' @section Gaussian-distributed Egg Batch Model:
#'
#' In this model of egg batch size (\code{\link{mbites_rBatchSizeNorm}}), the egg batch size is a
#' normally distributed random variable.
#'
#' @section Blood Meal Dependent Egg Batch Model:
#'
#' In this model of egg batch size (\code{\link{mbites_rBatchSizeBms}}), the egg batch size is a
#' function of the blood meal size.
#'
#' @section Oogenesis Model Choice:
#'
#' Oogenesis model should be selected in \code{\link{MBITES_Setup}} prior to creating any objects.
#' Oogenesis occurs during the post-prandial rest following a blood meal when the mosquito's state is updated, see \code{\link{mbites_updateState}}.
#' Both models require the user to select a refeeding model and a egg batch size model choice
#' (MBITES does not check if these selections are logically consistent, and care should be taken to choose appropriate combinations of models).
#'
#' @section Oogenesis Model 1 (Enter gravid state after egg maturation time has passed):
#'
#' In the first model (\code{\link{mbites_oogenesis1}}), the egg batch will be gravid after a period of maturation time
#' has passed for the clutch of eggs. This can be a normally distributed amount of time (see \code{\link{mbites_rEggMaturationTimeNorm}})
#' or occur instantaneously (see \code{\link{mbites_rEggMaturationTimeOff}}).
#'
#' @section Oogenesis Model 2 (Enter gravid state after egg provision is fulfilled):
#'
#' In the second model (\code{\link{mbites_oogenesis2}}), a batch of eggs (of some size) requires a certain blood provision
#' per egg in the batch. After this provision has been filled by blood feeding, the mosquito is gravid and will seek
#' an aquatic habitat to oviposit.
#'
#'  * This method is bound to \code{Mosquito_Female$Oogenesis}.
#' @name MBITES-Oogenesis
NULL
#> NULL


###############################################################################
# Egg Maturation (called from updateState in MBITES-Bout.R)
###############################################################################

#' MBITES: Check Egg Maturation
#'
#' This function is called during \code{\link{mbites_updateState}},
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
    private$state = "B"
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

#' MBITES: Probability of Refeeding as Function of Egg Batch Size
#'
#' Probability to re-enter blood feeding cycle after incomplete blood feeding given by \eqn{ \frac{2+rf_{b}}{1+rf_{b}}-\frac{e^{rf_{a}\times \frac{batch}{batch_{max}}}}{rf_{b}+e^{rf_{a}\times \frac{batch}{batch_{max}}}} }
#' This models mosquito propensity to take more blood if the egg batch is too small.
#'  * This method is bound to \code{MosquitoFemale$pReFeed()}
#'
mbites_pReFeed_batch <- function(){
  rf_a = MBITES:::Parameters$get_rf_a()
  rf_b = MBITES:::Parameters$get_rf_b()
  batchP = private$batch / MBITES:::Parameters$get_maxBatch()
  (2+rf_b)/(1+rf_b) - exp(rf_a*batchP)/(rf_b + exp(rf_a*batchP))
}

#' MBITES: Probability of Refeeding as Function of Blood Meal Size
#'
#' Probability to re-enter blood feeding cycle after incomplete blood feeding given by \eqn{ \frac{2+rf_{b}}{1+rf_{b}}-\frac{e^{rf_{a}\times bmSize}}{rf_{b}+e^{rf_{a}\times bmSize}} }
#' This models mosquito propensity to take more blood if the previous blood meal was too small.
#'  * This method is bound to \code{MosquitoFemale$pReFeed()}
#'
mbites_pReFeed_bm <- function(){
  rf_a = MBITES:::Parameters$get_rf_a()
  rf_b = MBITES:::Parameters$get_rf_b()
  (2+rf_b)/(1+rf_b) - exp(rf_a*private$bmSize)/(rf_b + exp(rf_a*private$bmSize))
}


###############################################################################
# Oogenesis Model 1 (set in the MBITES_Setup)
###############################################################################

#' @rdname MBITES-Oogenesis
mbites_oogenesis1 <- function(){

  # dont make a new batch if i am carrying around eggs
  if(private$batch <= 0){
    private$batch = self$rBatchSize()
    private$eggT = private$tNow + self$rEggMaturationTime()
  }
  private$bmSize = 0
  private$bloodfed = FALSE

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
    # dont need to set these flags; gets checked at end of every bout in mbites_updateState
    # private$gravid = TRUE # now mosquito is gravid
    # private$state = "O"
    private$bmSize = max(0,private$bmSize - private$eggP)
  }

  private$bloodfed = FALSE

}


###############################################################################
# Trivial Oogenesis model (for MBDETES approximation)
###############################################################################

mbites_oogenesisMBDETES <- function(){

  private$batch = self$rBatchSize() # new batch of eggs
  private$eggT = 0 # ready to go immediately
  private$bmSize = 0 # takes all of the blood to do this
  private$bloodfed = FALSE # now i'm unfed

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
