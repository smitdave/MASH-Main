###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Blood Meal
#     MBITES Team
#     March 2018
#
###############################################################################

###############################################################################
# Bloodmeal (note: refeeding is now in MBITES-Oogenesis)
###############################################################################

#' M-BITES: Bloodmeal Energetics for \code{\link{MosquitoFemale}}
#'
#' Handle bloodmeal size (see \code{\link{mbites_rBloodMealSize}}), overfeeding (see \code{\link{mbites_pOverFeed}}),
#' and egg batch size and maturation time. Note that refeeding behavior is calculated during resting bout.
#'  * This method is bound to \code{MosquitoFemale$BloodMeal()}.
#' @md
mbites_BloodMeal <- function(){
  private$bloodfed = TRUE
  private$bmSize = self$rBloodMealSize()

  self$Overfeeding()
  self$BloodEnergetics()

  # egg production
  self$oogenesis() # MBITES-Oogenesis
}


#' MBITES-Generic: Draw Bloodmeal Size for \code{\link{MosquitoFemale}}
#'
#' Draw a bloodmeal size from Beta(bm.a,bm.b)
#'  * This method is bound to \code{MosquitoFemale$rBloodMealSize()}.
#' @md
mbites_rBloodMealSize <- function(){
  rbeta(n=1,MBITES:::Parameters$get_bm_a(),MBITES::Parameters$get_bm_b())
}


###############################################################################
# Overfeeding
###############################################################################

mbites_Overfeeding <- function(){
  # Overfeeding mortality
  if(MBITES:::Parameters$get_OVERFEED()){
    if(runif(1) < self$pOverFeed()){
      private$state = 'D'
    }
  }
}

#' MBITES-Generic: Probaility of Overfeeding due to Bloodmeal Size for \code{\link{MosquitoFemale}}
#'
#' Probability of death due to overfeeding from bloodmeal size given by \deqn{\frac{e^{of.a\times bmSize}}{of.b+e^{of.a\times bmSize}}}
#'  * This method is bound to \code{MosquitoFemale$pOverFeed()}.
#' @md
mbites_pOverFeed <- function(){
  of_a = MBITES:::Parameters$get_of_a()
  of_b = MBITES:::Parameters$get_of_b()
  exp(of_a*private$bmSize)/(of_b + exp(of_a*private$bmSize))
}
