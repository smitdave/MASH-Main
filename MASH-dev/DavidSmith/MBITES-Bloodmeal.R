###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   M-BITES: The Blood Meal  
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################

#################################################################
# The Blood Meal  
#################################################################

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
  self$oogenesis() 
}

mbites_Overfeeding <- function(){
  # Overfeeding mortality
  if(private$FemalePopPointer$get_MBITES_PAR("OVERFEED")){
    if(runif(1) < self$pOverFeed()){
      private$state = 'D'
    }
  }
} 

###############################################################################
# Blood Meal 
###############################################################################

#' MBITES-Generic: Draw Bloodmeal Size for \code{\link{MosquitoFemale}}
#'
#' Draw a bloodmeal size from Beta(bm.a,bm.b)
#'  * This method is bound to \code{MosquitoFemale$rBloodMealSize()}.
#' @md
mbites_rBloodMealSize <- function(){
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
mbites_pOverFeed <- function(){
  return(
    exp(private$FemalePopPointer$get_MBITES_PAR("of.a")*private$bmSize)/(private$FemalePopPointer$get_MBITES_PAR("of.b") + exp(private$FemalePopPointer$get_MBITES_PAR("of.a")*private$bmSize))
  )
}

### NOTE: Moved to MBITES-Oogenesis.R 
#################################################################################
### Refeed
#################################################################################
##
###' MBITES-Generic: Probaility to re-enter Blood Feeding Cycle from Incomplete Feeding for \code{\link{MosquitoFemale}}
###'
###' Probability to re-enter blood feeding cycle after incomplete blood feeding given by \deqn{ \frac{2+rf.b}{1+rf.b}-\frac{e^{rf.a\times bmSize}}{rf.b+e^{rf.a\times bmSize}} }
###'  * This method is bound to \code{MosquitoFemale$pReFeed()}.
###' @md
##mbites_pReFeed <- function(){
##  return(
##    (2+private$FemalePopPointer$get_MBITES_PAR("rf.b"))/(1+private$FemalePopPointer$get_MBITES_PAR("rf.b")) - exp(private$FemalePopPointer$get_MBITES_PAR("rf.a")*private$bmSize)/(private$FemalePopPointer$get_MBITES_PAR("rf.b") + exp(private$FemalePopPointer$get_MBITES_PAR("rf.a")*private$bmSize))
##  )
##}
##

