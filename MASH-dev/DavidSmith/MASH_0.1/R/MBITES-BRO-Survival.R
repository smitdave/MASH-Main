#################################################################
#
#   MASH
#   R6-ified
#   MBITES-BRO (Blood Feeding, Resting, Oviposition)
#   Senescence, flight and resting survival
#   David Smith, Hector Sanchez, Sean Wu
#   July 28, 2017
#
#################################################################

#################################################################
# Flight Survival
#################################################################

#' MBITES-BRO: Get Baseline Survival Probability for \code{\link{MicroMosquitoFemale}}
#'
#' Get baseline survival probability for \code{\link{mbitesGeneric_surviveFlight}}.
#'  * This method is bound to \code{MicroMosquitoFemale$get_surviveFlightProb()}.
#' @md
mbitesBRO_get_surviveFlightProb <- function(){
  switch(private$state,
    B = {return(private$FemalePopPointer$get_MBITES_PAR("B_surv"))},
    R = {return(private$FemalePopPointer$get_MBITES_PAR("R_surv"))},
    O = {return(private$FemalePopPointer$get_MBITES_PAR("O_surv"))},
    {stop("illegal behavioral state for MBITES-BRO")}
  )
}


#################################################################
# Resting Survival
#################################################################

#' MBITES-BRO: Get Resting Hazards for \code{\link{MicroMosquitoFemale}}
#'
#' Get resting hazards for \code{\link{mbitesGeneric_surviveResting}}.
#'  * This method is bound to \code{MicroMosquitoFemale$get_restHaz()}.
#' @md
mbitesBRO_get_restHaz <- function(){
  switch(private$inPointSet,
    f = {return(private$LandscapePointer$get_FeedingSites(private$ix)$get_hazLspot(private$lspot))},
    l = {return(private$LandscapePointer$get_AquaSites(private$ix)$get_haz())},
    {stop("illegal point set for MBITES-BRO")}
  )
}
