###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   M-BITES: Survival
#   MASH-MICRO Team
#   September 2017
#
###############################################################################

###############################################################################
# Flight Survival
###############################################################################

#' M-BITES: Get Baseline Survival Probability for \code{\link{MosquitoFemale}}
#'
#' Get baseline survival probability for \code{\link{mbitesGeneric_surviveFlight}}.
#'  * This method is bound to \code{MosquitoFemale$get_surviveFlightProb()}.
#'
mbites_get_surviveFlightProb <- function(){
  switch(private$state,
    F = {return(private$FemalePopPointer$get_MBITES_PAR("F_surv"))},
    B = {return(private$FemalePopPointer$get_MBITES_PAR("B_surv"))},
    R = {return(private$FemalePopPointer$get_MBITES_PAR("R_surv"))},
    L = {return(private$FemalePopPointer$get_MBITES_PAR("L_surv"))},
    O = {return(private$FemalePopPointer$get_MBITES_PAR("O_surv"))},
    M = {return(private$FemalePopPointer$get_MBITES_PAR("M_surv"))},
    S = {return(private$FemalePopPointer$get_MBITES_PAR("S_surv"))},
    {stop("illegal behavioral state for M-BITES")}
  )
}


###############################################################################
# Resting Survival
###############################################################################

#' M-BITES: Get Resting Hazards for \code{\link{MosquitoFemale}}
#'
#' Get resting hazards for \code{\link{mbitesGeneric_surviveResting}}.
#'  * This method is bound to \code{MosquitoFemale$get_restHaz()}.
#'
mbites_get_restHaz <- function(){
  switch(private$pSetNow,
    f = {return(private$LandscapePointer$get_FeedingSites(private$locNow)$get_hazLspot(private$lspot))},
    l = {return(private$LandscapePointer$get_AquaSites(private$locNow)$get_haz())},
    m = {return(private$LandscapePointer$get_MatingSites(private$locNow)$get_haz())},
    s = {return(private$LandscapePointer$get_SugarSites(private$locNow)$get_haz())},
    {stop("illegal point set for M-BITES")}
  )
}
