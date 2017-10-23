###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES-BRO: Survival
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################

#################################################################
# Flight Survival
#################################################################

#' MBITES-BRO: Get Baseline Survival Probability for \code{\link{MosquitoFemale}}
#'
#' Get baseline survival probability for \code{\link{mbitesGeneric_surviveFlight}}.
#'  * This method is bound to \code{MosquitoFemale$get_surviveFlightProb()}.
#' @md
mbitesBRO_get_surviveFlightProb <- function(){
  switch(private$state,
    B = {return(private$FemalePopPointer$get_MBITES_PAR("B_surv"))},
    R = {return(private$FemalePopPointer$get_MBITES_PAR("R_surv"))},
    O = {return(private$FemalePopPointer$get_MBITES_PAR("O_surv"))},
    M = {return(private$FemalePopPointer$get_MBITES_PAR("M_surv"))},
    S = {return(private$FemalePopPointer$get_MBITES_PAR("S_surv"))},
    {stop("illegal behavioral state for MBITES-BRO")}
  )
}


#################################################################
# Resting Survival
#################################################################

#' MBITES-BRO: Get Resting Hazards for \code{\link{MosquitoFemale}}
#'
#' Get resting hazards for \code{\link{mbitesGeneric_surviveResting}}.
#'  * This method is bound to \code{MosquitoFemale$get_restHaz()}.
#' @md
mbitesBRO_get_restHaz <- function(){
  switch(private$pSetNow,
    f = {return(private$LandscapePointer$get_FeedingSites(private$locNow)$get_hazLspot(private$lspot))},
    l = {return(private$LandscapePointer$get_AquaSites(private$locNow)$get_haz())},
    m = {return(private$LandscapePointer$get_MatingSites(private$locNow)$get_haz())},
    s = {return(private$LandscapePointer$get_SugarSites(private$locNow)$get_haz())},
    {stop("illegal point set for MBITES-BRO")}
  )
}
