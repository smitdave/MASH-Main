###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES: Cohort Setup
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################


##############################################################
# Setup Function
##############################################################

#' Initialize Cohort Methods for M-BITES
#'
#' This function initializes generic methods for M-BITES models; please note that the
#' switches for this function modify only the methods that are added to the \code{\link{MosquitoFemale}}
#' and \code{\link{MosquitoMale}} classes.
#'
#' @param batchSize character switch that should be one of \code{"bms","norm"} for egg batch sizes dependent on bloodmeal size or normally distributed
#' @param eggMatT character switch that should be one of \code{"off","norm"} for egg batch maturation time turned off or normally distributed
#'
#'
#'
#' @return modifies the \code{\link{MosquitoFemale}} and \code{\link{MosquitoMale}} classes.
#' @export
MBITES.Cohort.Setup <- function(overwrite = TRUE){

  # alert user
  message("initializing M-BITES Cohort shared methods")

  ##############################################################
  # MBITES-ChooseHost.R
  ##############################################################

  MosquitoFemale$set(which = "public",name = "chooseHost",
            value = mbitesCohort_chooseHost,
            overwrite = overwrite
  )

}
