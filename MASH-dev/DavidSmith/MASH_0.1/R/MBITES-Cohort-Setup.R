#################################################################
#
#   MASH
#   R6-ified
#   M-BITES Cohort Methods
#   Setup Module
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   August 9, 2017
#
#################################################################


##############################################################
# Setup Function
##############################################################

#' Initialize Cohort Methods for M-BITES
#'
#' This function initializes generic methods for M-BITES models; please note that the
#' switches for this function modify only the methods that are added to the \code{\link{MicroMosquitoFemale}}
#' and \code{\link{MicroMosquitoMale}} classes.
#'
#' @param batchSize character switch that should be one of \code{"bms","norm"} for egg batch sizes dependent on bloodmeal size or normally distributed
#' @param eggMatT character switch that should be one of \code{"off","norm"} for egg batch maturation time turned off or normally distributed
#'
#'
#'
#' @return modifies the \code{\link{MicroMosquitoFemale}} and \code{\link{MicroMosquitoMale}} classes.
#' @export
MBITES.Cohort.Setup <- function(overwrite = TRUE){

  # alert user
  message("initializing M-BITES Cohort shared methods")

  ##############################################################
  # MBITES-ChooseHost.R
  ##############################################################

  MicroMosquitoFemale$set(which = "public",name = "chooseHost",
            value = mbitesCohort_chooseHost,
            overwrite = overwrite
  )

}
