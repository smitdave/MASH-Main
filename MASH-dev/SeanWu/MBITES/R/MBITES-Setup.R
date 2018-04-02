###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Setup Simulation
#     MBITES Team
#     March 2018
#
###############################################################################

#' MBITES: Setup
#'
#' Setup routines should be called prior to making any actual objects (ie; before calling any initialization functions, such as \code{\link{MBITES_Initialize}}, \code{link{Tile_Initialize}}, etc).
#' The setup function is responsible for assigning concrete methods to proper interfaces in the \code{\link{Mosquito}}, \code{\link{Mosquito_Female}}, and \code{\link{Mosquito_Male}}
#' classes as well as setting up the time-to-event closures in the global \code{\link{MBITES_Parameters}} object needed to sample waiting times to next launch.
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @name MBITES Setup
NULL
#> NULL

###############################################################################
# Initialize MBITES model
###############################################################################

#' MBITES: Setup Model
#'
#' See \code{\link{MBITES Setup}} for more details. Note that because of lazy evaluation of function arguments only arguments that are
#' needed for the specific models chosen need to be provided as input by the user.
#'
#' @details
#' Arguments for the setup function are listed below:
#'  * timing_model: integer flag for time-to-event distribution. All timing models require further arguments which are given below their main heading.
#'    * 1: shifted exponential distribution, see \code{\link{make_ttEvent_Exp}} for more details (if selecting this model please enter required arguments below)
#'      * rate_b: inverse of average waiting time to next launch for blood feeding attempt bout
#'      * tmin_b: minimum waiting time prior to next launch for blood feeding attempt bout
#'      * rate_o: inverse of average waiting time to next launch for oviposition attempt bout
#'      * tmin_o: minimum waiting time prior to next launch for oviposition attempt bout
#'      * rate_m: inverse of average waiting time to next launch for mating attempt bout
#'      * tmin_m: minimum waiting time prior to next launch for mating attempt bout
#'      * rate_s: inverse of average waiting time to next launch for sugar feeding attempt bout
#'      * tmin_s: minimum waiting time prior to next launch for sugar feeding attempt bout
#'      * rate_bs: inverse of average waiting time to next launch for blood feeding search bout
#'      * tmin_bs: minimum waiting time prior to next launch for blood feeding search bout
#'      * rate_os: inverse of average waiting time to next launch for oviposition search bout
#'      * tmin_os: minimum waiting time prior to next launch for oviposition search bout
#'      * rate_ms: inverse of average waiting time to next launch for mating search bout
#'      * tmin_ms: minimum waiting time prior to next launch for mating search bout
#'      * rate_ss: inverse of average waiting time to next launch for sugar feeding search bout
#'      * tmin_ss: minimum waiting time prior to next launch for sugar feeding search bout
#'    * 2: shifted gamma distribution, note that minimum waiting time arguments still need to be provided, as given above, see \code{\link{make_ttEvent_Gamma}} for more details (if selecting this model please enter required arguments below)
#'      * mean_b: inverse of average waiting time to next launch for blood feeding attempt bout
#'      * cv_b: coefficient of variation between mean and variance of waiting time
#'      * mean_o: inverse of average waiting time to next launch for oviposition attempt bout
#'      * cv_o: coefficient of variation between mean and variance of waiting time
#'      * mean_m: inverse of average waiting time to next launch for mating attempt bout
#'      * cv_m: coefficient of variation between mean and variance of waiting time
#'      * mean_s: inverse of average waiting time to next launch for sugar feeding attempt bout
#'      * cv_s: coefficient of variation between mean and variance of waiting time
#'      * mean_bs: inverse of average waiting time to next launch for blood feeding search bout
#'      * cv_bs: coefficient of variation between mean and variance of waiting time
#'      * mean_os: inverse of average waiting time to next launch for oviposition search bout
#'      * cv_os: coefficient of variation between mean and variance of waiting time
#'      * mean_ms: inverse of average waiting time to next launch for mating search bout
#'      * cv_ms: coefficient of variation between mean and variance of waiting time
#'      * mean_ss: inverse of average waiting time to next launch for sugar feeding search bout
#'      * cv_ss: coefficient of variation between mean and variance of waiting time
#'    * 3: shifted diurnal (sinusoidal) distribution, see \code{\link{make_ttEvent_Diurnal}} (if selecting this model please enter required arguments below)
#'      * not implemented yet
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @export
MBITES_Setup <- function(

  # Timing
  timing_model = 1L,
  # exp
  rate_b = NULL,
  tmin_b = NULL,
  rate_o = NULL,
  tmin_o = NULL,
  rate_m = NULL,
  tmin_m = NULL,
  rate_s = NULL,
  tmin_s = NULL,
  rate_bs = NULL,
  tmin_bs = NULL,
  rate_os = NULL,
  tmin_os = NULL,
  rate_ms = NULL,
  tmin_ms = NULL,
  rate_ss = NULL,
  tmin_ss = NULL,
  # gamma
  mean_b = NULL,
  cv_b = NULL,
  mean_o = NULL,
  cv_o = NULL,
  mean_m = NULL,
  cv_m = NULL,
  mean_s = NULL,
  cv_s = NULL,
  mean_bs = NULL,
  cv_bs = NULL,
  mean_os = NULL,
  cv_os = NULL,
  mean_ms = NULL,
  cv_ms = NULL,
  mean_ss = NULL,
  cv_ss = NULL,

  # Oogenesis
  oogenesis_model = 1L
){

  # set flag
  MBITES:::Globals$set_SETUP(TRUE)

  # Timing
  MBITES:::Parameters$set_ttEvent(
    timing_model,
    # exp
    rate_b,
    tmin_b,
    rate_o,
    tmin_o,
    rate_m,
    tmin_m,
    rate_s,
    tmin_s,
    rate_bs,
    tmin_bs,
    rate_os,
    tmin_os,
    rate_ms,
    tmin_ms,
    rate_ss,
    tmin_ss,
    # gamma
    mean_b,
    cv_b,
    tmin_b,
    mean_o,
    cv_o,
    tmin_o,
    mean_m,
    cv_m,
    tmin_m,
    mean_s,
    cv_s,
    tmin_s,
    mean_bs,
    cv_bs,
    tmin_bs,
    mean_os,
    cv_os,
    tmin_os,
    mean_ms,
    cv_ms,
    tmin_ms,
    mean_ss,
    cv_ss,
    tmin_ss
  )


  # Oogenesis
  switch(oogenesis_model,
    "1" = {
      # model
      Mosquito_Female$set(which = "public",name = "oogenesis",
                value = mbites_oogenesis1, overwrite = TRUE
      )
    },
    "2" = {
      # model
      Mosquito_Female$set(which = "public",name = "oogenesis",
                value = mbites_oogenesis2, overwrite = TRUE
      )
      # egg provision field
      Mosquito_Female$set(which = "private",name = "eggP",
                value = numeric(1), overwrite = TRUE
      )
    },
    {stop("invalid entry for 'oogenesis_model'\n")}
  )

}
