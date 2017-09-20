###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES-Male: Parameters
#   MASH-MICRO Team
#   September 19, 2017
#
###############################################################################

#' Generate Parameters for M-BITES Male Lifecycle Module
#'
#' Generate named list of parameters used throughout MICRO/M-BITES. All arguments have default values which are listed below before the definition.
#'
#' @param maleHistory logical; write male histories to JSON
#' @param mateFitness vector of relative mating fitness
#' @param M_wts landing spot weights
#' @param S_wts landing spot weights
#' @param R_wts landing spot weights
#' @param InAndOut matrix of landing probabilities
#' @param M_surv baseline probability to survive flights associated with mating
#' @param S_surv baseline probability to survive flights associated with sugar feeding
#' @param R_surv baseline probability to survive flights associated with resting
#' @param M_succeed probability to successfully enter swarm in mating bout
#' @param S_succeed probability to successfully locate sugar source and replenish energy in sugar feeding bout
#' @param R_succeed probability to successfully locate suitable resting spot in resting bout
#' @param gammaShape shape parameter for Gamma distributed dwell times; if gammaShape = 1 same as exponential, variance decreases as gammaShape becomes large (see \code{\link{mbitesMale_timingGamma}})
#' @param M_time average duration of time spent in mating state (unconditional on next state)
#' @param S_time average duration of time spent in sugar feeding state (unconditional on next state)
#' @param R_time average duration of time spent in resting state (unconditional on next state)
#' @param TATTER logical; simulate multiplicative effect on mortality of wing tattering
#' @param ttsz.p zero-inflation of Beta distributed per-bout wing tattering damage
#' @param ttsz.a alpha parameter of Beta distributed per-bout wing tattering damage
#' @param ttsz.b beta parameter of Beta distributed per-bout wing tattering damage
#' @param ttr.a shape parameter of per-bout multiplicative effect on mortality of wing tattering damage
#' @param ttr.a shape parameter of per-bout multiplicative effect on mortality of wing tattering damage
#' @param SENESCE logical; simulate multiplicative effect on mortality of senescence
#' @param sns.a shape parameter of per-bout probability of death due to mosquito age
#' @param sns.b shape parameter of per-bout probability of death due to mosquito age
#' @param S.u per-bout energy expenditure
#' @param S.a shape parameter of per-bout probability of survival as function of energy reserves
#' @param S.b shape parameter of per-bout probability of survival as function of energy reserves
#' @param S.sa shape parameter of probability to queue sugar bout as function of energy reserves
#' @param S.sb shape parameter of probability to queue sugar bout as function of energy reserves
#' @return a named list of parameters
#' @examples
#' MBITES.Male.Parameters()
#' @export
MBITES.Male.Parameters <- function(

  maleHistory = FALSE,
  mateFitness = 1,

  M_wts = rep(1,5),
  S_wts = rep(1,5),
  R_wts = rep(1,5),
  InAndOut = matrix(data = c(4,2,2,1,6,
                             2,1,1,1,4,
                             1,1,1,1,2,
                             0,0,0,0,1,
                             1,1,2,1,0),
                             nrow = 5,ncol = 5,byrow = TRUE,dimnames = list(c("i","w","v","r","l"),c("i","w","v","r","l"))),

  M_surv = 0.85,
  S_surv = 0.80,
  R_surv = 0.95,

  M_succeed = 0.95,
  S_succeed = 0.95,
  R_succeed = 0.99,

  gammaShape = 8,

  M_time = 0.5,
  S_time = 0.65,
  R_time = 0.75,

  TATTER = FALSE,
  ttsz.p = 0.5,
  ttsz.a = 5,
  ttsz.b = 95,
  ttr.a = 15,
  ttr.b = 500,

  SENESCE = FALSE,
  sns.a = 0.1,
  sns.b = 100,

  S.u = 1/5,
  S.a = 20,
  S.b = 10,
  S.sa = 15,
  S.sb = 5

){
  return(list(

    maleHistory = maleHistory,
    mateFitness = mateFitness,
    M_wts = M_wts,
    S_wts = S_wts,
    R_wts = R_wts,
    InAndOut = InAndOut,

    M_surv = M_surv,
    S_surv = S_surv,
    R_surv = R_surv,

    M_succeed = M_succeed,
    S_succeed = S_succeed,
    R_succeed = R_succeed,

    gammaShape = gammaShape,

    M_time = M_time,
    S_time = S_time,
    R_time = R_time,

    TATTER = TATTER,
    ttsz.p = ttsz.p,
    ttsz.a = ttsz.a,
    ttsz.b = ttsz.b,
    ttr.a = ttr.a,
    ttr.b = ttr.b,

    SENESCE = SENESCE,
    sns.a = sns.a,
    sns.b = sns.b,

    S.u = S.u,
    S.a = S.a,
    S.b = S.b,
    S.sa = S.sa,
    S.sb = S.sb,

    initState = "M",
    lspot = c("i","w","v","r","l")

  ))

}
