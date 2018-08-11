###############################################################################
#         __  _______  ____  _______________________
#        /  |/  / __ )/ __ \/ ____/_  __/ ____/ ___/
#       / /|_/ / __  / / / / __/   / / / __/  \__ \
#      / /  / / /_/ / /_/ / /___  / / / /___ ___/ /
#     /_/  /_/_____/_____/_____/ /_/ /_____//____/
#
#     MBDETES - Auxiliary Functions to Help Calculate Parameters for MBITES/MBDETES mapping
#     MBITES Team
#     June 2018
#
###############################################################################


###############################################################################
# Refeeding
###############################################################################

#' MBDETES: Refeeding Probability
#'
#' Computes the integral of the probability of
#' feeding conditioned on the size of the bloodmeal,
#' multiplied by the probability of bloodmeal
#' of that size.
#'  * MBITES functions can be found in MBITES-Oogenesis.R
#'
#' @export
MBDETES_PrRefeed <- function(){
  rf_a = MBITES:::Parameters$get_rf_a()
  rf_b = MBITES:::Parameters$get_rf_b()
  bm_a = MBITES:::Parameters$get_bm_a()
  bm_b = MBITES:::Parameters$get_bm_b()

  FF = function(X){
    dbeta(X,bm_a,bm_b)*(2+rf_b)/(1+rf_b) - exp(rf_a*X)/(rf_b + exp(rf_a*X))
  }
  integrate(FF, 0, 1)$value
}

#' MBDETES: Refeeding Probability (Parameter Calibration)
#'
#' Calculate the expectation probability of refeeding
#' (integrating the probability with respect to the probability density function describing blood meal size).
#'
#' @param rf_a shape parameters for refeeding
#' @param rf_b shape parameters for refeeding
#' @param bm_a alpha parameter for beta distributed blood meal size
#' @param bm_b beta parameter for beta distributed blood meal size
#'
#' @export
MBDETES_PrRefeed_pars <- function(rf_a=10,rf_b=3,bm_a=7.5,bm_b=2.5){

  FF = function(X,rf_a,rf_b,bm_a,bm_b){
    dbeta(X,bm_a,bm_b)*(2+rf_b)/(1+rf_b) - exp(rf_a*X)/(rf_b + exp(rf_a*X))
  }
  integrate(f=FF,lower=0,upper=1,rf_a=rf_a,rf_b=rf_b,bm_a=bm_a,bm_b=bm_b)$value
}

#' MBDETES: Refeeding Probability Objective Function (Parameter Calibration)
#'
#' Objective function for optimization to find \code{rf_a} and \code{rf_b} to match target value of 'G'
#' parameter (probability to refeed following resting) by minimizing absolute value of difference between \code{\link{MBDETES_PrRefeed_pars}} evaluated at parameters \code{theta} and argument \code{val}.
#' Returns a closure that can be passed to optimization functions.
#'
#' @param val target value of paramter 'G'
#'
#' @export
MBDETES_PrRefeed_closure <- function(val){

  fn <- function(theta){
    fnval <- MBDETES_PrRefeed_pars(rf_a = theta[1],rf_b = theta[2])
    return(abs(fnval-val))
  }
  return(fn)
}

#' MBDETES: Refeeding Probability Calibration
#'
#' Use \code{\link[stats]{constrOptim}} to find refeeding shape parameters to match target MBDETES paramter value.
#' If providing starting values, \code{rf_a} must be greater than \code{rf_b} to match the linear constraints used in the optimization.
#'
#' @param G target value of 'G' (probability to refeed following resting)
#' @param rf_a starting values for refeeding shape parameters
#' @param rf_b starting values for refeeding shape parameters
#'
#' @export
MBDETES_PrRefeed_optim <- function(G,rf_a=10,rf_b=3){

  if(rf_b>=rf_a){stop("'rf_a' must be strictly greater than 'rf_b'")}

  fn <- MBDETES_PrRefeed_closure(G)

  ui <- matrix(data=c(1,0,0,1,1,-1),nrow=3,ncol=2,byrow=TRUE)
  ci <- rep(0,3)

  constrOptim(theta = c(rf_a,rf_b),f = fn,grad = NULL,ui = ui,ci = ci)
}


###############################################################################
# Overfeeding
###############################################################################

#' MBDETES: Overfeeding Probability
#'
#' Computes the integral of the probability of
#' overfeeding conditioned on the size of the bloodmeal,
#' multiplied by the probability of bloodmeal
#' of that size.
#'  * MBITES functions can be found in MBITES-BloodMeal.R
#'
#' @export
MBDETES_PrOverfeed <- function(){
  of_a = MBITES:::Parameters$get_of_a()
  of_b = MBITES:::Parameters$get_of_b()
  bm_a = MBITES:::Parameters$get_bm_a()
  bm_b = MBITES:::Parameters$get_bm_b()

  FF = function(X){
    dbeta(X,bm_a,bm_b)*exp(of_a*X)/(of_b + exp(of_a*X))
  }
  integrate(FF, 0, 1)$value
}

#' MBDETES: Overfeeding Probability (Parameter Calibration)
#'
#' Calculate the expectation probability of overfeeding
#' (integrating the probability with respect to the probability density function describing blood meal size).
#'
#' @param of_a shape parameters for overfeeding
#' @param of_b shape parameters for overfeeding
#' @param bm_a alpha parameter for beta distributed blood meal size
#' @param bm_b beta parameter for beta distributed blood meal size
#'
#' @export
MBDETES_PrOverfeed_calibrate <- function(of_a=5,of_b=5e3,bm_a=7.5,bm_b=2.5){

  FF = function(X,of_a,of_b,bm_a,bm_b){
    dbeta(X,bm_a,bm_b)*exp(of_a*X)/(of_b + exp(of_a*X))
  }
  integrate(f=FF,lower=0,upper=1,of_a=of_a,of_b=of_b,bm_a=bm_a,bm_b=bm_b)$value
}


###############################################################################
# Post-Prandial Resting Flight Mortality
###############################################################################

#' MBDETES: Probability to Survive Post-prandial Resting Flight
#'
#' Computes the integral of the probability of
#' mortality conditioned on the size of the bloodmeal,
#' multiplied by the probability of bloodmeal
#' of that size.
#' @export
MBDETES_PrPPRFlight <- function(){
  PPR_a = MBITES:::Parameters$get_PPR_a()
  PPR_b = MBITES:::Parameters$get_PPR_b()
  bm_a = MBITES:::Parameters$get_bm_a()
  bm_b = MBITES:::Parameters$get_bm_b()

  FF = function(X){
    dbeta(X,bm_a,bm_b)*exp(PPR_a*X)/(PPR_b + exp(PPR_a*X))
  }
  integrate(FF, 0, 1)$value
}

#' MBDETES: Probability to Survive Post-prandial Resting Flight (Parameter Calibration)
#'
#' Calculate the expectation probability to survive the post-prandial resting flight
#' (integrating the probability with respect to the probability density function describing blood meal size).
#'
#' @param PPR_a shape parameters for ppr mortality
#' @param PPR_b shape parameters for ppr mortality
#' @param bm_a alpha parameter for beta distributed blood meal size
#' @param bm_b beta parameter for beta distributed blood meal size
#'
#' @export
MBDETES_PrPPRFlight_pars <- function(PPR_a=15,PPR_b=300,bm_a=7.5,bm_b=2.5){

  FF = function(X,PPR_a,PPR_b,bm_a,bm_b){
    dbeta(X,bm_a,bm_b)*exp(PPR_a*X)/(PPR_b + exp(PPR_a*X))
  }

  integrate(f=FF,lower=0,upper=1,PPR_a=PPR_a,PPR_b=PPR_b,bm_a=bm_a,bm_b=bm_b)$value
}

#' MBDETES: Probability to Survive Post-prandial Resting Flight Objective Function (Parameter Calibration)
#'
#' Objective function for optimization to find \code{PPR_a} and \code{PPR_b} to match target value of 'E'
#' parameter (probability to survive post-prandial resting flight) by minimizing absolute value of difference between \code{\link{MBDETES_PrRefeed_pars}} evaluated at parameters \code{theta} and argument \code{val}.
#' Returns a closure that can be passed to optimization functions.
#'
#' @param val target value of paramter 'E'
#'
#' @export
MBDETES_PrPPRFlight_closure <- function(val){

  fn <- function(theta){
    fnval <- MBDETES_PrPPRFlight_pars(PPR_a = theta[1],PPR_b = theta[2])
    return(abs(fnval-val))
  }
  return(fn)
}

#' MBDETES: Probability to Survive Post-prandial Resting Flight Calibration
#'
#' Use \code{\link[stats]{constrOptim}} to find ppr mortality shape parameters to match target MBDETES paramter value.
#' If providing starting values, \code{PPR_a} must be less than \code{PPR_b} to match the linear constraints used in the optimization.
#'
#' @param E target value of 'E' (probability to survive post-prandial resting flight)
#' @param PPR_a starting values for ppr mortality shape parameters
#' @param PPR_b starting values for ppr mortality shape parameters
#'
#' @export
MBDETES_PrPPRFlight_optim <- function(E,PPR_a=15,PPR_b=300){

  if(PPR_a>=PPR_b){stop("'PPR_a' must be strictly less than 'PPR_b'")}

  fn <- MBDETES_PrPPRFlight_closure(E)

  ui <- matrix(data=c(1,0,0,1,-1,1),nrow=3,ncol=2,byrow=TRUE)
  ci <- rep(0,3)

  constrOptim(theta = c(PPR_a,PPR_b),f = fn,grad = NULL,ui = ui,ci = ci)
}


###############################################################################
# Searching (Probability to Leave a Site)
###############################################################################

#' MBDETES: Probability to Leave a Site
#'
#' Return the probability for a mosquito to leave a site and (initiate a search).
#'
#' @param site a \code{\link{Site}} object
#' @param res either 'aqua' or 'feed'
#' @param fail did the attempt bout result in a failure?
#'
#' @export
MBDETES_PrLeave <- function(site,res,fail=FALSE){
  # P(Leave | Fail)
  if(fail){
    # if I need an aquatic habitat
    if(res=="aqua"){
      #  P(Leave | Fail, Site doesn't have what I need)
      if(!site$has_aqua()){
        return(1)
      #  P(Leave | Fail, Site has what I need)
      } else {
        p <- MBITES:::Parameters$get_boutFail_p()
        p <- p + ((1-p)*MBITES:::Parameters$get_disperse())
        return(p)
      }
    }
    # if I need a blood feeding queue
    if(res=="feed"){
      #  P(Leave | Fail, Site doesn't have what I need)
      if(!site$has_feed()){
        return(1)
      #  P(Leave | Fail, Site has what I need)
      } else {
        p <- MBITES:::Parameters$get_boutFail_p()
        p <- p + ((1-p)*MBITES:::Parameters$get_disperse())
        return(p)
      }
    }
  # P(Leave | Success)
  } else {
    # if i need an aquatic habitat
    if(res=="aqua"){
      # P(Leave | Success, Site doesn't have what I need)
      if(!site$has_aqua()){
        return(1)
      # P(Leave | Success, Site has what I need)
      } else {
        return(MBITES:::Parameters$get_disperse())
      }
    }
    if(res=="feed"){
      # P(Leave | Success, Site doesn't have what I need)
      if(!site$has_feed()){
        return(1)
      # P(Leave | Success, Site has what I need)
      } else {
        return(MBITES:::Parameters$get_disperse())
      }
    }
  }
}

#' MBDETES: Probability to Leave a Site (Paramter Calibration)
#'
#' Parameter explicit version of \code{\link{MBDETES_PrLeave}}
#'
#' @param site does the \code{\link{Site}} have the resource the mosquito needs?
#' @param boutFail_p probability to search even if the site has what you need after a failure
#' @param disperse baseline probability to leave
#' @param res either 'aqua' or 'feed'
#' @param fail did the attempt bout result in a failure?
#'
#' @export
MBDETES_PrLeave_pars <- function(site,res,boutFail_p,disperse,fail=FALSE){
  # P(Leave | Fail)
  if(fail){
    # if I need an aquatic habitat
    if(res=="aqua"){
      #  P(Leave | Fail, Site doesn't have what I need)
      if(!site){
        return(1)
      #  P(Leave | Fail, Site has what I need)
      } else {
        p <- boutFail_p
        p <- p + ((1-p)*disperse)
        return(p)
      }
    }
    # if I need a blood feeding queue
    if(res=="feed"){
      #  P(Leave | Fail, Site doesn't have what I need)
      if(!site){
        return(1)
      #  P(Leave | Fail, Site has what I need)
      } else {
        p <- boutFail_p
        p <- p + ((1-p)*disperse)
        return(p)
      }
    }
  # P(Leave | Success)
  } else {
    # if i need an aquatic habitat
    if(res=="aqua"){
      # P(Leave | Success, Site doesn't have what I need)
      if(!site){
        return(1)
      # P(Leave | Success, Site has what I need)
      } else {
        return(disperse)
      }
    }
    if(res=="feed"){
      # P(Leave | Success, Site doesn't have what I need)
      if(!site){
        return(1)
      # P(Leave | Success, Site has what I need)
      } else {
        return(disperse)
      }
    }
  }
}


###############################################################################
# Bout Survival Probability
###############################################################################

#' MBDETES: Probability to Survive a Bout
#'
#' Compute survival probability as a result of flight and local hazards. This gives the expectation
#' of survival from \code{\link{mbites_survival}}.
#'
#' @param site a \code{\link{Site}} object
#' @param bout character in 'F','B','L','O'
#'
#' @export
MBDETES_PrSurvive <- function(site, bout){
  if(bout == "F"){
    p <- MBITES:::Parameters$get_Bs_surv()
  }
  if(bout == "B"){
    p <- MBITES:::Parameters$get_B_surv()
  }
  if(bout == "L"){
    p <- MBITES:::Parameters$get_Os_surv()
  }
  if(bout == "O"){
    p <- MBITES:::Parameters$get_O_surv()
  }
  return(p * (1-site$get_haz()))
}

#' MBDETES: Probability to Survive a Bout (Parameter Calibration)
#'
#' Parameter explicit version of \code{\link{MBDETES_PrSurvive}}
#'
#' @param haz hazard associated with the \code{\link{Site}}
#' @param surv the baseline survival probability (for 'F' maps to \code{Bs_surv}, 'B' maps to \code{B_surv}, 'L' maps to \code{Os_surv}, 'O' maps to \code{O_surv})
#'
#' @export
MBDETES_PrSurvive_pars <- function(haz,surv){
  return(surv * (1 - haz))
}


###############################################################################
# Specific Survival Probability (depending on blood meal size)
###############################################################################

#' MBDETES: Probability of Surviving the post-prandial flight (laden mosquito)
#'
#'
#' @export
MBDETES_SurvLaden <- function(){
  lrl = MBDETES_RestingParam()
  land = lrl[1]; retry = lrl[2]; leave = lrl[3]
  PPR_a = MBITES:::Parameters$get_PPR_a()
  PPR_b = MBITES:::Parameters$get_PPR_b()
  bm_a = MBITES:::Parameters$get_bm_a()
  bm_b = MBITES:::Parameters$get_bm_b()
  FF = function(X){
     p = dbeta(X,bm_a,bm_b)*exp(PPR_a*X)/(PPR_b + exp(PPR_a*X))
     p*(land+leave)/(1-retry)
  }
  integrate(FF, 0, 1)$value
}

#' MBDETES: Probability of Leaving, post prandially (laden mosquito)
#'
#'
#' @export
MBDETES_LeaveLaden <- function(){
  lrl = MBDETES_RestingParam()
  land = lrl[1]; retry = lrl[2]; leave = lrl[3]
  PPR_a = MBITES:::Parameters$get_PPR_a()
  PPR_b = MBITES:::Parameters$get_PPR_b()
  bm_a = MBITES:::Parameters$get_bm_a()
  bm_b = MBITES:::Parameters$get_bm_b()
  FF = function(X){
     p = dbeta(X,bm_a,bm_b)*exp(PPR_a*X)/(PPR_b + exp(PPR_a*X))
     p*leave/(1-retry)
  }
  integrate(FF, 0, 1)$value
}

#' MBDETES: Probability of Surviving the post-prandial flight (unladen mosquito)
#'
#'
#' @export
MBDETES_SurvUnladen <- function(){
  lrl = MBDETES_RestingParam()
  land = lrl[1]; retry = lrl[2]; leave = lrl[3]
  (land+leave)/(1-retry)
}

#' MBDETES: Probability of Leaving, post prandially (unladen mosquito)
#'
#'
#' @export
MBDETES_LeaveUnladen <- function(){
  lrl = MBDETES_RestingParam()
  land = lrl[1]; retry = lrl[2]; leave = lrl[3]
  leave/(1-retry)
}


###############################################################################
# Resting Survival
###############################################################################

#' MBDETES: Resting spot probabilities
#'
#' Calculate resting spot probabilities according to a mixture distribution with
#' equal weights given to blood feeding and oviposition behavioral states (B,O).
#'
#' @export
MBDETES_RestingParam <- function(){

  inandout = MBITES:::Parameters$get_InAndOut()
  inandout = inandout/rowSums(inandout)
  inandout_eigen = eigen(t(inandout))
  inandout_stationary = inandout_eigen$vectors[,1]/sum(inandout_eigen$vectors[,1])

  mix_wts = rep(1,2)/2
  probs = mix_wts[1]*(inandout_stationary* MBITES:::Parameters$get_wts("B")) +
          mix_wts[2]*(inandout_stationary* MBITES:::Parameters$get_wts("O"))

  land = sum(probs[1:3])
  retry = probs[4]
  leave = probs[5]

  out = unname(c(land, retry, leave))
  return(out/sum(out))
}

#' MBDETES: Probability of Death from Local (Site-specific) Hazards
#'
#' Mortality due to local hazards is a Bernoulli event (see \code{\link{mbites_surviveHazards}})
#' so this function just returns the probability of death.
#'
#' @export
MBDETES_LocalHazMortality <- function(site){
  return(site$get_haz())
}
