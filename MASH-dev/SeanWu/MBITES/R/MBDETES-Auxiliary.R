###############################################################################
#         __  _______  ____  _______________________
#        /  |/  / __ )/ __ \/ ____/_  __/ ____/ ___/
#       / /|_/ / __  / / / / __/   / / / __/  \__ \
#      / /  / / /_/ / /_/ / /___  / / / /___ ___/ /
#     /_/  /_/_____/_____/_____/ /_/ /_____//____/
#
#     MBDETES - Evaluate Parameters from a Landscape
#     MBITES Team
#     June 2018
#
###############################################################################


###############################################################################
# Interface to users
###############################################################################

#' MBDETES: Calculate State Transitions Matrix
#'
#' For each \code{\link{Site}} in a landscape, calculate approximate
#' state transitions matrix by calling \code{\link{MBDETES_StateTransitions}}.
#' This requires a full landscape and humans to be constructed first, as well as MBITES parameters to be set (the only MBITES initialization function that is not necessary is \code{MBITES_Initialize}).
#'
#'
#' @param tileID an integer ID of a tile (must have already called \code{\link{Tile_Initialize}} and the appropriate \code{Human_XX_Initialize} routines)
#' @return a list of length equal to number of sites in the tile \code{tileID} where
#'         each element is a named matrix giving transition probabilities between MBDETES
#'         states {F,B,R,L,O,D}
#' @export
MBDETES_Approx <- function(tileID){

  # make sure tile exists
  if(tileID > MBITES:::Globals$get_n_tiles()){
    stop("argument 'tileID' must refer to a valid tile\n")
  }

  # get tile reference
  tile = MBITES:::Globals$get_tile(tileID)
  n = tile$get_sites()$size()

  tile$get_sites()$apply(tag="clear_ActivitySpace")
  tile$get_humans()$apply(tag="oneDay_ActivitySpace")

  # calculate transitions for each site
  out = vector(mode="list",length=n)
  pb = txtProgressBar(min = 0,max = n)
  for(i in 1:n){

    site = tile$get_site(i)
    out[[i]] = MBDETES_StateTransitions(site)

    setTxtProgressBar(pb,i)
  }

  tile$get_sites()$apply(tag="clear_ActivitySpace")

  return(out)
}


###############################################################################
# Refeeding, Overfeeding, Survival
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

#' MBDETES: Probability to Survive Post-prandial Resting Flight
#'
#'
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


###############################################################################
# Searching
###############################################################################

#' MBDETES: Probability to Leave a Site
#' Return the probability for a mosquito to leave a site and (initiate a search).
#' @param site a \code{\link{Site}} object
#' @param res either 'aqua' or 'feed'
#' @param fail did the attempt bout result in a failure?
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


#' MBDETES: Probability to Survive a Bout
#' Compute survival probability as a result of flight and local hazards. This gives the expectation
#' of survival from \code{\link{mbites_survival}}.
#' @param site a \code{\link{Site}} object
#' @param bout character in 'F','B','L','O'
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


###############################################################################
# State Transitions
###############################################################################

#' MBDETES: Blood Feeding Search Bout State Transitions
#'
#' Calculate vector of probabilities to transition between: F -> F, B, D
#' This function is called from \code{\link{MBDETES_StateTransitions}}.
#'
#' @param site a \code{\link{Site}} object
#' @export
MBDETES_FstateTransitions <- function(site){

  A <- MBITES:::Parameters$get_Bs_succeed()
  D1 <- D2 <- MBDETES_PrSurvive(site,"F")
  F1 <- MBDETES_PrLeave(site,"feed",FALSE)

  PAR = list(A=A,D1=D1,F1=F1)
  BFSB_F2X(PAR)
}

BFSB_F2X <- function(PAR){
  with(PAR,{

    F2F <- (A*D1*F1) + ((1-A)*D2)
    F2B <- A*D1*(1-F1)
    F2D <- 1-(F2F+F2B)

    return(c(F2F, F2B, 0, 0, 0, F2D))

  })
}


#' MBDETES: Blood Feeding Attempt Bout State Transitions
#'
#' Calculate vector of probabilities to transition between: B -> F, B, R, D.
#' In the flow diagram of the "B" state (blood feeding attempt bout),
#' probabilities are calculated for all paths except those that branch after
#' entering "R", which are calculated in \code{\link{MBDETES_RperiodTransitions}}.
#' This function is called from \code{\link{MBDETES_StateTransitions}}.
#'
#' @param site a \code{\link{Site}} object
#' @export
MBDETES_BstateTransitions <- function(site){

  # Does the mosquito choose
  A = MBITES:::Parameters$get_B_succeed()

  # check the function
  if(site$has_feed()){
    host = site$get_feed(1L)$RiskQ$typewtsQ()
    host = host/sum(host)
  } else {
    host = c(0,0,0,1)
  }
  B1=host[1]
  B2=host[2]
  B3=host[3]
  B0=host[4]

  # probability of taking a human bloodmeal | human chosen
  h1 = MBITES:::Parameters$get_surviveH()
  h2 = MBITES:::Parameters$get_probeH()
  C1 = 1-h1
  C2 = h1*h2
  C3 = h1*(1-h2)


  h3 = MBITES:::Parameters$get_surviveprobeH()
  h4 = MBITES:::Parameters$get_feedH()
  D1 = 1-h3
  D2 = h3*h4
  D3 = h3*(1-h4)

  # probability of taking a bloodmeal | other host chosen
  z1 = MBITES:::Parameters$get_surviveZ()
  z2 = MBITES:::Parameters$get_feedZ()
  C4 = 1-z1
  C5 = z1*z2
  C6 = z1*(1-z2)

  # probability of failing | trap chosen
  C7 = 0.5
  C8 = 1-C7

  # P(survive PPR | implicit conditioning on having taken a bloodmeal)
  E <- MBDETES_PrPPRFlight()

  # P(Survive | i tried to bloodfeed)
  F2 <-  MBDETES_PrSurvive(site,"B")

  # P(Stay | I want to bloodfeed, i failed to  bloodfeed last time)
  H3 <- 1 - MBDETES_PrLeave(site,"feed",TRUE)

  PAR = list(A=A, B0=B0, B1=B1, B2=B2, B3=B3,
          C1=C1, C2=C2, C3=C3, C4=C4,
          C5=C5, C6=C6, C7=C7, C8=C8,
          D1=D1, D2=D2, D3=D3,
          E=E, F2=F2, H3=H3)
  BFAB_B2X(PAR)
}

BFAB_B2X <- function(PAR){with(PAR,{

  Fail <- (1-A) + A*(B0 + B1*(C3 + C2*D3) + B2*C6 + B3*C8)

  B2F <- Fail*F2*(1-H3)
  B2B <- Fail*F2*H3
  B2R <- A*(B1*C2*D2 + B2*C5)*E
  B2D <- 1-B2R-B2F-B2B

  return(c(B2F, B2B, B2R, 0, 0, B2D))
})}


#' MBDETES: Resting Period State Transitions
#'
#' Calculate vector of probabilities to transition between: R-> B, F, L, O, D
#' This function is called from \code{\link{MBDETES_StateTransitions}}.
#'
#' @param site a \code{\link{Site}} object
#' @export
MBDETES_RperiodTransitions <- function(site){

  # P(Survive | I was busy bloodfeeding)
  F1 <- MBDETES_PrSurvive(site,"B")

  # P(Refeed)
  G <- MBDETES_PrRefeed()

  # H1: P(Stay | I want to bloodfeed, i'm resting)
  H1 <- 1 - MBDETES_PrLeave(site,"feed",FALSE)

  # H2: P(Stay | I want to oviposit, i'm resting)
  H2 <- 1 - MBDETES_PrLeave(site,"aqua",FALSE)

  PAR <- list(F1=F1,G=G,H1=H1,H2=H2)
  BFAB_R2X(PAR)
}

BFAB_R2X <- function(PAR){with(PAR,{
  R2F = F1*G*(1-H1)
  R2B = F1*G*H1
  R2O = F1*(1-G)*H2
  R2L = F1*(1-G)*(1-H2)
  R2D = 1-R2B-R2O-R2L-R2F

  R2ALL = c(R2F,R2B,0,R2L,R2O,R2D)
  return(R2ALL)
})}


#' MBDETES: Egg Laying Search Bout State Transitions
#'
#' Calculate vector of probabilities to transition between: L -> L, O, D
#' This function is called from \code{\link{MBDETES_StateTransitions}}.
#'
#' @param site a \code{\link{Site}} object
#' @export
MBDETES_LstateTransitions <- function(site){

  A <- MBITES:::Parameters$get_Os_succeed()
  D2 <- D1 <- MBDETES_PrSurvive(site,"L")
  R <- MBDETES_PrRefeed()
  F1 <- MBDETES_PrLeave(site,"feed",FALSE)
  F2 <- MBDETES_PrLeave(site,"aqua",FALSE)

  PAR <- list(A=A,D1=D1,D2=D2,R=R,F1=F1,F2=F2)
  ELSB_L2X(PAR)
}

ELSB_L2X <- function(PAR){
  with(PAR,{

    L2F <- A*D1*R*F1  # blood feeding search
    L2B <- A*D1*R*(1-F1) # blood feeding here
    L2L <- (A*D1*(1-R)*F2) + ((1-A)*D2) # oviposition search
    L2O <- A*D1*(1-R)*(1-F2) # oviposition here
    L2D <- (A*(1-D1)) + ((1-A)*(1-D2)) # death

    return(c(L2F,L2B,0,L2L,L2O,L2D))

  })
}


#' MBDETES: Egg Laying Attempt Bout State Transitions
#'
#' Calculate vector of probabilities to transition between: O -> F, B, L, O, D
#' This function is called from \code{\link{MBDETES_StateTransitions}}.
#'
#' @param site a \code{\link{Site}} object
#' @export
MBDETES_OstateTransitions <- function(site){

  A = MBITES:::Parameters$get_O_succeed() # P(bout failure)

  B0 = 1 - site$has_aqua() # P(No aquatic habitats and/or traps present)
  B2 = 0 # P(Trap | Aquatic habitat and/or trap present)
  B1 = (1 - B2) # P(Aquatic habitat | Aquatic habitat and/or trap present)

  C1 = 0
  C2 = 1
  C3 = 0
  C4 = 1
  C5 = 0

  D1 = MBDETES_PrSurvive(site,"O")
  D2 = D1

  E = 0

  F1 = 0.5 # not implemented

  F2 = 1 - MBDETES_PrLeave(site,"feed",FALSE) # P(leave | i want to blood feed, bout was not a failure)
  F3 = 1 - MBDETES_PrLeave(site,"aqua",TRUE) # P(leave | i want to oviposit, bout was a failure)

  PAR = list(A=A, B0=B0, B1=B1, B2=B2,
          C1=C1, C2=C2, C3=C3, C4=C4, C5=C5,
          D1=D1, D2=D2, E=E, F1=F1, F2=F2, F3=F3)
  ELAB_O2X(PAR)
}

ELAB_O2X <- function(PAR){with(PAR,{

  Fail = (1-A) + (A*B0) + (A*(1-B0)*B1*C3) + (A*(1-B0)*B2*C5)

  O2L <- (Fail*D2*(1-F3)) + (A*(1-B0)*B1*C2*D1*E*(1-F1))
  O2O <- (Fail*D2*F3) + (A*(1-B0)*B1*C2*D1*E*F1)
  O2F <- (A*(1-B0)*B1*C2*D1*(1-E)*(1-F2))
  O2B <- (A*(1-B0)*B1*C2*D1*(1-E)*F2)
  O2D <- 1-O2L-O2O-O2F-O2B
  return(c(O2F,O2B,0,O2L,O2O,O2D))
})}


#' MBDETES: The State Transition Matrix
#'
#' Calculate state transitions matrix for one \code{\link{Site}} object.
#' Transitions are of form FBRLO to FBRLOD (the rows sum to one).
#'
#' @param site a \code{\link{Site}} object
#' @export
MBDETES_StateTransitions <- function(site){
  FBRLOD = matrix(0,nrow=6,ncol=6,dimnames=list(c("F","B","R","L","O","D"),c("F","B","R","L","O","D")))
  FBRLOD[1,] = MBDETES_FstateTransitions(site)
  FBRLOD[2,] = MBDETES_BstateTransitions(site)
  FBRLOD[3,] = MBDETES_RperiodTransitions(site)
  FBRLOD[4,] = MBDETES_LstateTransitions(site)
  FBRLOD[5,] = MBDETES_OstateTransitions(site)
  FBRLOD[6,6] = 1 # death is absorbing
  return(FBRLOD)
}
