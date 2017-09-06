###############################################################################
#
#      _____ _           ______           __
#     / ___/(_)___ ___  / ____/________  / /
#     \__ \/ / __ `__ \/ __/ / ___/ __ \/ /
#    ___/ / / / / / / / /___/ /__/ /_/ / /
#   /____/_/_/ /_/ /_/_____/\___/\____/_/
#
#   MASH-MACRO
#   SimEcol (Simulated Ecology): Biting Time Series
#   David Smith, Hector Sanchez, Sean Wu
#   August 30, 2017
#
###############################################################################

#' Simulated Ecology: Biting Intensity Function
#'
#' Evaluate instantaneous biting intensity at time t. Biting intensity is modulated by periodic
#' seasonality.
#'
#' @param t time (units of days)
#' @param hb baseline biting
#' @param hs amplitude of seasonal biting around baseline \code{hb}
#' @param trend modulate biting intensity over time
#' @param wt biting weight as function of time (is a vector calculated from \code{\link{biteWeight_SimEcol}})
#'
#'
#' @export
biteIntensity_SimEcol <- function(t,hb,hs,trend=0,wt=0){
  wt*(hb*(1+trend*t)*(1 + hs*sin(2*pi*t/365))^2)
}


#' Simulated Ecology: Age-dependent Biting Weight
#'
#' Evaluate age-dependent human biting weight.
#'
#' @param age numeric value of age, given in days since birth
#' @param aa maximum value attained by the function
#' @param bb speed at which function attains its maximum (smaller values lead to faster attainment of maximum, larger values become linear, must be >0)
#'
#'
#' @export
biteWeight_SimEcol <- function(age, aa=1.7, bb=4){
  aa*age/(bb*365+age)
}



#' Simulated Ecology: Generate Seasonal Biting Pattern
#'
#' Generate a seasonally-forced lifetime biting pattern for a single human as a non-homogeneous Poisson process.
#'
#' @param N total number of bites over time interval
#' @param mxYR time in years to simulate (will run from 0 to mxYR)
#' @param hb passed to \code{\link{biteIntensity_SimEcol}}
#' @param hs passed to \code{\link{biteIntensity_SimEcol}}
#' @param trend strength of seasonal trend, passed to \code{\link{biteIntensity_SimEcol}}
#' @param dt time step (in days)
#'
#' @export
makeBites_SimEcol <- function(N, mxYR, hb, hs, trend = 0, dt = 5){

  t = seq(0,mxYR*365, by = dt)
  wt = biteWeight_SimEcol(t) # biting weight is increasing function age; bites should become more dense in time as age increase
  xp = biteIntensity_SimEcol(t = t,hb =  hb,hs =  hs,trend = trend, wt=wt) # biting hazard?
  xpC = cumsum(xp) # cumulative biting hazard?
  bite.dt = runif(N,0,max(xpC))

  ixx = vapply(X = 1:N,FUN = function(i,bite.dt,xpC){
    min(which(bite.dt[i] < xpC))
  },FUN.VALUE = integer(1),bite.dt=bite.dt,xpC=xpC)

  return(t[ixx])
}


#' Simulated Ecology: Plot Seasonal Biting Pattern
#'
#' Plot the non-homogeneous Poisson biting process produced by \code{\link{makeBitesPlot_SimEcol}}.
#'
#' @param N total number of bites over time interval
#' @param mxYR time in years to simulate (will run from 0 to mxYR)
#' @param hb passed to \code{\link{biteIntensity_SimEcol}}
#' @param hs passed to \code{\link{biteIntensity_SimEcol}}
#' @param trend strength of seasonal trend, passed to \code{\link{biteIntensity_SimEcol}}
#' @param dt time step (in days)
#' @param returnVals if \code{TRUE}, return the biting times
#'
#' @export
makeBitesPlot_SimEcol <- function(N, mxYR, hb, hs, trend = 0, dt = 5, returnVals = FALSE){

  t = seq(0,mxYR*365, by = dt)
  wt = biteWeight_SimEcol(t) # biting weight is increasing function age; bites should become more dense in time as age increase
  xp = biteIntensity_SimEcol(t = t,hb =  hb,hs =  hs,trend = trend, wt=wt) # biting hazard?
  xpC = cumsum(xp) # cumulative biting hazard?
  bite.dt = runif(N,0,max(xpC))

  ixx = vapply(X = 1:N,FUN = function(i,bite.dt,xpC){
    min(which(bite.dt[i] < xpC))
  },FUN.VALUE = integer(1),bite.dt=bite.dt,xpC=xpC)

  # set up plotting surface
  par(mfrow=c(2,2))

  # plot biting weight
  plot(wt,type="l",xaxt="n",xlab="Age (Years)",ylab="Biting Weight",col="purple",lwd=1.25)
  xAxis = which(t %% 365 == 0)
  axis(1, at=xAxis, labels=round(t[xAxis]/365,2),las=1)

  # set up graphical pars
  oldMar = par("mar")
  par(mar = c(5,5,2,5))

  # plot xp
  plot(xp,type="l",xaxt="n",ylab="Biting Hazard",xlab="Age (Years)",col="red3",lwd=1.2)
  axis(1, at=xAxis, labels=round(t[xAxis]/365,2),las=1)
  par(new = T)
  plot(xpC,type="l", axes=F, xlab=NA, ylab=NA,col="darkgreen")
  # yAxis = seq(from=min(xpC),to=round(x = max(xpC),digits = -3),by=500)
  # axis(side = 4,at = yAxis,labels = yAxis)
  mtext(side = 4, line = 2.5, 'Cumulative Biting Hazard')

  plot(sort(t[ixx]),sort(xpC[ixx]),type="p",pch=16,xlab="Time of Sampled Bites",ylab="Cumulative Biting Hazard",xaxt="n",col="firebrick3")
  xAxis = seq(from=0,to=round(max(t[ixx]),digits = -3),length.out = 10)
  axis(side = 1,at = xAxis,labels = round(xAxis/365,1))

  # reset graphical pars
  par(mar = oldMar,mfrow=c(1,1))

  if(returnVals){
    return(t[ixx])
  }
}
