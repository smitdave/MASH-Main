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
biteWeight_SimEcol <- function(age, aa=1.7, bb=4){

}





hh = function(t,hb,hs,trend=0,wt=1){
  wt*(hb*(1+trend*t)*(1 + hs*sin(2*pi*t/365))^2)
}

make.weight = function(age, aa=1.7, bb=4){
  aa*age/(bb*365+age)
}

# N: number of bites
# mxYR: max year (make bites for 0:mxYR time period)
# hb:
# hs:
# trend:
# wt:
# dt: time step (in days)
make.bites = function(N, mxYR, hb, hs, trend=0, wt=1, dt=5){
  t = seq(0,mxYR*365, by = dt)
  wt = make.weight(t)
  xp = hh(t = t,hb =  hb,hs =  hs,trend = trend, wt=wt)
  xpC = cumsum(xp)
  bite.dt = runif(N,0,max(xpC))
  get.ix = function(i){
    min(which(bite.dt[i] < xpC))
  }
  ixx = sapply(1:N, get.ix)
  t[ixx]
}


make.bites.plot = function(N, mxYR, hb, hs, trend=0, wt=1, dt=5, plot=FALSE){

  t = seq(0,mxYR*365, by = dt)
  wt = make.weight(t) # biting weight is increasing function age; bites should become more dense in time as age increase
  xp = hh(t = t,hb =  hb,hs =  hs,trend = trend, wt=wt) # biting hazard?
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

  # it might be nice to try estimating the intensity function of a NHPP with some package on this data
  # densEst = density(x = sort(t[ixx]))
  # nwSmooth = ksmooth(x = sort(t[ixx]),y=sort(xpC[ixx]),bandwidth = max(t[ixx])/10)
  # lines(x = nwSmooth$x,y = nwSmooth$y,col = "grey20",lwd=2.5)

  # reset graphical pars
  par(mar = oldMar,mfrow=c(1,1))

  # return bite times
  t[ixx]
}


#wt = seq(0, 5*365, by = 5)

age = seq(0:3650)
wt = make.weight(age)
xp = sapply(X = age,FUN = hh, hb=2, hs = 1, wt=wt)
#plot(age, xp, type = "l")

bites= sort(make.bites(70, 10, 1, 5, wt=wt, trend = .05))
make.bites.plot(70, 10, 1, 5, wt=wt, trend = .05,plot = T)
#plot(bites, bites*0, type = "p")
