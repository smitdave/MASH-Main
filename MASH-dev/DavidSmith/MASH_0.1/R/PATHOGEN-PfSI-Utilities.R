#################################################################
#
#   MASH
#   R6-ified
#   PfSI Auxiiary
#   Define auxiliary and plotting routines
#   David Smith, Hector Sanchez, Sean Wu
#   May 21, 2017
#
#################################################################


####################################################################################
# PfSI history to continuous time occupancy vector
####################################################################################

#' PfSI: Calculate State Space Occupancy Vector
#'
#' From history output of a \code{\link{HumanPop}}, convert individual histories in to a continuous time occupany vector.
#' The PfSI module has the following state space:
#' * S: susceptible
#' * I: infected
#' * F: fever
#' * P: chemoprophylactic protection
#' * PEvaxx: begin PE vaccination protection
#' * PEwane: end PE vaccination protection
#' * GSvaxx: begin GS vaccination protection
#' * GSwane: end GS vaccination protection
#' @md
#'
#' @param history human event histories from \code{HumanPop$get_History()}
#' @param cpp use Rcpp helper function \code{\link{util_PfSIoneHistory}}
#' @return list
#' @examples
#' util_PfSIHistory()
#' @export
util_PfSIHistory <- function(history, cpp = TRUE){

  # number of humans
  N = length(history)

  # create time bin for each discrete event
  minTime = min(vapply(X = history,FUN = function(x){x$eventT[1]},FUN.VALUE = numeric(1)))
  timeBins = parallel::mclapply(X = history,FUN = function(X,minTime){
    Filter(f = function(x){x > minTime},x = X$eventT)
  },minTime = minTime)
  timeBins = sort(unique(unlist(timeBins)))
  timeBins = c(minTime,timeBins)

  # create empty time series (each element is slice of occupancy vector at that point in time)
  timeSeries = t(vapply(X = timeBins,FUN = function(x){
    util_PfSISlice(time = x)
  },FUN.VALUE = numeric(10)))

  initState = unique(sapply(history,function(x){x$events[1]})) # initial state
  if(length(initState)>1){ # sanity check
    stop("more than one initial state")
  }

  # set initial occupancy vector
  switch(initState,
         "init" = for(i in 1:nrow(timeSeries)){timeSeries[i,"init"] <- N},
         "S" = for(i in 1:nrow(timeSeries)){timeSeries[i,"S"] <- N},
         "I" = for(i in 1:nrow(timeSeries)){timeSeries[i,"I"] <- N},
         "F" = for(i in 1:nrow(timeSeries)){timeSeries[i,"F"] <- N},
         "P" = for(i in 1:nrow(timeSeries)){timeSeries[i,"P"] <- N},
         "PEvaxx" = for(i in 1:nrow(timeSeries)){timeSeries[i,"PEvaxx"] <- N},
         "PEwane" = for(i in 1:nrow(timeSeries)){timeSeries[i,"PEwane"] <- N},
         "GSvaxx" = for(i in 1:nrow(timeSeries)){timeSeries[i,"GSvaxx"] <- N},
         "GSwane" = for(i in 1:nrow(timeSeries)){timeSeries[i,"GSwane"] <- N}
  )

  # fill occupancy vector for each person
  for(ixH in 1:N){
    print(paste0("propagating state for human: ",ixH," of ",N))
    if(cpp){
      util_PfSIoneHistory(historyIxH = history[[ixH]], timeBins = timeBins, timeSeries = timeSeries)
    } else {
      time = history[[ixH]]$eventT
      state = history[[ixH]]$events

      for(ixT in 2:length(time)){
        tIter = which(timeBins >= time[ixT]) # times over which to propagate forward current state
        for(i in tIter){

          timeSeries[i,state[ixT-1]] = timeSeries[i,state[ixT-1]] - 1
          timeSeries[i,state[ixT]] = timeSeries[i,state[ixT]] +1

        }
      }
    }

  }

  return(timeSeries)
}


#' PfSI: Empty Occupancy Vector
#'
#' Make an empty occupancy vector at a single time slice for the \code{PfSI} module. This is a helper function called by \code{\link{util_PfSIHistory}}
#'
#' @param time current time slice
#' @return list
#' @examples
#' util_PfSISlice(time = 0)
util_PfSISlice <- function(time){
  list(
    time = time,
    init = 0,
    S = 0,
    I = 0,
    F = 0,
    P = 0,
    PEvaxx = 0,
    PEwane = 0,
    GSvaxx = 0,
    GSwane = 0
  )
}




####################################################################################
# Plotting
####################################################################################

#' plot_PfSI_oneTrajectory
#'
#' write me
#'
#' @param a parameter
#' @return do something
#' @examples
#' plot_PfSI_oneTrajectory()
plot_PfSIOneTrajectory <- function(ixH, oneHistory, tMax){

  times = oneHistory$eventT[-1]
  events = oneHistory$events[-1]

  # fever
  ixF = which(events == "F")
  if(length(ixF)>0){
    points(x = times[ixF],y = rep(x = ixH,times = length(ixF)),pch=17,cex=0.5,col="red")
  }

  # prophylaxis
  ixP = which(events == "P")
  if(length(ixP)>0){
    points(x = times[ixP],y = rep(x = ixH,times = length(ixP)),pch=16,cex=0.5,col="blue")
  }

  # vaccination
  ixV = grep(pattern = "vaxx$",x = events)
  ixW = grep(pattern = "wane$",x = events)
  if(length(ixV)>0){
    points(x = times[ixV],y = rep(x = ixH,times = length(ixV)),pch=18,cex=0.5,col="darkorange")
  }
  if(length(ixW)>0){
    points(x = times[ixW],y = rep(x = ixH,times = length(ixW)),pch=18,cex=0.5,col="darkorange4")
  }

  events = events[-c(ixF,ixV,ixW)]
  times = times[-c(ixF,ixV,ixW)]

  # state trajectory
  if(length(events) > 0){

    for(i in 1:(length(events))){

      if(i == length(events)){ # final trajectory
        if(events[i] == "I"){
          segments(x0 = times[i],y0 = ixH,x1 = tMax,y1 = ixH,col = "red",lwd = 2)
        }
        if(events[i] == "S"){
          segments(x0 = times[i],y0 = ixH,x1 = tMax,y1 = ixH,col = "grey70",lwd = 2)
        }
        if(events[i] == "P"){
          segments(x0 = times[i],y0 = ixH,x1 = tMax,y1 = ixH,col = "blue",lwd = 2)
        }

      } else { # interior trajectory
        if(events[i] == "I"){
          segments(x0 = times[i],y0 = ixH,x1 = times[i+1],y1 = ixH,col = "red",lwd = 2)
        }
        if(events[i] == "S"){
          segments(x0 = times[i],y0 = ixH,x1 = times[i+1],y1 = ixH,col = "grey70",lwd = 2)
        }
        if(events[i] == "P"){
          segments(x0 = times[i],y0 = ixH,x1 = times[i+1],y1 = ixH,col = "blue",lwd = 2)
        }
      }

    } # end for
  } # end state trajectory

}

#' plot_PfSI
#'
#' write me
#'
#' @param a parameter
#' @return do something
#' @examples
#' plot_PfSI()
#' @export
plot_PfSI <- function(history){

  tMax = max(sapply(X = history,FUN = function(x){max(x$eventT)}))

  plot(1,type="n",xaxt="n",yaxt="n",ylab="Humans",xlab="Time (Years)",xlim=c(0,tMax),ylim=c(0,length(history)))
  ttMax = tMax/365
  axis(side = 1,at = c(0:ttMax)*365,labels = c(0:ttMax))

  for(ixH in 1:length(history)){
    plot_PfSIOneTrajectory(ixH = ixH,oneHistory = history[[ixH]],tMax = tMax)
  }

}
