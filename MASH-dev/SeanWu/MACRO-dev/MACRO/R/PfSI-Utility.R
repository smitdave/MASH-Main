################################################################################
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   PfSI - Process Output
#
#   Sean Wu
#   January 2019
#
################################################################################

#' PfSI: Process Human Infection Output
#'
#' After reading in the .csv output for the \code{human_inf} logging, bin humans
#' by states with a resolution of one day for analysis and plotting.
#' This function ignores information on location. Each time bin contains
#' all events that occured from the time label on the previous bin until
#' right before the label on that bin.
#'
#' @param h_inf a \code{data.frame} read in by \code{read.csv(file = path,stringsAsFactors = FALSE)}
#' @param tmax the maximum runtime of the simulation that is being processed
#' @param dx size of time bin (must be an integer greater than 0)
#' @param pb if \code{TRUE} show a progress bar
#'
#' @export
#' 
pfsi_human_output <- function(h_inf, tmax, dx = 1, pb = TRUE){
  
  time <- h_inf$time
  state0 <- h_inf$state0
  state1 <- h_inf$state1
  
  t_sort <- order(time)
  
  time <- time[t_sort]
  state0 <- state0[t_sort]
  state1 <- state1[t_sort]
  
  # tmax <- ceiling(tail(time,1))
  
  state_init <- c("S"=0,"I"=0,"P"=0,"F"=0,"PEvaxx"=0,"GSvaxx"=0,"PEwane"=0,"GSwane"=0)
  
  i <- 1
  while(time[i] <= 0){
    state_init[state1[i]] <- state_init[state1[i]] + 1
    i <- i + 1
  }
  
  # vector of time bins
  # time_dx <- c(0,seq(from=1,to=tmax+1,by=dx))
  time_dx <- c(0,seq(from=dx,to=(tmax+dx),by=dx))
  
  # fill the rest of the matrix with these states
  states <- matrix(0,byrow = T,nrow=length(time_dx),ncol = 9,dimnames = list(time_dx,c("time","S","I","P","F","PEvaxx","GSvaxx","PEwane","GSwane")))
  states[1,2:ncol(states)] <- state_init
  states[,1] <- time_dx
  
  time <- time[i:length(time)]
  state0 <- state0[i:length(state0)]
  state1 <- state1[i:length(state1)]
  
  # aggregate over days
  n <- nrow(states)
  if(pb){
    pbx <- txtProgressBar(min = 1,max = n)
  }
  for(i in 2:n){
    
    # to update for this time step, modify the state from the previous step
    states[i,c("S","I","P")] <- states[(i-1),c("S","I","P")]
    
    # get the stuff that happens in this time bin
    ix <- which(time < time_dx[i] & time >= time_dx[i-1])
    
    # events occured
    if(length(ix) > 0){
      
      # check for point events
      ix_p <- which(state0[ix] == "_")
      
      if(length(ix_p) > 0){
        for(j in ix_p){
          states[i,state1[ix][j]] <- states[i,state1[ix][j]] + 1
        }
      }
      
      # check for jump events
      ix_t <- which(state0[ix] != "_")
      
      if(length(ix_t) > 0){
        for(j in ix_t){
          
          states[i,state0[ix][j]] <- states[i,state0[ix][j]] - 1
          states[i,state1[ix][j]] <- states[i,state1[ix][j]] + 1
          
        }
      }
    }
    if(pb){
      setTxtProgressBar(pb = pbx,value = i)
    }
  }
  
  return(states)
}

# pfsi_human_output <- function(h_inf, tmax, dx = 1, pb = TRUE){
# 
#   time <- h_inf$time
#   state0 <- h_inf$state0
#   state1 <- h_inf$state1
# 
#   t_sort <- order(time)
# 
#   time <- time[t_sort]
#   state0 <- state0[t_sort]
#   state1 <- state1[t_sort]
# 
#   # tmax <- ceiling(tail(time,1))
# 
#   state_init <- c("S"=0,"I"=0,"P"=0,"F"=0,"PEvaxx"=0,"GSvaxx"=0,"PEwane"=0,"GSwane"=0)
# 
#   i <- 1
#   while(time[i] <= 0){
#     state_init[state1[i]] <- state_init[state1[i]] + 1
#     i <- i + 1
#   }
# 
#   # vector of time bins
#   time_dx <- c(0,seq(from=1,to=tmax+1,by=dx))
# 
#   # fill the rest of the matrix with these states
#   states <- matrix(0,byrow = T,nrow=length(time_dx),ncol = 9,dimnames = list(time_dx,c("time","S","I","P","F","PEvaxx","GSvaxx","PEwane","GSwane")))
#   states[1,2:ncol(states)] <- state_init
#   states[,1] <- time_dx
# 
#   time <- time[i:length(time)]
#   state0 <- state0[i:length(state0)]
#   state1 <- state1[i:length(state1)]
# 
#   # aggregate over days
#   n <- nrow(states)-1
#   if(pb){
#     pbx <- txtProgressBar(min = 1,max = n)
#   }
#   for(i in 1:n){
# 
#     # to update for this time step, modify the state from the previous step
#     states[(i+1),c("S","I","P")] <- states[i,c("S","I","P")]
# 
#     # get the stuff that happens in this time bin
#     ix <- which(time < time_dx[i] & time >= time_dx[i-1])
# 
#     # events occured
#     if(length(ix) > 0){
# 
#       # check for point events
#       ix_p <- which(state0[ix] == "_")
# 
#       if(length(ix_p) > 0){
#         for(j in ix_p){
#           states[(i+1),state1[ix][j]] <- states[(i+1),state1[ix][j]] + 1
#         }
#       }
# 
#       # check for jump events
#       ix_t <- which(state0[ix] != "_")
# 
#       if(length(ix_t) > 0){
#         for(j in ix_t){
# 
#           states[(i+1),state0[ix][j]] <- states[(i+1),state0[ix][j]] - 1
#           states[(i+1),state1[ix][j]] <- states[(i+1),state1[ix][j]] + 1
# 
#         }
#       }
#     }
#     if(pb){
#       setTxtProgressBar(pb = pbx,value = i)
#     }
#   }
# 
#   return(states)
# }
