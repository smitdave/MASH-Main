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
#' This function ignores information on location.
#'
#' @param h_inf a \code{data.frame} read in by \code{read.csv(file = path,stringsAsFactors = FALSE)}
#'
#' @export
pfsi_human_output <- function(h_inf){

  time <- h_inf$time
  state0 <- h_inf$state0
  state1 <- h_inf$state1

  t_sort <- order(time)

  time <- time[t_sort]
  state0 <- state0[t_sort]
  state1 <- state1[t_sort]

  tmax <- ceiling(tail(time,1))

  state_init <- c("S"=0,"I"=0,"P"=0,"F"=0,"PEvaxx"=0,"GSvaxx"=0,"PEwane"=0,"GSwane"=0)

  i <- 1
  while(time[i] <= 0){
    state_init[state1[i]] <- state_init[state1[i]] + 1
    i <- i + 1
  }

  # fill the rest of the matrix with these states
  states <- matrix(0,byrow = T,nrow=tmax+1,ncol = 8,dimnames = list(0:tmax,c("S","I","P","F","PEvaxx","GSvaxx","PEwane","GSwane")))
  states[1,] <- state_init

  time <- time[i:length(time)]
  state0 <- state0[i:length(state0)]
  state1 <- state1[i:length(state1)]

  # aggregate over days
  for(i in 1:(nrow(states)-1)){

    # to update for this time step, modify the state from the previous step
    states[(i+1),c("S","I","P")] <- states[i,c("S","I","P")]

    # get the stuff that happens in this time step
    ix <- which(time < i & time >= (i-1))

    # events occured
    if(length(ix) > 0){

      # check for point events
      ix_p <- which(state0[ix] == "_")

      if(length(ix_p) > 0){
        for(j in ix_p){
          states[(i+1),state1[ix][j]] <- states[(i+1),state1[ix][j]] + 1
        }
      }

      # check for jump events
      ix_t <- which(state0[ix] != "_")

      if(length(ix_t) > 0){
        for(j in ix_t){

          states[(i+1),state0[ix][j]] <- states[(i+1),state0[ix][j]] - 1
          states[(i+1),state1[ix][j]] <- states[(i+1),state1[ix][j]] + 1

        }
      }
    }
  }

  return(states)
}
