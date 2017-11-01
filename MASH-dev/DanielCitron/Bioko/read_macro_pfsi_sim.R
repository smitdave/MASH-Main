library(plyr)
library(data.table)
library(ggplot2)


# A function that outputs a data frame version of the xth row of pfsihistory
listcon <- function(x, pfsihistory) {
  df <- data.frame(humanID=x, eventT=pfsihistory[x][[1]]$eventT, events=pfsihistory[x][[1]]$events)
  return(df)
}

# Functiona thatsends NA values for a variable X to 0
# This goes inside sip transitions, since that is what was creating NAs from before...
na2zero <- function(x){
  # send x to 0 if x is NA
  if (is.na(x)){
    return(0)
  } else{
    return(x)
  }
}

# A function that takes a row from our table of all events at each discrete time step
# and turns those events into transitions
sip.transitions <- function(x, l.table){
  l.table.row <- l.table[x,]
  if (x >1){
    ns <- na2zero(l.table.row["S"]) # S events
    ni <- na2zero(l.table.row["I"]) # I events
    np <- na2zero(l.table.row["P"]) # P events
    np2s <- na2zero(l.table.row["P2S"]) # P2S events
    nf <- na2zero(l.table.row["F"]) # F events
    nvx <- na2zero(l.table.row["PEvaxx"]) # PEvaxx events
    nvw <- na2zero(l.table.row["PEwane"]) # PEwane events
    s <- ns - ni #- np + np2s # if we track the protected population separate from the susceptible population <- kludge
    i <- -ns + ni
    p <- np - np2s
    f <- nf
    vx <- nvx
    vw <- nvw
  } else {
    s <- l.table.row["S"] # S events
    i <- l.table.row["I"] # I events
    p <- 0
    f <- 0
    vx <- 0
    vw <- 0
  }
  # time step
  t <- rownames(l.table)[x]
  out <- as.integer(c(t,s,i,p,f,vx,vw))
  return(out)
}

# A function that assigns event times (ev_time) to the nearest next discrete event time
grid.time <- function(ev_time, t_grid){
  t_index <- findInterval(ev_time, t_grid, left.open = TRUE) + 1
  return(t_grid[t_index])
}

# A function that inputs pfsiHistory and outputs a table of prevalence curves and fever/vaccination-related events over time
pop_SI <- function(pfsiHist, t_grid, patchIDs=NULL){
  # Input a full pfsiHist, output from running the simulation
  # Output a table showing the SI curves, along with the fever and vaccination-related events

  # patchIDs is a vector of integer IDs for a subset of people in the simulation
  # if patchIDs is not specified, then we calculate the SI curves for the full population
  if (is.null(patchIDs)){
    # extract events from all individuals included in the population
    l <- ldply(lapply(X = 1:length(pfsiHist), FUN = listcon, pfsihistory = pfsiHist), data.frame)
  } else {
    # extract events only from the individuals specified in patchIDs
    l <- ldply(lapply(X = patchIDs, FUN = listcon, pfsihistory = pfsiHist), data.frame)
  }
  # convert to a data table
  l.dt <- as.data.table(l)
  setkey(l.dt, events)
  # remove all inits
  l.dt <- l.dt[l.dt$events!="init"]
  l.dt$events <- droplevels(l.dt$events)
  # set coarse-grained time step for each event, creates a new column
  h <- sapply(l.dt$eventT, grid.time, t_grid = t_grid)
  l.dt[, gridT := h]
  # order by time
  l.dt <- l.dt[order(l.dt$eventT)]
  # now we can use table to find the number of each type of event at each time step:
  l.table <- table(l.dt[, c(4,3), with = FALSE])
  # Now, convert transitions logging into deltas - how S and I change over time
  h <- sapply(1:nrow(l.table), sip.transitions, l.table = l.table)
  # Convert back to a data.frame, and convert transition deltas into SI numbers:
  tindexes <- findInterval(t_grid, h[1,])
  prev.traj <- data.frame(t = t_grid, # names of timesteps
                          S = cumsum(h[2,])[tindexes], I = cumsum(h[3,])[tindexes], P = cumsum(h[4,])[tindexes],# SI curves
                          F = rep(0, length(t_grid)), PEvaxx = rep(0, length(t_grid)), PEwane = rep(0, length(t_grid))
                          #F = h[5,][tindexes], PEvaxx = h[6,][tindexes], PEwane = h[7,][tindexes] # discrete events that occur
  )
  # Add in the discrete events
  prev.traj$F[findInterval(h[1,], t_grid)] <- prev.traj$F[findInterval(h[1,], t_grid)] + h[5,]
  prev.traj$PEvaxx[findInterval(h[1,], t_grid)] <- prev.traj$PEvaxx[findInterval(h[1,], t_grid)] + h[6,]
  prev.traj$PEwane[findInterval(h[1,], t_grid)] <- prev.traj$PEwane[findInterval(h[1,], t_grid)] + h[7,]
  # Remove NA values:
  prev.traj[is.na(prev.traj)] <- 0
  return(prev.traj)
}
