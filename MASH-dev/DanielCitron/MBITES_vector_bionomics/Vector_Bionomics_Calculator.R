###
# Vector_Bionomics_Calculator.R
#
# Daniel T. Citron, 5/25/18
#
# Scripts for analyzing the JSON output from MBITES event loggers
# Want to use the simulation output to calculate a variety of bionomic data, including
# 1. Vector lifetime distribution
# 2. Vector blood feeding interval distribution
# 3. Vector blood feeding events per lifetime distribution
# 4. Vectorial capacity distribution: 
#     number of secondary bites that arise following from a single host on a single day
#
###
# Example function call:
#
# bionomics.data <- combine.bionomics.data(filename, EIP=2) # read JSON data into data.table format
# 
# VC.distn <- vc.distribution.data(bionomics.data) # Calculate Vectorial Capacity
# hist(VC.distn, breaks = c(0:(ceiling(max(VC.distn))+1))-.5)
# 
###

require(data.table)
require(jsonlite)

### 
# preprocess.vector.data
# A function for processing a single mosquito's events from JSON data,
# to be passed on to vector.bionomics()
###
# Inputs: row.number - row number index in JSON data - integer
#         mosquito.biting.data - data, read in from JSON using fromJSON() in the jsonlite R package
# Outputs: data.table containing prepared data relevant to bionomics
#             mosquito ID: ID of mosquito- integer
#             birth.time: time of birth - numeric
#             death.time: time of death - numeric
#             timeFeed: times of primary bites - numeric
#             siteFeed: sites of primary bites - integer
#             bloodHost: bitten host ID - integer
#             probeAndFeed: whether or not the blood feeding attempt was successful - Boolean
preprocess.vector.data <- function(row.number, mosquito.biting.data = mosquito.biting.data){
  mos.dat.row <- mosquito.biting.data[row.number,]
  event.times <- unlist(mos.dat.row$time)
  row.table <- data.table(
    mosID = unlist(mos.dat.row$id),
    birth.time = event.times[1],
    death.time = event.times[length(event.times)] # last behavior should always be "D" for death
  )
  blood.feeding <- data.table(timeFeed = unlist(mos.dat.row$timeFeed),
                              siteFeed = unlist(mos.dat.row$siteFeed),
                              #siteFeed = unlist(mos.dat.row$sites)[1:length(unlist(mos.dat.row$timeFeed))], # the kluge we were using before
                              bloodHost = unlist(mos.dat.row$bloodHosts),
                              probeAndFeed = unlist(mos.dat.row$probeAndFeed)
  )
  return(cbind(row.table,blood.feeding))
}

### 
# count.secondary.bites
# A function for counting the number of secondary bites attributed to each primary bite
###
# Inputs: i - primary bite index - integer
#         bite.times - list of bite times - list of numerics
#         EIP - Entomological Inoculation Period - integer
# Outputs: number of secondary bites that occur (EIP) after primary bite time - integer
count.secondary.bites <- function(i, bite.times, EIP){
  return(length(which(bite.times[i:length(bite.times)] - bite.times[i] > EIP)))
}

###
# vector.bionomics
# A function for calculating a variety of vector bionomics, based on simulation output
###
# Inputs: mos.data: formatted output from preprocess.vector.data - data.table
#         EIP: Entomological Inoculation Period - integer
# Outputs: data.table containing bionomic-relevant data
#             mosquito ID: ID of mosquito- integer
#             lifetime: life time interval of mosquito - numeric
#             bite.times: times of primary bites - numeric
#             bite.hosts: host ID of bitten hosts - integer
#             bite.sites: site ID where bite occurred - integer
#             feeding.intervals: time intervals until next bite - numeric
#             VC.bites: counts of infectious secondary bites arising from each primary bite - integer
vector.bionomics <- function(mos.data, EIP = EIP){
  ## Mosquito lifetime
  lifetime <- mos.data$death.time[1] - mos.data$birth.time[1]
  ## Blood feeding time intervals
  successes <- mos.data[probeAndFeed == TRUE,] # only count successful feeding events
  feeding.times <- successes$timeFeed
  nBites <- length(feeding.times)
  if (nBites > 1){
    feed.intv <- feeding.times[c(2:length(feeding.times))] - feeding.times[c(1:(length(feeding.times)-1))]
  } else {
    feed.intv <- NA 
  }
  if (length(feed.intv) < nBites){
    feed.intv <- c(feed.intv, NA)
  }
  ## Host and location where bite occurred
  
  ## VC calculation
  if (nBites > 0){
    secondary.bites <- sapply(X=1:nBites, FUN = count.secondary.bites, bite.times = feeding.times, EIP = EIP)
  } else {
    secondary.bites <- NA
  }
  if (length(feeding.times)==0){ feeding.times <- NA} # for output, in case no feeding events happened
  return(data.table(
    mosID = mos.data$mosID[1],
    lifetime = lifetime,
    bite.times = feeding.times,
    bite.hosts = mos.data$bloodHost,
    bite.sites = mos.data$siteFeed,
    feeding.intervals = feed.intv,
    VC.bites = secondary.bites)
  )
}

### 
# vector.bionomics.processor
# Putting it all together: pipe the preprocessed output directly into the vector bionomics calculatr
###
# Inputs: row.number - row number index in JSON data - integer
#         mosquito.biting.data - data, read in from JSON using fromJSON() in the jsonlite R package
#         EIP: Entomological Inoculation Period - integer
# Outputs: data.table containing bionomic-relevant data
#             mosquito ID: ID of mosquito- integer
#             lifetime: life time interval of mosquito - numeric
#             bite.times: times of primary bites - numeric
#             bite.hosts: host ID of bitten hosts - integer
#             bite.sites: site ID where bite occurred - integer
#             feeding.intervals: time intervals until next bite - numeric
#             VC.bites: counts of infectious secondary bites arising from each primary bite - integer
vector.bionomics.processor <- function(row.number,
                                       mosquito.biting.data=mosquito.biting.data,
                                       EIP=EIP){
  mos.data = preprocess.vector.data(row.number, mosquito.biting.data)
  return(vector.bionomics(mos.data, EIP))
}

###
# combine.bionomics.data
# Read in and format all bionomics data from an MBITES mosquito biting data file
# Combines and formats the output data from every mosquito from an MBITES simulation
###
# Inputs: file.path.name - File path name - string
#         EIP - Entomological Incubation Period - integer
# Outputs: data.table containing bionomic-relevant data
#             mosquito ID: ID of mosquito- integer
#             lifetime: life time interval of mosquito - numeric
#             bite.times: times of primary bites - numeric
#             bite.hosts: host ID of bitten hosts - integer
#             bite.sites: site ID where bite occurred - integer
#             feeding.intervals: time intervals until next bite - numeric
#             VC.bites: counts of infectious secondary bites arising from each primary bite - integer
# Example call:
# bionomics.data <- combine.bionomics.data(filename, EIP = 11)
###
combine.bionomics.data <- function(file.path.name, EIP = 11){
  # read in a JSON file
  mosquito.biting.data <- fromJSON(file.path.name)
  # number of rows in the JSON file
  rows <- nrow(mosquito.biting.data)-1
  bionomics.data <- rbindlist(lapply(X=1:rows, vector.bionomics.processor,
                                     mosquito.biting.data=mosquito.biting.data, EIP=EIP))
  return(bionomics.data)
}



### 
# lifetimes.distribution.data
# Calculate histogram data of lifetimes
# Can be used as an input on a histogram to plot and represent statistics
###
# Inputs: bionomics.data, output from combine.bionomics.data
# Outputs: lifetimes.data - array of mosquito lifetimes - numeric vector
#
# Example usage:
# lifetimes.data <- lifetimes.distribution.data(bionomics.data)
# hist(lifetimes.data, breaks = c(0:ceiling(max(lifetimes.data)))-.5) # plot a histogram
###
lifetimes.distribution.data <- function(bionomics.data){
  lifetimes.data <- bionomics.data[, .SD[1], by = "mosID"]$lifetime
  return(lifetimes.data)
}


### 
# feed.intervals.distribution.data
# Calculate histogram data of blood feeding intervals
# Can be used as an input on a histogram to plot and represent statistics
###
# Inputs: bionomics.data, output from combine.bionomics.data
# Outputs: feedint.data - array of intervals between blood feeding events - numeric vector
#
# Example usage:
# feedtimes.data <- feed.intervals.distribution.data(bionomics.data)
# hist(feedtimes.data, breaks = c(0:ceiling(max(feedtimes.distn)))-.5) # plot a histogram
###
feed.intervals.distribution.data <- function(bionomics.data){
  feedtimes.data <- bionomics.data[!is.na(feeding.intervals)]$feeding.intervals
  return(feedtimes.data)
}


### 
# biting.distribution.data
# Calculate histogram data of bites per mosquito lifetime
# Can be used as an input on a histogram to plot and represent statistics
###
# Inputs: bionomics.data, output from combine.bionomics.data
# Outputs: numbites.data - array of number of biting events per lifetime - numeric vector
#
# Example usage:
# numbites.data <- biting.distribution.data(bionomics.data)
# hist(numbites.data, breaks = c(0:ceiling(max(feedtimes.distn)))-.5) # plot a histogram
###
biting.distribution.data <- function(bionomics.data){
  # Count number of bites per mosquito:
  numbites.data <- bionomics.data[!is.na(bite.times)][,c("mosID","bite.times")][, .N, by = "mosID"]
  # No bite is listed with NA, so we can add zero-bite mosquitoes as zeroes...
  numbites.data <- rbind(numbites.data, data.table(mosID = bionomics.data[is.na(bite.times)][,c("mosID")]$mosID, N = 0))$N
}


### 
# vc.distribution.data
# Calculate histogram data for vectorial capcity,
#   specifically the number of secondary bites arising from each primary biting event on each host
#   NB: This does not require hosts to be infected or infectious, this is only counting the number 
#       of pairs of events that could potentially lead to parasite transmission.
#   NB: This can be extended to integrate over all hosts in a particular patch, or over all days,
#       depending on which specific definition of VC one ascribes to.
#   NB: The EIP was specified earlier, when bionomics.data was compiled from the raw simulation output
# Can be used as an input on a histogram to plot and represent statistics
###
# Inputs: bionomics.data, output from combine.bionomics.data
# Outputs: vc.data - array of number of 2ndary bites per host per day - numeric vector
#
# Example usage:
# vc.data <- vc.distribution.data(bionomics.data)
# hist(vc.data, breaks = c(0:ceiling(max(feedtimes.distn)))-.5) # plot a histogram
###
vc.distribution.data <- function(bionomics.data){
  vc.data <- bionomics.data[!is.na(VC.bites)][, sum(VC.bites), by=.(bite.times, bite.hosts)]$V1
  return(vc.data)
}



###
# Left to do: 
# Need to accommodate death event logging (pull, try running again with pretty data)
# Need to make sure feeding *site* is marked in the json file, along with host
# Need to accommodate both pretty and mini-formatted data - allow for streaming JSON in (pull, try running without pretty data)