require(data.table)

###
# Vector_Bionomics_Calculator.R
#
# Daniel T. Citron, 5/25/18
#
# Scripts for analyzing the JSON output from MBITES event loggers
# Want to use the simulation output to calculate a variety of bionomic data
###


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
  event.times <- unlist(mos.dat.row$time) # this will have to be altered as soon as we have "D" events...
  #behaviors <- unlist(mos.dat.row$behavior)
  row.table <- data.table(
    mosID = unlist(mos.dat.row$id),
    birth.time = event.times[1],
    death.time = event.times[length(event.times)]
  )
  blood.feeding <- data.table(timeFeed = unlist(mos.dat.row$timeFeed),
                              #siteFeed = unlist(mos.dat.row.ex1$siteFeed), #### FIX ME
                              siteFeed = unlist(mos.dat.row$sites)[1:length(unlist(mos.dat.row$timeFeed))],
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

vector.bionomics.processor(52, mosquito.biting.data, EIP)

# To do: 
# Need to accommodate death event logging (pull, try running again with pretty data)
# Need to accommodate both pretty and mini-formatted data - allow for streaming JSON in (pull, try running without pretty data)

# If necessary: can update the VC calculator to acccomodate differentiating between hosts
# Right now we are assuming that each mosquito bites a different host, which is wrong.