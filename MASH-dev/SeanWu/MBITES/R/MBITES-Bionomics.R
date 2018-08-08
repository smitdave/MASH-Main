###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Bionomics
#     MBITES Team
#     July 2018
#
###############################################################################


###############################################################################
# mosquito lifespans
###############################################################################

#' Bionomics: Compute Mosquito Lifespans
#'
#' Takes in JSON output parsed into a data.frame object from
#' an MBITES simulation run and returns a data.frame of mosquito lifespans.
#' Mosquitoes that were still alive at the end of simulation are filtered out.
#'
#' @param mosquitos a data.frame of parsed JSON mosquito output
#' @export
Bionomics_lifespan <- function(mosquitos) {

  n = nrow(mosquitos)
  w = rep(NaN,n)
  pb = txtProgressBar(min = 1, max = n, initial = 0)

  for(i in 1:n){
    # filter mosquitoes that were still alive at end of simulation
    if(tail(mosquitos[i,"behavior"][[1]],1) != "E"){
      w[i] = tail(mosquitos[i,"time"][[1]],1) - mosquitos[i,"time"][[1]][1]
    }
    setTxtProgressBar(pb,i)
  }

  w = Filter(Negate(is.nan),w)
  return(data.frame(lifespan=w))
}

###############################################################################
# mosquito blood hosts
###############################################################################

# who: 'human','all','zoo'

#' Bionomics: Compute Number of Blood Meals
#'
#' Takes in JSON output parsed into a data.frame object from
#' an MBITES simulation run.
#' This function returns a data.frame of lifetime blood meals
#' for each mosquito for a chosen host type. It does not filter mosquitos who took no blood meals
#' (that is to say 0s are counted and included in the output).
#' Mosquitoes that were still alive at the end of simulation are filtered out.
#'
#' @param mosquitos a data.frame of parsed JSON mosquito output
#' @param who either 'human', 'zoo', or 'all'
#' @export
Bionomics_humanBloodHost <- function(mosquitos, who = "human"){

  n = nrow(mosquitos)
  w = rep(NaN,n)
  pb = txtProgressBar(min = 1, max = n, initial = 0)

  for(i in 1:n){
    # filter mosquitoes that were still alive at end of simulation
    if(tail(mosquitos[i,"behavior"][[1]],1) != "E"){
      bh = mosquitos[i,"bloodHosts"][[1]]
      switch(who,
             human = {w[i] = length(bh[bh > 0])},
             all = {w[i] = length(bh[bh > 0 | bh == -1])},
             zoo = {w[i] = length(bh[bh == -1])},
             {stop("argument 'who' must be in: 'human', 'all', or 'zoo'")}
             )
    }
    setTxtProgressBar(pb,i)
  }

  w = Filter(Negate(is.nan),w)
  return(data.frame(humanHost=w))
}


###############################################################################
# intervals between blood meals
###############################################################################

#' Bionomics: Compute Interval between Blood Meals
#'
#' Takes in JSON output parsed into a data.frame object from
#' an MBITES simulation run.
#' This function returns a data.frame of intervals between blood meals
#' for each mosquito for the chosen host type.
#' Mosquitoes that were still alive at the end of simulation are filtered out.
#'
#' @param mosquitos a data.frame of parsed JSON mosquito output
#' @param who either 'human', 'zoo', or 'all'
#' @export
Bionomics_bloodIntervals <- function(mosquitos, who = "human"){

  # check args
  if(!(who %in% c("human","all","zoo"))){stop("argument 'who' must be in: 'human', 'all', or 'zoo'")}

  # only want mosquitoes with more than 1 bloodmeal and died before end of the simulation
  filter <- sapply(mosquitos[,"bloodHosts"],function(x){length(x)>1}) & sapply(mosquitos[,"behavior"],function(x){tail(x,1)!="E"})

  # get the intervals
  intervals <- mapply(function(host,time,who){
    # get indices
    ix <- switch(who,
                 human = {which(host>0)},
                 all = {which(host>0 | host==-1)},
                 zoo = {which(host == -1)}
                 )
    # check we haven't indexed nothing
    if(length(ix)==0){
      return(NaN)
    } else {
      return(diff(time[ix]))
    }
  },
  host=mosquitos[which(filter),"bloodHosts"],
  time=mosquitos[which(filter),"timeFeed"],
  MoreArgs = list(who=who),
  USE.NAMES = FALSE)
  # end mapply call

  # clean up and return
  intervals <- unlist(intervals)
  intervals <- Filter(Negate(is.nan),intervals)

  return(data.frame(bmIntervals=intervals))
}


###############################################################################
# human biting rate
###############################################################################

#' Bionomics: Compute Proportion of Blood Meals on Humans
#'
#' Takes in JSON output parsed into a data.frame object from
#' an MBITES simulation run.
#' This function returns a single value, corresponding to \deqn{Q} in
#' Ross-Macdonald parameters.
#' Mosquitoes that were still alive at the end of simulation are filtered out.
#'
#' @param mosquitos a data.frame of parsed JSON mosquito output
#' @export
Bionomics_humanBitingProportion <- function(mosquitos){

  # only want mosquitoes who died before the end of simulation
  filter <- sapply(mosquitos[,"behavior"],function(x){tail(x,1)!="E"})

  # dont count mosquitoes who never took a blood meal
  hbr <- sapply(mosquitos[which(filter),"bloodHosts"],function(x){
    if(length(x)==1 & x[1]==0){
      return(NaN)
    }
    nHuman <- sum(x > 0)
    return(nHuman/length(x))
  })

  hbr <- Filter(Negate(is.nan),hbr)

  return(hbr)
}


#' Bionomics: Compute Blood Feeding Events by Age
#'
#' Takes in JSON output parsed into a data.frame object from
#' an MBITES simulation run.
#' Computes for all processed mosquitoes a vector of ages when successful blood feeding events
#' were recorded.
#' Blood feeding rate by age can be calculated from this output by calculating the density
#' of events as a function of age.
#' Mosquitoes that were still alive at the end of simulation or never oviposited are filtered out.
#'
#' @param mosquitos a data.frame of parsed JSON mosquito output
#' @export
Bionomics_bloodfeedingRate <- function(mosquitos){

  filter <- sapply(mosquitos[,"behavior"],function(x){tail(x,1)!="E"})
  filter <- which(filter)

  feed_list <- vector(mode="list",length=length(filter))

  pb <- txtProgressBar(min = 0,max = length(filter)+2)
  for(i in 1:length(filter)){

    # mosy did blood feed
    if(mosquitos[filter[i],"bloodHosts"][[1]][1] != 0){

      bday <- mosquitos_df[filter[i],"time"][[1]][1]
      feed_ages <- mosquitos_df[filter[i],"timeFeed"][[1]]
      feed_ages <- feed_ages - bday

      feed_list[[i]]$feed_ages <- feed_ages

    # mosquito did not oviposit
    } else {

      feed_list[[i]]$feed_ages <- NaN

    }

    setTxtProgressBar(pb,i)
  }

  # filter out empty elements
  filter_fn <- function(x){
    if(is.nan(x$feed_ages[1])){
      return(FALSE)
    } else {
      return(TRUE)
    }
  }

  feed_list <- Filter(filter_fn,feed_list)
  setTxtProgressBar(pb,i+1)

  # reduce filtered list
  reduce_fn <- function(x,y){
    list(feed_ages=c(x$feed_ages,y$feed_ages))
  }
  feed_list <- Reduce(reduce_fn,feed_list)
  setTxtProgressBar(pb,i+2)

  # return sorted list
  feed_list$feed_ages <- feed_list$feed_ages[order(feed_list$feed_ages)]

  return(feed_list$feed_ages)
}


###############################################################################
# vectorial capacity
###############################################################################

# calculate VC:
# this function actually counts the number of secondary bites and their
# spatial distribution from each human. it is easy to then get VC from it.

#' Bionomics: Compute Vectorial Capacity
#'
#' Takes in JSON output parsed into a data.frame object from
#' an MBITES simulation run.
#' Computes vectorial capacity, as well as its spatial dispersion, from a human-centric (ego-centric)
#' algorithm, described as follows:
#'  1. For each mosquito iterate through all its bites:
#'    * If the bite had a successful blood meal (human to mosquito transmission only occurs during a blood meal)
#'      find all pairs of bites seperated by more than EIP days, where the other bites can be probing events or blood meal events.
#'    * Add these secondary bites to the initial bite's human host's individual vectorial capacity.
#'    * Optionally, record the sites where these secondary bites were dispersed to.
#' Mosquitoes that were still alive at the end of simulation are filtered out.
#' Please note that in order to reconstruct kernels for VC, the distance matrix between sites
#' must be preserved somewhere, as the mosquito only records the index of the site it visited, not the xy coordinates.
#'
#' @return a list where each element corresponds to a human host.
#'         Each host has \code{VC}, which is the total number of secondary bites arising from him or her, and
#'         \code{spatialVC} which is a list of origin/destination pairs tracking dispersion of each initial bite.
#' @param mosquitos a data.frame of parsed JSON mosquito output
#' @param humans a data.frame of parsed JSON human output
#' @param EIP the length of EIP
#' @param spatial compute spatial dispersion of bites or not
#' @export
Bionomics_vectorialCapacity <- function(mosquitos,humans,EIP,spatial=FALSE){

  # number of humans
  nhum <- nrow(humans)

  # get only mosquitoes who took more than 1 blood meal and were dead by end of simulation
  filter <- sapply(mosquitos[,"bloodHosts"],function(x){length(x)>1}) & sapply(mosquitos[,"behavior"],function(x){tail(x,1)!="E"})
  filter <- which(filter)

  # VC
  VC <- replicate(n = nhum,expr = {
    list(
      VC = 0, # their vectorial capacity
      spatialVC = list() # the distribution of secondary bites (on sites)
    )
  },simplify = FALSE)

  # iterate over mosquitoes
  pb <- txtProgressBar(min = 1,max = length(filter))
  for(i in filter){

    bloodHosts <- mosquitos[i,"bloodHosts"][[1]]
    timeFeed <- mosquitos[i,"timeFeed"][[1]]
    siteFeed <- mosquitos[i,"siteFeed"][[1]]
    probeAndFeed <- mosquitos[i,"probeAndFeed"][[1]]

    # iterate over bites
    while(length(timeFeed)>1){

      # only if the bite was a probing AND feeding event
      # (feeding needs to occur for human -> mosy transmission)
      if(probeAndFeed[1]){

        # get indices of secondary bites
        pairTimes <- diff(timeFeed)
        secondaryBites <- which(pairTimes>EIP)+1

        # only if there were secondary bites arising from this bite
        if(length(secondaryBites)>0){

          # add to the primary host's VC
          VC[[bloodHosts[1]]]$VC = VC[[bloodHosts[1]]]$VC + length(secondaryBites)

          # get spatial distribution of bites
          if(spatial){
            spatDist <- list(origin=siteFeed[1],dest=siteFeed[secondaryBites])
            len <- length(VC[[bloodHosts[1]]]$spatialVC)
            VC[[bloodHosts[1]]]$spatialVC[[len+1]] = spatDist
          }
        }
      } # finish iterating over bites

      # take off the first bite
      bloodHosts <- bloodHosts[-1]
      timeFeed <- timeFeed[-1]
      siteFeed <- siteFeed[-1]
      probeAndFeed <- probeAndFeed[-1]
    }

    setTxtProgressBar(pb,i)
  } # finish iterating over mosquitoes

  return(VC)
}


###############################################################################
# egg production & oviposition
###############################################################################

#' Bionomics: Compute Lifetime Egg Production and Spatial Dispersion
#'
#' Takes in JSON output parsed into a data.frame object from
#' an MBITES simulation run.
#' Computes lifetime egg production, as well as its spatial dispersion.
#' Mosquitoes that were still alive at the end of simulation are filtered out.
#' Please note that in order to reconstruct kernels for egg dispersion, the distance matrix between sites
#' must be preserved somewhere, as the mosquito only records the index of the site it visited, not the xy coordinates.
#'
#' @param mosquitos a data.frame of parsed JSON mosquito output
#' @param spatial compute spatial dispersion of eggs or not
#' @export
Bionomics_lifetimeOviposition <- function(mosquitos, spatial=FALSE){

  filter <- sapply(mosquitos[,"behavior"],function(x){tail(x,1)!="E"})
  filter <- which(filter)

  out <- rep(0,length(filter))
  if(spatial){
    dispersion <- replicate(length(filter),{list(natal=NaN,dest=NaN)},simplify=FALSE)
  }

  pb <- txtProgressBar(min = 0,max = length(filter))
  for(i in 1:length(filter)){
    out[i] <- sum(mosquitos[filter[i],"ovipositionBatchSize"][[1]])
    if(spatial & out[i]>0){
      dispersion[[i]]$natal <- mosquitos_df[filter[i],"sites"][[1]][1]
      dispersion[[i]]$dest <- mosquitos_df[filter[i],"ovipositionSites"][[1]]
    }
    setTxtProgressBar(pb,i)
  }

  if(spatial){
    return(list(
      dispersion=dispersion,
      lifetime=out
    ))
  } else {
    return(out)
  }
}

#' Bionomics: Compute Oviposition Intervals and Number of Successful Oviposition Events
#'
#' Takes in JSON output parsed into a data.frame object from
#' an MBITES simulation run.
#' Computes the interval between successful oviposition events and the total number of
#' successful oviposition events in a mosquito's lifespan (0s are included, that is to say if a mosquito
#' never successfully oviposited it will contribute a 0 to the vector).
#' Mosquitoes that were still alive at the end of simulation are filtered out.
#'
#' @param mosquitos a data.frame of parsed JSON mosquito output
#' @export
Bionomics_ovipositionInterval <- function(mosquitos){

  filter <- sapply(mosquitos[,"behavior"],function(x){tail(x,1)!="E"})
  filter <- which(filter)

  numOviposit <- rep(0,length(filter))
  interval <- vector(mode="list",length=length(filter))

  pb <- txtProgressBar(min = 0,max = length(filter))
  for(i in 1:length(filter)){

    numOviposit[i] <- length(mosquitos_df[filter[i],"ovipositionTimes"][[1]])

    if(numOviposit[i]>1){
      interval[[i]] <- diff(mosquitos_df[filter[i],"ovipositionTimes"][[1]])
    }
    setTxtProgressBar(pb,i)
  }

  interval <- Filter(Negate(is.null),interval)
  interval <- do.call(c,interval)

  return(
    list(numOviposit=numOviposit,interval=interval)
  )
}

#' Bionomics: Compute Batch Sizes (Oviposition Events) by Age
#'
#' Takes in JSON output parsed into a data.frame object from
#' an MBITES simulation run.
#' Computes for all processed mosquitoes a vector of ages when successful oviposition events
#' were recorded and a vector of egg batch sizes sorted by ages (vectors are the same length).
#' Oviposition rate by age can be calculated from this output by calculating the density
#' of events as a function of age.
#' Mosquitoes that were still alive at the end of simulation or never oviposited are filtered out.
#'
#' @param mosquitos a data.frame of parsed JSON mosquito output
#' @export
Bionomics_ovipositionRate <- function(mosquitos){

  filter <- sapply(mosquitos[,"behavior"],function(x){tail(x,1)!="E"})
  filter <- which(filter)

  age_batch_list <- vector(mode="list",length=length(filter))

  pb <- txtProgressBar(min = 0,max = length(filter)+2)
  for(i in 1:length(filter)){

    # mosy did oviposit
    if(mosquitos[filter[i],"ovipositionTimes"][[1]][1] > 0){

      bday <- mosquitos_df[filter[i],"time"][[1]][1]
      batch_sizes <-  mosquitos_df[filter[i],"ovipositionBatchSize"][[1]]
      batch_times <- mosquitos_df[filter[i],"ovipositionTimes"][[1]]
      batch_times <- batch_times - bday

      age_batch_list[[i]]$batches <- batch_sizes
      age_batch_list[[i]]$ages <- batch_times

    # mosquito did not oviposit
    } else {
      # how to deal with these mosquitoes?????
      age_batch_list[[i]]$batches <- NaN
      age_batch_list[[i]]$ages <- NaN

    }

    setTxtProgressBar(pb,i)
  }

  # filter out empty elements
  filter_fn <- function(x){
    if(is.nan(x$batches[1]) & is.nan(x$ages[1])){
      return(FALSE)
    } else {
      return(TRUE)
    }
  }

  age_batch_list <- Filter(filter_fn,age_batch_list)
  setTxtProgressBar(pb,i+1)

  # reduce filtered list
  reduce_fn <- function(x,y){
    list(batches=c(x$batches,y$batches),ages=c(x$ages,y$ages))
  }
  age_batch_list <- Reduce(reduce_fn,age_batch_list)
  setTxtProgressBar(pb,i+2)

  # return sorted list
  age_batch_list$batches <- age_batch_list$batches[order(age_batch_list$ages)]
  age_batch_list$ages <- age_batch_list$ages[order(age_batch_list$ages)]

  return(age_batch_list)
}
