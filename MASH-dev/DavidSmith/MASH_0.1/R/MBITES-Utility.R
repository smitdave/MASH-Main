#################################################################
#
#   MASH
#   M-BITES
#   Utilities for M-BITES
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   August 10, 2017
#
#################################################################

#################################################################
#   Empirical Oviposition Probability
#################################################################

#' M-BITES Utility: Calculate Empirical Probability of Oviposition at \code{\link{AquaticSite}}
#'
#' Return vector of scaled frequencies of oviposition at aquatic habitats.
#'
#' @param history list of mosquito histories
#' @param nAqua number of aquatic habitats
#' @md
#' @export
ovipositionEq_utility <- function(history, nAqua){

  aquaVec = rep(0L,times=nAqua)

  aquaVecAll = parallel::mclapply(X = history,FUN = ovipositionEq_oneHistory_utility, aquaVec = aquaVec)
  aquaVec = Reduce(f = "+",x = aquaVecAll)

  return(aquaVec/sum(aquaVec))
}

#' M-BITES Utility: Calculate Counts of Oviposition at \code{\link{AquaticSite}}
#'
#' Return vector of counts recording number of ovipositions at each site on landscape. Called by \code{\link{ovipositionEq_utility}}
#'
#' @param oneHistory single mosquito history
#' @param aquaVec vector of length equal to number of aquatic sites on \code{\link{Landscape}}
#' @md
#' @export
ovipositionEq_oneHistory_utility <- function(oneHistory, aquaVec){

  with(oneHistory,{
    if(!"O" %in% stateH){
      return(aquaVec)
    } else {
      oviIx = which(stateH == "O")
      for(ix in oviIx){
        aquaVec[ixH[oviIx]] = aquaVec[ixH[oviIx]] + 1L
      }
      return(aquaVec)
    }
  })
}


#################################################################
#   Calculate RM Parameters from Histories
#################################################################

#' M-BITES Utility: Import JSON Mosquito Histories
#'
#' From methods which write mosquito bionomics and histories to JSON, reimport as R lists.
#'
#' @param directory directory in which output was stored (see \code{MicroTile$get_directory})
#' @param string search string for \code{\link{grep}} to read files (can be "cohort" or "history")
#' @md
#' @export
importMicroMosquitoHistory_utility <- function(directory, string){

  # find what to import
  directory = tile$get_directory()
  dirFiles = system(command = paste0("ls ",directory,"MOSQUITO/"),intern = TRUE)
  readFiles = grep(pattern = "cohort",x = dirFiles,ignore.case = TRUE)

  # import JSON
  fileOut = parallel::mclapply(X = dirFiles[readFiles],FUN = function(x){
    jsonlite::fromJSON(txt = paste0(directory,"MOSQUITO/",x),flatten = TRUE)
  },mc.cores = parallel::detectCores()-2)

  # parse and return as list
  fileOut = Reduce(f = c,x = fileOut)
  fileOut = unname(fileOut)
  return(fileOut)
}

#' M-BITES Utility: Calculate Ross-MacDonald Bionomics from Mosquito Histories
#'
#' From \code{MicroTile$simCohort} or microsimulation runs, calculate basic mosquito bionomics.
#'
#' @param history list of mosquito histories, if data exists in JSON format see \code{\link{importMicroMosquitoHistory_utility}} to import
#' @md
#' @export
calculateBionomics_utility <- function(history){

  # output
  allBloodmeals = humanBloodmeals = lifespans = meanEggBatches = totalEggBatches = rep(NA,length=length(history))
  allBloodmealIntervals = humanBloodmealIntervals = rep(NA,length=length(history)*7)

  # calculate bionomics
  i = j = k = 1
  for(ix in 1:length(history)){

    # number of bloodmeals
    nBloodmeals = length(history[[ix]]$bionomics_bmInt)
    if(nBloodmeals > 0){
      allBloodmealIntervals[j:(j+nBloodmeals-1)] = history[[ix]]$bionomics_bmInt
      j = j + nBloodmeals
    }

    # number of human bloodmeals
    nHumanBloodmeals = length(history[[ix]]$bionomics_bmIntH)
    if(nHumanBloodmeals > 0){
      humanBloodmealIntervals[k:(k+nHumanBloodmeals-1)] = history[[ix]]$bionomics_bmIntH
      k = k + nHumanBloodmeals
    }

    allBloodmeals[i] = history[[ix]]$feedAllH
    humanBloodmeals[i] = history[[ix]]$feedHumanH
    lifespans[i] = history[[ix]]$bionomics_lifespan
    meanEggBatches[i] = history[[ix]]$bionomics_mBatch
    totalEggBatches[i] = history[[ix]]$bionomics_tBatch
    i = i + 1
  }

  allBloodmealIntervals = Filter(Negate(is.na),x=allBloodmealIntervals)
  humanBloodmealIntervals = Filter(Negate(is.na),x=humanBloodmealIntervals)

  # return cohort data and summary (means) of cohort data
  return(list(
    cohort = list(allBloodmeals=allBloodmeals,humanBloodmeals=humanBloodmeals,lifespans=lifespans,meanEggBatches=meanEggBatches,totalEggBatches=totalEggBatches,allBloodmealIntervals=allBloodmealIntervals,humanBloodmealIntervals=humanBloodmealIntervals),
    summary = list(allBloodmeals=mean(allBloodmeals),humanBloodmeals=mean(humanBloodmeals),lifespans=mean(lifespans),meanEggBatches=mean(meanEggBatches),totalEggBatches=mean(totalEggBatches))
  ))
}
