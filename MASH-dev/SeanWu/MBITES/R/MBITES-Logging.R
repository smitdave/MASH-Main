###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Logging
#     MBITES Team
#     July 2018
#
###############################################################################

# additional options for detailed logging of mosquito behavior


###############################################################################
# Mosquito Base Class History Logging
###############################################################################

#' MBITES: Track History
#'
#' At the end of each bout (\code{\link{mbites_oneBout}}), track the mosquito's history. If the mosquito
#' is dead, write out the history to a JSON-formatted file
#'  * This method is bound to \code{Mosquito_Female$trackHistory}
#'
mbites_trackHistory <- function(){

  # increment number of events
  private$nEvent = private$nEvent + 1L

  # check we have not overran vector
  lVec = length(private$timeHist)
  if(private$nEvent > lVec){
    private$timeHist = c(private$timeHist,numeric(lVec))
    private$siteHist = c(private$siteHist,integer(lVec))
    private$searchHist = c(private$searchHist,logical(lVec))
    private$stateHist = c(private$stateHist,character(lVec))
  }

  # add to history
  private$timeHist[private$nEvent] = private$tNext # set to tNext because that's everything that could have happened up to that next launch
  private$siteHist[private$nEvent] = private$site$get_id()
  private$searchHist[private$nEvent] = private$search
  private$stateHist[private$nEvent] = private$state

}

#' MBITES: Basic History List
#'
#' Return a named list of the basic history object.
#'  * This method is bound to \code{Mosquito$basicHistoryList}
#'
mbites_basicHistoryList <- function(){
  list(
    id = private$id,
    tile = private$tileID,
    time = private$timeHist[1:private$nEvent],
    sites = private$siteHist[1:private$nEvent],
    search = private$searchHist[1:private$nEvent],
    behavior = private$stateHist[1:private$nEvent],
    cod = private$cod
  )
}

#' MBITES: Export History and Remove Self (Female)
#'
#' If the mosquito is dead, write out its history to a JSON-formatted file and then delete from the container object (\code{\link{HashMap}}).
#'  * This method is bound to \code{Mosquito_Female$exit}
#'
#' @param pretty prettify JSON output
#'
mbites_exit_Mosquito_Female <- function(endSim=FALSE){
  self$trackHistory()
  # write out to JSON (eventually need to use jsonlite::stream_out for efficiency)
  if(endSim){
    private$stateHist[private$nEvent] = "E"
  } else {
    private$stateHist[private$nEvent] = "D"
  }
  # build up output object based on level of logging
  out = self$basicHistoryList()
  if(MBITES:::Globals$get_mosquito_f_bloodHist()){
    out = c(out,self$bloodHistList())
  }
  if(MBITES:::Globals$get_mosquito_f_eggHist()){
    out = c(out,self$eggHistList())
  }
  # write out
  cat(jsonlite::toJSON(x = out, pretty = MBITES:::Globals$pretty),",\n",sep="",file=MBITES:::Globals$get_mosquito_f_out())
  # remove this mosquito from the hash table
  MBITES:::Globals$get_tile(private$tileID)$get_mosquitoes()$rm(private$id)
}

#' MBITES: Export History and Remove Self (Male)
#'
#' If the mosquito is dead, write out its history to a JSON-formatted file and then delete from the container object (\code{\link{HashMap}}).
#'  * This method is bound to \code{Mosquito_Male$exit}
#'
#' @param pretty prettify JSON output
#'
mbites_exit_Mosquito_Male <- function(endSim=FALSE){
  self$trackHistory()
  # write out to JSON (eventually need to use jsonlite::stream_out for efficiency)
  if(endSim){
    private$stateHist[private$nEvent] = "E"
  } else {
    private$stateHist[private$nEvent] = "D"
  }
  cat(jsonlite::toJSON(x = self$basicHistoryList(), pretty = MBITES:::Globals$pretty),",\n",sep="",file=MBITES:::Globals$get_mosquito_m_out())
  # remove this mosquito from the hash table
  MBITES:::Globals$get_tile(private$tileID)$get_mosquitoes()$rm(private$id)
}


Mosquito$set(which = "public",name = "trackHistory",
          value = mbites_trackHistory, overwrite = TRUE
)

Mosquito$set(which = "public",name = "basicHistoryList",
          value = mbites_basicHistoryList, overwrite = TRUE
)

Mosquito_Female$set(which = "public",name = "exit",
          value = mbites_exit_Mosquito_Female, overwrite = TRUE
)

Mosquito_Male$set(which = "public",name = "exit",
          value = mbites_exit_Mosquito_Male, overwrite = TRUE
)


###############################################################################
# Female Mosquito Blood Host Logging
###############################################################################

#' default track probing is to turn it off
trackProbe_Mosquito_Female_Null <- function(){}

#' default track feeding is to turn it off
trackFeed_Mosquito_Female_Null <- function(){}

#' default track resting is to turn it off
trackRest_Mosquito_Female_Null <- function(){}

# set methods
Mosquito_Female$set(which = "public",name = "trackProbe",
          value = trackProbe_Mosquito_Female_Null, overwrite = TRUE
)

Mosquito_Female$set(which = "public",name = "trackFeed",
          value = trackFeed_Mosquito_Female_Null, overwrite = TRUE
)

Mosquito_Female$set(which = "public",name = "trackRest",
          value = trackRest_Mosquito_Female_Null, overwrite = TRUE
)

# advanced host tracking

#' track probing
trackProbe_Mosquito_Female <- function(){
  # check we have not overran vector
  lVec = length(private$feedTime)
  if(private$nFeed > lVec){
    private$feedTime = c(private$feedTime,numeric(lVec))
    private$feedSite = c(private$feedSite,integer(lVec))
    private$hostHist = c(private$hostHist,integer(lVec))
    private$probeAndFeed = c(private$probeAndFeed,logical(lVec))
  }

  private$feedTime[private$nFeed] = private$tNow
  private$feedSite[private$nFeed] = private$siteHist[private$nEvent]
  private$hostHist[private$nFeed] = private$hostID
  private$probeAndFeed[private$nFeed] = FALSE

  private$nFeed = private$nFeed + 1L
}

#' track feeding
trackFeed_Mosquito_Female <- function(){
  private$probeAndFeed[private$nFeed-1L] = TRUE
}

#' track resting
trackRest_Mosquito_Female <- function(){
  private$trackRest[private$nFeed-1L] = private$tNow
}

#' MBITES: Blood Feeding History List
#'
#' Return a named list of the blood feeding history object.
#'  * This method is bound to \code{Mosquito_Female$bloodHistList}
#'
mbites_bloodHistList <- function(){
  list(
    bloodHosts = private$hostHist[1:(private$nFeed-1L)],
    timeFeed = private$feedTime[1:(private$nFeed-1L)],
    siteFeed = private$feedSite[1:(private$nFeed-1L)],
    probeAndFeed = private$probeAndFeed[1:(private$nFeed-1L)],
    trackRest = private$trackRest[1:(private$nFeed-1L)]
  )
}

# # need to overwrite default exit function
# mbites_exit_Mosquito_Female <- function(endSim=FALSE){
#   self$trackHistory()
#   # write out to JSON (eventually need to use jsonlite::stream_out for efficiency)
#   if(endSim){
#     private$stateHist[private$nEvent] = "E"
#   } else {
#     private$stateHist[private$nEvent] = "D"
#   }
#   cat(jsonlite::toJSON(x = list(
#           # basic history
#           id = private$id,
#           tile = private$tileID,
#           time = private$timeHist[1:private$nEvent],
#           sites = private$siteHist[1:private$nEvent],
#           search = private$searchHist[1:private$nEvent],
#           behavior = private$stateHist[1:private$nEvent],
#           # blood feeding history
#           bloodHosts = private$hostHist[1:(private$nFeed-1L)],
#           timeFeed = private$feedTime[1:(private$nFeed-1L)],
#           siteFeed = private$feedSite[1:(private$nFeed-1L)],
#           probeAndFeed = private$probeAndFeed[1:(private$nFeed-1L)]
#           # write out
#       ), pretty = MBITES:::Globals$pretty),",\n",sep="",file=MBITES:::Globals$get_mosquito_f_out())
#   # remove this mosquito from the hash table
#   MBITES:::Globals$get_tile(private$tileID)$get_mosquitoes()$rm(private$id)
# }

#' detailed logging of blood feeding events
#' @export
trackBloodHost <- function(){

  # private fields for logging events
  Mosquito_Female$set(which = "private",name = "nFeed",
            value = 1L, overwrite = TRUE
  )

  Mosquito_Female$set(which = "private",name = "feedTime",
            value = numeric(10), overwrite = TRUE
  )

  Mosquito_Female$set(which = "private",name = "feedSite",
            value = integer(10), overwrite = TRUE
  )

  Mosquito_Female$set(which = "private",name = "hostHist",
            value = integer(10), overwrite = TRUE
  )

  Mosquito_Female$set(which = "private",name = "probeAndFeed",
            value = logical(10), overwrite = TRUE
  )

  Mosquito_Female$set(which = "private",name = "restTime",
            value = numeric(10), overwrite = TRUE
  )

  # public method for tracking
  Mosquito_Female$set(which = "public",name = "trackProbe",
            value = trackProbe_Mosquito_Female, overwrite = TRUE
  )

  Mosquito_Female$set(which = "public",name = "trackFeed",
            value = trackFeed_Mosquito_Female, overwrite = TRUE
  )

  Mosquito_Female$set(which = "public",name = "trackRest",
            value = trackRest_Mosquito_Female, overwrite = TRUE
  )

  Mosquito_Female$set(which = "public",name = "bloodHistList",
            value = mbites_bloodHistList, overwrite = TRUE
  )

  # Mosquito_Female$set(which = "public",name = "exit",
  #           value = mbites_exit_Mosquito_Female, overwrite = TRUE
  # )

  # set flag
  MBITES:::Globals$set_mosquito_f_bloodHist(TRUE)
}


###############################################################################
# Female Mosquito Oviposition Logging
###############################################################################

#' default track oviposition is to turn it off
trackOviposit_Mosquito_Female_Null <- function(){}

Mosquito_Female$set(which = "public",name = "trackOviposit",
          value = trackOviposit_Mosquito_Female_Null, overwrite = TRUE
)

#' track oviposition
trackOviposit_Mosquito_Female <- function(){
  # check we have not overran vector
  lVec = length(private$eggTime)
  if(private$nEgg > lVec){
    private$eggTime = c(private$eggTime,numeric(lVec))
    private$eggSite = c(private$eggSite,integer(lVec))
    private$eggBatch = c(private$eggBatch,integer(lVec))
  }

  private$eggTime[private$nEgg] = private$tNow
  private$eggSite[private$nEgg] = private$siteHist[private$nEvent]
  private$eggBatch[private$nEgg] = private$batch

  private$nEgg = private$nEgg + 1L

}

#' MBITES: Oviposition History List
#'
#' Return a named list of the oviposition history object.
#'  * This method is bound to \code{Mosquito_Female$eggHistList}
#'
mbites_eggHistList <- function(){
  list(
    ovipositionTimes = private$eggTime[1:(private$nEgg-1L)],
    ovipositionSites = private$eggSite[1:(private$nEgg-1L)],
    ovipositionBatchSize = private$eggBatch[1:(private$nEgg-1L)]
  )
}

#' detailed logging of oviposition events
#' @export
trackOviposition <- function(){

  # private fields for logging events
  Mosquito_Female$set(which = "private",name = "nEgg",
            value = 1L, overwrite = TRUE
  )

  Mosquito_Female$set(which = "private",name = "eggTime",
            value = numeric(10), overwrite = TRUE
  )

  Mosquito_Female$set(which = "private",name = "eggSite",
            value = integer(10), overwrite = TRUE
  )

  Mosquito_Female$set(which = "private",name = "eggBatch",
            value = integer(10), overwrite = TRUE
  )

  # public method for tracking
  Mosquito_Female$set(which = "public",name = "trackOviposit",
            value = trackOviposit_Mosquito_Female, overwrite = TRUE
  )

  Mosquito_Female$set(which = "public",name = "eggHistList",
            value = mbites_eggHistList, overwrite = TRUE
  )

  # set flag
  MBITES:::Globals$set_mosquito_f_eggHist(TRUE)
}
