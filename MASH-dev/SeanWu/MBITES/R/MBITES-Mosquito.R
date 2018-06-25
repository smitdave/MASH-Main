###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Mosquito
#     MBITES Team
#     February 2018
#
###############################################################################


###############################################################################
# Abstract Base Mosquito
###############################################################################

#' MBITES: Mosquito Class
#'
#' All mosquitoes inherit from the \code{Mosquito} abstract base class object.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: im an agument!
#'
#' @section **Methods**:
#'  * method: im a method!
#'
#' @section **Fields**:
#'  * id: integer id (obtained from \code{\link{MBITES_Globals}})
#'  * field: im a field!
#'
#' @export
Mosquito <- R6::R6Class(classname = "Mosquito",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(bDay, state, site, tileID){

                     # set up parameters
                     private$id = MBITES:::Globals$get_mosquito_id()

                     private$alive = TRUE

                     private$site = site
                     private$tileID = tileID

                     private$bDay = bDay
                     private$tNow = bDay
                     private$tNext = bDay

                     private$search = TRUE
                     private$state = state
                     private$starved = FALSE

                     # set up history
                     private$timeHist[1] = bDay
                     private$siteHist[1] = site$get_id()
                     private$stateHist[1] = state
                     private$searchHist[1] = TRUE

                     # logging
                     futile.logger::flog.trace("Mosquito %s being born at: self %s , private %s",private$id,pryr::address(self),pryr::address(private))

                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     # logging
                     futile.logger::flog.trace("Mosquito %s being killed at: self %s , private %s",private$id,pryr::address(self),pryr::address(private))
                   }

                 ), # end public members

                 # private members
                 private = list(

                   # basic parameters
                   id             = integer(1), # character id
                   alive          = logical(1), # am i alive?

                   # location
                   tileID         = integer(1), # id of the tile i am in
                   site           = NULL, # reference to my current site
                   rspot          = "v", # my current resting spot

                   # resources
                   sugar_resource      = NULL, # reference to my current sugar resource
                   mating_resource       = NULL, # reference to my current mating swarm resource

                   # timing
                   bDay          = numeric(1), # the day i emerged
                   tNext         = numeric(1), # time of my next launch
                   tNow          = numeric(1), # time of my current launch

                   # behavioral state parameters
                   search         = logical(1), # next launch is for search or attempt bout?
                   searchNow      = logical(1), # is my current bout a search bout?
                   state          = character(1), # my current behavioral state
                   starved        = FALSE, # am i starved for sugar?
                   boutFail       = 0L, # counter

                   # energetics
                   energy         = 1, # my current energy
                   mature         = FALSE, # am i mature?

                   # survival (mosquitoes start out at full life)
                   damage_physical = 0, # physical damage
                   damage_chemical = 0, # chemical damage

                   # resource ids
                   sugarID        = integer(1), # id of my sugar  source
                   mateID         = integer(1), # id of my mate

                   # history
                   nEvent         = 1L, # number of bouts + emergence (birthday) (increment at the beginning of the trackHistory function)
                   timeHist       = numeric(30), # history of event times (t)
                   siteHist       = integer(30), # history of sites visited (s)
                   searchHist     = logical(30), # history of searching?
                   stateHist      = character(30) # history of behavioral states (b)
                 ) # end private members
)

get_id_Mosquito <- function(){
  return(private$id)
}

Mosquito$set(which = "public",name = "get_id",
    value = get_id_Mosquito, overwrite = TRUE
)

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

#' MBITES: Export History and Remove Self
#'
#' If the mosquito is dead, write out its history to a JSON-formatted file and then delete from the container object (\code{\link{HashMap}}).
#'  * This method is bound to \code{Mosquito_Female$exit}
#'
#' @param pretty prettify JSON output
#'
mbites_exit <- function(endSim=FALSE){
  self$trackHistory()
  # write out to JSON (eventually need to use jsonlite::stream_out for efficiency)
  if(endSim){
    private$stateHist[private$nEvent] = "E"
  } else {
    private$stateHist[private$nEvent] = "D"
  }
  cat(jsonlite::toJSON(x = list(
          id = private$id,
          tile = private$tileID,
          time = private$timeHist[1:private$nEvent],
          sites = private$siteHist[1:private$nEvent],
          search = private$searchHist[1:private$nEvent],
          behavior = private$stateHist[1:private$nEvent]
      ), pretty = MBITES:::Globals$pretty),",\n",sep="",file=MBITES:::Globals$get_mosquito_f_out())
  # remove this mosquito from the hash table
  MBITES:::Globals$get_tile(private$tileID)$get_mosquitoes()$rm(private$id)
}

Mosquito$set(which = "public",name = "trackHistory",
          value = mbites_trackHistory, overwrite = TRUE
)

Mosquito$set(which = "public",name = "exit",
          value = mbites_exit, overwrite = TRUE
)


###############################################################################
# Female Mosquito
###############################################################################

#' MBITES: Female Mosquito Class
#'
#' Female mosquitoes inherit from the \code{\link{Mosquito}} abstract base class object.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: im an agument!
#'
#' @section **Methods**:
#'  * method: im a method!
#'
#' @section **Fields**:
#'  * id: integer id (obtained from \code{\link{MBITES_Globals}})
#'  * field: im a field!
#'
#' @export
Mosquito_Female <- R6::R6Class(classname = "Mosquito_Female",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,
                 inherit = MBITES:::Mosquito,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(bDay, site, tileID){

                     super$initialize(bDay,MBITES:::Parameters$get_defaultState_F(),site,tileID) # construct the base-class parts

                     private$energyPreG = MBITES:::Parameters$get_energyPreG()

                   }, # end constructor

                   # pathogenDynamics
                   pathogenDynamics = function(){
                     futile.logger::flog.warn("default 'pathogenDynamics' being called for mosquito: ",private$id)
                   }

                 ), # end public members

                 # private members
                 private = list(

                   # resources
                   aqua_resource       = NULL, # reference to my current aquatic habitat resource
                   feeding_resource       = NULL, # reference to my current blood feeding resource

                   # behavioral state parameters
                   mated          = FALSE, # have i mated yet?
                   gravid         = logical(1), # am i gravid to oviposit?

                   # energetics
                   energyPreG    = numeric(1), # pre-gonotrophic energy requirement

                   # bloodfeeding and oogenesis
                   bloodfed       = FALSE, # have i fed on blood this bout?
                   batch          = integer(1), # size of my egg batch
                   eggT           = 2e16, # time my egg batch is ready
                   eggP           = numeric(1),
                   bmSize         = numeric(1), # size of my blood meal

                   # host ids
                   hostID         = integer(1) # id of my blood host

                 ) # end private members
) # end class definition

###############################################################################
# Female Mosquito Blood Host Logging
###############################################################################

#' default track probing is to turn it off
trackProbe_Mosquito_Female_Null <- function(){}

#' default track feeding is to turn it off
trackFeed_Mosquito_Female_Null <- function(){}

# set methods
Mosquito_Female$set(which = "public",name = "trackProbe",
          value = trackProbe_Mosquito_Female_Null, overwrite = TRUE
)

Mosquito_Female$set(which = "public",name = "trackFeed",
          value = trackFeed_Mosquito_Female_Null, overwrite = TRUE
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

# need to overwrite default exit function
mbites_exit_Mosquito_Female <- function(endSim=FALSE){
  self$trackHistory()
  # write out to JSON (eventually need to use jsonlite::stream_out for efficiency)
  if(endSim){
    private$stateHist[private$nEvent] = "E"
  } else {
    private$stateHist[private$nEvent] = "D"
  }
  cat(jsonlite::toJSON(x = list(
          # basic history
          id = private$id,
          tile = private$tileID,
          time = private$timeHist[1:private$nEvent],
          sites = private$siteHist[1:private$nEvent],
          search = private$searchHist[1:private$nEvent],
          behavior = private$stateHist[1:private$nEvent],
          # blood feeding history
          bloodHosts = private$hostHist[1:(private$nFeed-1L)],
          timeFeed = private$feedTime[1:(private$nFeed-1L)],
          siteFeed = private$feedSite[1:(private$nFeed-1L)],
          probeAndFeed = private$probeAndFeed[1:(private$nFeed-1L)]
          # write out
      ), pretty = MBITES:::Globals$pretty),",\n",sep="",file=MBITES:::Globals$get_mosquito_f_out())
  # remove this mosquito from the hash table
  MBITES:::Globals$get_tile(private$tileID)$get_mosquitoes()$rm(private$id)
}

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
            value = numeric(10), overwrite = TRUE
  )

  Mosquito_Female$set(which = "private",name = "hostHist",
            value = integer(10), overwrite = TRUE
  )

  Mosquito_Female$set(which = "private",name = "probeAndFeed",
            value = logical(10), overwrite = TRUE
  )

  # public method for tracking
  Mosquito_Female$set(which = "public",name = "trackProbe",
            value = trackProbe_Mosquito_Female, overwrite = TRUE
  )

  Mosquito_Female$set(which = "public",name = "trackFeed",
            value = trackFeed_Mosquito_Female, overwrite = TRUE
  )

  Mosquito_Female$set(which = "public",name = "exit",
            value = mbites_exit_Mosquito_Female, overwrite = TRUE
  )
}


###############################################################################
# Male Mosquito
###############################################################################

#' MBITES: Male Mosquito Class
#'
#' Male mosquitoes inherit from the \code{\link{Mosquito}} abstract base class object.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: im an agument!
#'
#' @section **Methods**:
#'  * method: im a method!
#'
#' @section **Fields**:
#'  * id: integer id (obtained from \code{\link{MBITES_Globals}})
#'  * field: im a field!
#'
#' @export
Mosquito_Male <- R6::R6Class(classname = "Mosquito_Male",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,
                 inherit = MBITES:::Mosquito,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(bDay, site, tileID){

                     super$initialize(bDay,MBITES:::Parameters$get_defaultState_M(),site,tileID) # construct the base-class parts


                   } # end constructor

                 ),

                 # private members
                 private = list()
)
