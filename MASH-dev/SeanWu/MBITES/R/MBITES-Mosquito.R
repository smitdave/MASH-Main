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

                     private$site = site
                     private$tileID = tileID

                     private$bDay = bDay
                     private$tNow = bDay
                     private$tNext = bDay

                     private$search = FALSE
                     private$state = state

                     # set up history
                     private$tHist[1] = bDay
                     private$sHist[1] = site$get_id()
                     private$bHist[1] = state

                   } # end constructor

                 ), # end public members

                 # private members
                 private = list(

                   # basic parameters
                   id             = integer(1), # character id

                   # location
                   tileID         = integer(1), # id of the tile i am in
                   site           = NULL, # reference to my current site
                   rspot          = character(1), # my current resting spot

                   # resources
                   sugar_res      = NULL, # reference to my current sugar resource
                   mate_res       = NULL, # reference to my current mating swarm resource

                   # timing
                   bDay          = numeric(1), # the day i emerged
                   tNext         = numeric(1), # time of my next launch
                   tNow          = numeric(1), # time of my current launch

                   # behavioral state parameters
                   search         = logical(1), # next launch is for search or attempt bout?
                   searchNow      = logical(1), # is my current bout a search bout?
                   state          = character(1), # my current behavioral state
                   starved        = logical(1), # am i starved for sugar?
                   boutFail       = integer(1), # counter

                   # energetics
                   energy         = numeric(1), # my current energy
                   # energy_preG    = numeric(1), # pre-gonotrophic energy requirement
                   mature         = logical(1), # am i mature?

                   # survival
                   damage_physical = numeric(1), # physical damage
                   damage_chemical = numeric(1), # chemical damage

                   # bloodfeeding and oogenesis
                   # batch          = integer(1), # size of my egg batch
                   # bm_size        = numeric(1), # size of my blood meal

                   # resource ids
                   sugarID        = integer(1), # id of my sugar  source
                   mateID         = integer(1), # id of my mate

                   # history
                   nEvent         = 1L, # number of bouts + emergence (birthday) (increment at the beginning of the trackHistory function)
                   tHist          = numeric(20), # history of event times (t)
                   sHist          = integer(20), # history of sites visited (s)
                   bHist          = character(20) # history of behavioral states (b)
                 ) # end private members
)



# get_id


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
                   initialize = function(bDay, state, site, tileID){

                     super$initialize(bDay,state,site,tileID) # construct the base-class parts

                     private$energy_preG = MBITES:::Parameters$get_energy_preG()

                   }, # end constructor

                   # pathogenDynamics
                   pathogenDynamics = function(){
                     futile.logger::flog.warn("default 'pathogenDynamics' being called for mosquito: ",private$id)
                   }

                 ), # end public members

                 # private members
                 private = list(

                   # resources
                   aqua_res       = NULL, # reference to my current aquatic habitat resource
                   feed_res       = NULL, # reference to my current blood feeding resource

                   # behavioral state parameters
                   mated          = FALSE, # have i mated yet?
                   gravid         = logical(1), # am i gravid to oviposit?

                   # energetics
                   energy_preG    = numeric(1), # pre-gonotrophic energy requirement

                   # bloodfeeding and oogenesis
                   bloodfed       = FALSE, # have i fed on blood this bout?
                   batch          = integer(1), # size of my egg batch
                   eggT           = numeric(1), # time my egg batch is ready
                   bm_size        = numeric(1), # size of my blood meal

                   # host ids
                   hostID         = integer(1) # id of my blood host

                 ) # end private members
) # end class definition

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
  lVec = length(private$hist$tHist)
  if(private$nEvent > lVec){
    private$tHist = c(private$tHist,numeric(lVec))
    private$sHist = c(private$sHist,integer(lVec))
    private$bHist = c(private$bHist,character(lVec))
  }

  # add to history
  private$tHist[private$nEvent] = private$tNext # set to tNext because that's everything that could have happened up to that next launch
  private$sHist[private$nEvent] = private$site$get_id()
  private$bHist[private$nEvent] = private$state

  # write and delete if dead
  self$exit()
}

#' MBITES: Export History and Remove Self
#'
#' If the mosquito is dead, write out its history to a JSON-formatted file and then delete from the container object (\code{\link{HashMap}}).
#'  * This method is bound to \code{Mosquito_Female$exit}
#'
mbites_exit <- function(){
  if(private$state=="D"){
    # write out to JSON (eventually need to use jsonlite::stream_out for efficiency)
    cat(jsonlite::toJSON(x = list(
            id = private$id,
            time = private$hist$tHist[1:private$nEvent],
            sites = private$hist$sHist[1:private$nEvent],
            behavior = private$hist$bHist[1:private$nEvent]
        ), pretty = TRUE),"\n",sep="",file=mosquito_f_out)
    # remove this mosquito from the hash table
    MBITES:::Globals$get_tile(private$tileID)$get_mosquitoes()$rm(private$id)
  }
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

                     super$initialize(FALSE,bDay,site,tileID) # construct the base-class parts


                   } # end constructor

                 ),

                 # private members
                 private = list()
)
