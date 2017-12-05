###############################################################################
#
#       __  ____  ____  ______    _   __
#      / / / / / / /  |/  /   |  / | / /
#     / /_/ / / / / /|_/ / /| | /  |/ /
#    / __  / /_/ / /  / / ___ |/ /|  /
#   /_/ /_/\____/_/  /_/_/  |_/_/ |_/
#
#   MASH-MACRO
#   HUMAN: HumanPop Class Definition
#   David Smith, Hector Sanchez, Sean Wu
#   August 18, 2017
#
###############################################################################

#' HumanPop Class Definition
#'
#' Generate a single human population; they may live in a \code{\link{MacroPatch}} or a MicroPatch, the individual humans in the pop may move freely between patches.
#' Each instance of a \code{\link{Human}} lives in a \code{HumanPop}
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * N: number of humans
#'  * patchID: integer ID of the patch this \code{HumanPop} lives in
#'  * houseIDs = NULL: vector of house IDs (only needed in MICRO, in MACRO human location is only resolved to the \code{\link{MacroPatch}} level)
#'  * ages: vector of birthdays (given as tStart - age at start)
#'  * bWeights: vector of biting weights
#'  * tStart = 0: time to start simulation; constructor will complain if is a value other than 0
#'
#' @section **Methods**:
#'  * get_pop: see \code{\link{get_pop_HumanPop}}
#'  * get_human: see \code{\link{get_human_HumanPop}}
#'  * get_history: see \code{\link{get_history_HumanPop}}
#'  * simHumans: see \code{\link{simHumans_HumanPop}}
#'
#' @section **Fields**:
#'  * N: number of human
#'  * tStart: time to start simulation
#'  * pop: a object of class \code{\link[MASHcpp]{HashMap}} that stores instantiations of \code{\link{Human}}, see help for more details on the internal structure of this type.
#'
#'
#'
#'
#'
#'
#'
#'
#' @export
HumanPop <- R6::R6Class(classname = "HumanPop",
                    portable = TRUE,
                    cloneable = FALSE,
                    lock_class = FALSE,
                    lock_objects = FALSE,

                    # public members
                    public = list(

                      #################################################
                      # Constructor
                      #################################################

                      # initialize = function(patchID, HumanPop_PAR){
                      initialize = function(HumanPop_PAR){

                        private$N = length(HumanPop_PAR)
                        private$pop = MASHcpp::HashMap$new(N = private$N)
                        private$tStart = 0

                        # private$pop = MASHcpp::HashMap$new(N = HumanPop_PAR$N+50L)
                        # private$N = HumanPop_PAR$N
                        # private$tStart = 0

                        for(i in 1:private$N){

                          cat("initializing human ",i," of ",private$N,"\n")

                          # id = paste0(i,"_",patchID)
                          # human = Human$new(myID = id, houseID = HumanPop_PAR$homeIDs[i], patchID = patchID, age = HumanPop_PAR$age[i], bWeight = HumanPop_PAR$bWeight[i])
                          # private$pop$assign(key=id,value=human)

                          id = paste0(i,"_",HumanPop_PAR[[i]]$patchID)
                          human = Human$new(myID=id,houseID=HumanPop_PAR[[i]]$houseID,patchID=HumanPop_PAR[[i]]$patchID,homeHouseID=HumanPop_PAR[[i]]$houseID,homePatchID=HumanPop_PAR[[i]]$patchID,age=HumanPop_PAR[[i]]$age,bWeight=HumanPop_PAR[[i]]$bWeight)
                          human$set_HumansPointer(self)
                          private$pop$assign(key=id,value=human)


                        }

                      }

                    ),

                    # private members
                    private = list(

                      # fields
                      N = NULL,
                      tStart = numeric(1),
                      pop = NULL,

                      # pointers
                      TilePointer = NULL,
                      PatchPointer = NULL

                    )
)

###############################################################################
# HumanPop: Getters & Setters
###############################################################################

#' HumanPop: Get the Population
#'
#' Return \code{private$pop}
#'
#'
get_pop_HumanPop <- function(){
  return(private$pop)
}

HumanPop$set(which = "public",name = "get_pop",
  value = get_pop_HumanPop,
  overwrite = TRUE
)

#' HumanPop: Get a Human
#'
#' Given a character \code{humanID} return that human (calls \code{\link[MASHcpp]{get_HashMap}}) to get the human.
#'
#' @param humanID character
#'
get_human_HumanPop <- function(humanID){
  return(private$pop$get(key=humanID))
}

HumanPop$set(which = "public",name = "get_human",
  value = get_human_HumanPop,
  overwrite = TRUE
)

#' HumanPop: Get all Histories
#'
#' Return all human histories as a named list (names correspond to human \code{myID}). See \code{\link[MASHcpp]{HistoryGeneric}} for details on the history implementation.
#'
#'
get_history_HumanPop <- function(){
  return(
    private$pop$apply(tag="get_history",returnVal=TRUE)
  )
}

HumanPop$set(which = "public",name = "get_history",
  value = get_history_HumanPop,
  overwrite = TRUE
)

#' HumanPop: Get all Pathogen histories
#'
#' Get all pathogen histories.
#'  * This function is bound to \code{HumanPop$get_PathogensHistory()}
#'
get_PathogensHistory_HumanPop <- function(){
  return(
    private$pop$apply(tag="get_PathogensHistory",returnVal=TRUE)
  )
}

HumanPop$set(which = "public",name = "get_PathogensHistory",
  value = get_PathogensHistory_HumanPop,
  overwrite = TRUE
)



###############################################################################
# HumanPop: Simulation & Events
###############################################################################

#' HumanPop: Simulate Humans
#'
#' Simulate each human's event queue.
#'
#' @param tPause numeric (run all events in each human's \code{\link[MASHcpp]{HumanEventQ}} that occur before this time)
#'
simHumans_HumanPop <- function(tPause){
  private$pop$apply(tag="simHuman",returnVal=FALSE,tPause=tPause)
}

HumanPop$set(which = "public",name = "simHumans",
  value = simHumans_HumanPop,
  overwrite = TRUE
)


###############################################################################
# HumanPop: Pointers
###############################################################################


#' Get Tile Pointer
#'
#' Return either microsimulation \code{\link[MASHmicro]{Tile}} or macrosimulation \code{\link{Tile}} enclosing this site.
#'  * This method is bound to \code{HumanPop$get_TilePointer}
#'
get_TilePointer_HumanPop <- function(){
 return(private$TilePointer)
}

HumanPop$set(which="public", name="get_TilePointer",
	value = get_TilePointer_HumanPop, overwrite=TRUE
)


#' Set Tile Pointer
#'
#' Set either microsimulation \code{\link[MASHmicro]{Tile}} or macrosimulation \code{\link{Tile}} enclosing this site.
#'  * This method is bound to \code{HumanPop$set_TilePointer}
#'
#' @param TilePointer an environment
#'
set_TilePointer_HumanPop <- function(TilePointer){
 private$TilePointer = TilePointer
}


HumanPop$set(which="public", name="set_TilePointer",
	value = set_TilePointer_HumanPop, overwrite=TRUE
)
