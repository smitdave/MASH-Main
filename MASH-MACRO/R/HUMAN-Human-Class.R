###############################################################################
#
#       __  ____  ____  ______    _   __
#      / / / / / / /  |/  /   |  / | / /
#     / /_/ / / / / /|_/ / /| | /  |/ /
#    / __  / /_/ / /  / / ___ |/ /|  /
#   /_/ /_/\____/_/  /_/_/  |_/_/ |_/
#
#   MASH-MACRO
#   HUMAN: Human Class Definition
#   David Smith, Hector Sanchez, Sean Wu
#   August 17, 2017
#
###############################################################################


#' Human Class Definition
#'
#' This is a generic human being blah blah ...
#' Each instance of a \code{Human} lives in a \code{\link{HumanPop}}
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section Constructor:
#'  * item 1:
#'
#' @section Methods:
#'  * get_myID: see \code{\link{get_myID_human}}
#'  * set_myID: see \code{\link{set_myID_human}}
#'  * get_houseID: see \code{\link{get_houseID_human}}
#'  * set_houseID: see \code{\link{set_houseID_human}}
#'  * get_patchID: see \code{\link{get_patchID_human}}
#'  * set_patchID: see \code{\link{et_patchID_human}}
#'  * get_bDay: see \code{\link{get_bDay_human}}
#'  * set_bDay: see \code{\link{set_bDay_human}}
#'  * get_EventQ: see \code{\link{get_EventQ_human}}
#'  * get_Alive: see \code{\link{get_Alive_human}}
#'  * set_Alive: see \code{\link{set_Alive_human}}
#'  * get_sex: see \code{\link{get_sex_human}}
#'  * set_sex: see \code{\link{set_sex_human}}
#'  * get_bWeight: see \code{\link{get_bWeight_human}}
#'  * set_bWeight: see \code{\link{set_bWeight_human}}
#'  * set_Pathogens: see \code{\link{set_Pathogens_human}}
#'  * get_HumansPointer: see \code{\link{get_HumansPointer_human}}
#'  * set_HumansPointer: see \code{\link{set_HumansPointer_human}}
#'  * oneEvent: see \code{\link{oneEvent_human}}
#'  * runEvent: see \code{\link{runEvent_human}}
#'  * liveLife: see \code{\link{liveLife_human}}
#'  * event_maxDeath: see \code{\link{event_maxDeath_human}}
#'  * death: see \code{\link{death_human}}
#'  * track_history: see \code{\link{track_history_human}}
#'  * get_history: see \code{\link{get_history_human}}
#'
#' @section Fields:
#'  * **myID**: character identifier of this human, first digits are integer ID and digits after underscore (_) are the patchID; houseID is not necessary because
#'             location is not resolved to house level in MACRO and MICRO only needs houseID to distribute risk onto \code{\link{feedingSite}} rather than to store humans.
#'             myID is needed to resolve location to patch level in order to move people between patches in either MICRO or MACRO.
#'
#' @md
#' @export
Human <- R6::R6Class(classname="Human",
                     portable = TRUE,
                     cloneable = TRUE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #################################################
                       # Constructor
                       #################################################

                       initialize = function(myID, houseID = NULL, patchID = NULL, bDay = NULL, bWeight = NULL){
                         private$myID = myID
                         private$houseID = houseID
                         private$patchID = patchID
                         private$bDay = bDay
                         private$bWeight = bWeight
                         private$EventQueue = MASHcpp::HumanEventQ()
                         private$History = MASHcpp::HistoryGeneric()
                       }

                       ),

                     #private members
                     private = list(

                       #General Information
                       myID = NULL, # string
                       houseID = NULL, # integer
                       patchID = NULL, # integer
                       Alive = TRUE, # boolean
                       bDay = NULL, # double
                       sex = NULL, # integer

                       #Event Queue
                       EventQueue = NULL, # see HUMANS-EventQ.hpp

                       # Event History
                       History = NULL, # see HUMANS-History.hpp

                       # Health & Related
                       bWeight = NULL, # double

                       # Pathogens
                       Pathogens = NULL, # see PATHOGEN-XX.hpp

                       # Pointers
                       HumansPointer = NULL # environment

                     )
                     

                     


) #end class definition


#################################################
# Getters and Setters
#################################################


#' Human: Gets ID
#'
#' Gets ID for human
#'
#' More details
#'

get_myID_human = function(){
return(private$myID)
}


Human$set(which="public", name="get_myID",
	value=get_myID_human,
	overwrite=TRUE)


#' Human: Sets ID
#'
#' Sets ID for human
#'
#' More details
#'
set_myID_human = function(myID){
 private$myID = myID
}


Human$set(which="public", name="set_myID",
	value=set_myID_human, 
	overwrite=TRUE)


#' Human: Gets house ID
#'
#' Gets ID for house
#'
#' More details
#'
get_houseID_human = function(){
 return(private$houseID)
}


Human$set(which="public", name="get_houseID",
	value=get_houseID_human,
	overwrite=TRUE)


#' Human: Sets house ID
#'
#' Sets ID for house
#'
#' More details
#'
set_houseID_human = function(houseID){
 private$houseID = houseID
}


Human$set(which="public", name="set_houseID",
	value=set_houseID_human,
	overwrite=TRUE)


#' Human: Gets patch ID
#'
#' Gets ID for patch
#'
#' More details
#'
get_patchID_human = function(){
 return(private$patchID)
}


Human$set(which="public", name="get_patchID",
	value=get_patchID_human,
	overwrite=TRUE)


#' Human: Sets patch ID
#'
#' Sets ID for patch
#'
#' More details
#'
set_patchID_human = function(patchID){
 private$patchID = patchID
}


Human$set(which="public", name="set_patchID",
	value=set_patchID_human,
	 overwrite=TRUE)


#' Human: Get birthday
#'
#' Gets birthday for human
#'
#' More details
#'
get_bDay_human = function(){
 return(private$bDay)
}


Human$set(which="public", name="get_bDay",
	value=get_bDay_human,
	 overwrite=TRUE)


#' Human: Set birthday
#'
#' Sets birthday
#'
#' More details
#'
set_bDay_human = function(bDay){
 private$bDay = bDay
}


Human$set(which="public", name="set_bDay",
	value=set_bDay_human,
	overwrite=TRUE)


#' Human: Set EventQ
#'
#' Sets EventQ for human
#'
#' More details
#'
get_EventQ_human = function(){
 return(private$EventQueue)
}


Human$set(which="public", name="get_bDay",
	value=get_bDay_human,
	overwrite=TRUE)


#' Human: Get Life Status
#'
#' Gets whether human alive or dead
#'
#' More details
#'
get_Alive_human = function(){
 return(private$Alive)
}


Human$set(which="public", name="get_Alive",
	value=get_Alive_human,
	 overwrite=TRUE)


#' Human: Set Life Status
#'
#' Sets whether human alive or not
#'
#' More details
#'
set_Alive_human = function(Alive){
 private$Alive = Alive
}


Human$set(which="public", name="set_Alive",
	value=set_Alive_human,
	overwrite=TRUE)


#' Human: Get sex for human
#'
#' Gets sex for human
#'
#' More details
#'
get_sex_human = function(){
 return(private$sex)
}


Human$set(which="public", name="get_sex",
	value=get_sex_human,
	 overwrite=TRUE)


#' Human: Set sex
#'
#' Sets sex for human
#'
#' More details
#'
set_sex_human = function(sex){
 private$sex = sex
}


Human$set(which="public", name="set_sex",
	value=set_sex_human,
	overwrite=TRUE)


#' Human: Get weight
#'
#' Gets weight for human
#'
#' More details
#'
get_bWeight_human = function(){
 return(private$bWeight)
}


Human$set(which="public", name="get_bWeight",
	value=get_bWeight_human,
	 overwrite=TRUE)


#' Human: Set weight
#'
#' Sets weight for human
#'
#' More details
#'
set_bWeight_human = function(bWeight){
 private$bWeight = bWeight
}


Human$set(which="public", name="set_bWeight",
	value=set_bWeight_human,
	overwrite=TRUE)


#' Human: Get pathogen
#'
#' Gets pathogen
#'
#' More details
#'
get_Pathogens_human = function(){
 return(private$Pathogens)
}


Human$set(which="public", name="get_Pathogens",
	value=get_Pathogens_human,
	 overwrite=TRUE)


#' Human: Set pathogen
#'
#' Sets pathogen
#'
#' More details
#'
set_Pathogens_human = function(Pathogens){
 private$Pathogens = Pathogens
}


Human$set(which="public", name="set_Pathogens",
	value=set_Pathogens_human,
	overwrite=TRUE)


#################################################
# Pointers
#################################################


#' Human: Get Humans Pointer
#'
#' Gets humans pointer 
#'
#' More details
#'
get_HumansPointer_human = function(){
 return(private$HumansPointer)
}

Human$set(which="public", name="get_HumansPointer",
	value=get_HumansPointer_human,
	overwrite=TRUE)


#' Human: Set Human Pointer
#'
#' Sets human pointer
#'
#' More details
#'
set_HumansPointer_human = function(HumansPointer){
 private$HumansPointer = HumansPointer
}


Human$set(which="public", name="set_HumansPointer",
	value=set_HumansPointer_human,
	overwrite=TRUE)


#################################################
# Event Queue
#################################################


#' Human: Initialize Event
#'
#' Initializes one event
#'
#' More details
#'
oneEvent_human = function(tPause){
event = private$EventQueue$firstEvent()
self$runEvent(tEvent = event$tEvent, PAR = event$PAR, tag = event$tag)
private$EventQueue$rmFirstEventFromQ()
}


Human$set(which="public", name="oneEvent",
	value=oneEvent_human,
	 overwrite=TRUE)


#' Human: Run Event
#'
#' Runs event
#'
#' More details
#'
runEvent_human = function(tEvent, PAR, tag){
 self[[tag]](tEvent, PAR)
}


Human$set(which="public", name="runEvent",
	value=runEvent_human,
	 overwrite=TRUE)


#' Human: liveLife
#'
#' Initializes liveLife
#'
#' More details
#'
liveLife_human = function(tPause){
while(private$Alive & private$EventQueue$firstTime() < tPause){
  self$oneEvent(tPause)
  if(private$EventQueue$get_queueN()==0){
    break()
  }
}

}


Human$set(which="public", name="lifeLife",
	value=liveLife_human,
	 overwrite=TRUE)


#################################################
# Life Events
#################################################


#' Human: The Maximum Death Time Event Package
#'
#' Maximum Death Time Event Package
#'
#' More details
#'
event_maxDeath_human = function(tEvent = 73000, PAR = NULL, tag = "death"){
 list(tEvent = tEvent, PAR = PAR, tag = tag)
}


Human$set(which="public", name="event_maxDeath",
	value=event_maxDeath_human,
	 overwrite=TRUE)


#' Human: Death
#'
#' tracks death
#'
#' More details
#'
# death: the death event
death_human = function(tEvent, PAR){
 self$track_history(tEvent = tEvent, event = "D")
 private$Alive = FALSE
}


Human$set(which="public", name="death",
	value=death_human,
	 overwrite=TRUE)


#################################################
# Auxiliary Functions
#################################################


#' Human: Tracks History
#'
#' Tracks history of events for human
#'
#' More details
#'
track_history_human = function(tEvent, event){
 private$History$track_history(tEvent, event)
}


Human$set(which="public", name="track_history",
	value=track_history_human,
	 overwrite=TRUE)


#' Human: Get History
#'
#' Gets history of human
#'
#' More details
#'
get_history_human = function(){
 return(private$History$get_history())
}


Human$set(which="public", name="get_history",
	value=get_history_human,
	 overwrite=TRUE)



