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
#'  * item 1:
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
                       },

                       #################################################
                       # Getters and Setters
                       #################################################

                       #myID
                       get_myID = function(){
                         return(private$myID)
                       },
                       set_myID = function(myID){
                         private$myID = myID
                       },

                       #houseID
                       get_houseID = function(){
                         return(private$houseID)
                       },
                       set_houseID = function(houseID){
                         private$houseID = houseID
                       },

                       #patchID
                       get_patchID = function(){
                         return(private$patchID)
                       },
                       set_patchID = function(patchID){
                         private$patchID = patchID
                       },

                       #bDay
                       get_bDay = function(){
                         return(private$bDay)
                       },
                       set_bDay = function(bDay){
                         private$bDay = bDay
                       },

                       #eventQ
                       get_EventQ = function(){
                         return(private$EventQueue)
                       },

                       #Alive
                       get_Alive = function(){
                         return(private$Alive)
                       },
                       set_Alive = function(Alive){
                         private$Alive = Alive
                       },

                       get_sex = function(){
                         return(private$sex)
                       },
                       set_sex = function(sex){
                         private$sex = sex
                       },

                       # Health & Related
                       get_bWeight = function(){
                         return(private$bWeight)
                       },
                       set_bWeight = function(bWeight){
                         private$bWeight = bWeight
                       },

                       # Pathogens
                       get_Pathogens = function(){
                         return(private$Pathogens)
                       },
                       set_Pathogens = function(Pathogens){
                         private$Pathogens = Pathogens
                       },

                       #################################################
                       # Pointers
                       #################################################

                       # HumanPop
                       get_HumansPointer = function(){
                         return(private$HumansPointer)
                       },
                       set_HumansPointer = function(HumansPointer){
                         private$HumansPointer = HumansPointer
                       },

                       #################################################
                       # Event Queue
                       #################################################

                       #oneEvent:
                       oneEvent = function(tPause){
                        event = private$EventQueue$firstEvent()
                        self$runEvent(tEvent = event$tEvent, PAR = event$PAR, tag = event$tag)
                        private$EventQueue$rmFirstEventFromQ()
                       },

                       # run an event
                       runEvent = function(tEvent, PAR, tag){
                         self[[tag]](tEvent, PAR)
                       },

                       #liveLife:
                       liveLife = function(tPause){
                        while(private$Alive & private$EventQueue$firstTime() < tPause){
                          self$oneEvent(tPause)
                          if(private$EventQueue$get_queueN()==0){
                            break()
                          }
                        }

                       },

                       #################################################
                       # Life Events
                       #################################################

                       # event_maxDeath: the maximum death time event package
                       event_maxDeath = function(tEvent = 73000, PAR = NULL, tag = "death"){
                         list(tEvent = tEvent, PAR = PAR, tag = tag)
                       },

                       # death: the death event
                       death = function(tEvent, PAR){
                         self$track_history(tEvent = tEvent, event = "D")
                         private$Alive = FALSE
                       },

                       #################################################
                       # Auxiliary Functions
                       #################################################

                       track_history = function(tEvent, event){
                         private$History$track_history(tEvent, event)
                       },

                       get_history = function(){
                         return(private$History$get_history())
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
