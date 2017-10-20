#################################################################
#
#   MASH
#   R6-ified
#   Class definition for human
#   David Smith, Hector Sanchez, Sean Wu
#   May 19, 2016
#
#################################################################


##########################################
# Human Class Definition
##########################################

#' Human Class Definition
#'
#' This is a generic human being blah blah ...
#' Each instance of a \code{Human} lives in a \code{\link{HumanPop}}
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#' @details
#' \itemize{
#' \item{\code{reg}} - .
#' \item{\code{outvar}} - .
#' \item{\code{levels}} - .
#' \item{\code{nbins}} - .
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{new(reg, DataStorageClass.g0, ...)}}{...}
#'   \item{\code{fit(data)}}{...}
#'   \item{\code{predict(newdata)}}{...}
#'   \item{\code{predictAeqa(newdata)}}{...}
#' }
#' @section Active Bindings:
#' \describe{
#'   \item{\code{cats}}{...}
#' }
#' @export
Human <- R6::R6Class(classname="Human",
                     portable = TRUE,
                     cloneable = FALSE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #initialize
                       initialize = function(myID, hhID, bDay, bWeight){
                         private$myID = myID
                         private$hhID = hhID
                         private$bDay = bDay
                         private$bWeight = bWeight
                         private$EventQueue = MASH::HumanEventQ()
                       },

                       #################################################
                       # Getters and Setters
                       #################################################

                       #myID
                       get_myID = function(){
                         return(private$myID)
                       },

                       #hhID
                       get_hhID = function(){
                         return(private$hhID)
                       },
                       set_hhID = function(hhID){
                         private$hhID = hhID
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

                       # history
                       get_History = function(){
                         list(
                           events = private$events,
                           eventT = private$eventT
                         )
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

                       # Pointers to enclosing HumanPop$pop
                       get_PopPointer = function(){
                         return(private$PopPointer)
                       },
                       set_PopPointer = function(PopPointer){
                         private$PopPointer = PopPointer
                       },
                       # Pointers to enclosing HumanPop self
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
                         private$events = c(private$events, event)
                         private$eventT = c(private$eventT, tEvent)
                       }

                     ),

                     #private members
                     private = list(

                       #General Information
                       myID = NULL,
                       hhID = NULL,
                       Alive = TRUE,
                       bDay = NULL,
                       sex = NULL,
                       weight = 0,
                       height = 0,

                       #Event Queue
                       EventQueue = NULL,

                       # Event History
                       events = c("init"),
                       eventT = c(-1),

                       # Health & Related
                       bWeight = NULL,

                       # Pathogens
                       Pathogens = NULL,

                       # Pointers
                       PopPointer = NULL, # point to HumanPop$pop that encloses this human
                       HumansPointer = NULL # point to HumanPop's public methods that encloses this human

                     )

) #end class definition
