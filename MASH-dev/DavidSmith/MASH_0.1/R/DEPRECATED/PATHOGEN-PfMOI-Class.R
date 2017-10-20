#################################################################
#
#   MASH-R6
#   PATHOGEN component
#   PfMOI module R6 class definitions
#   David Smith, Hector Sanchez, Sean Wu
#   June 9, 2017
#
#################################################################


#################################################################
# Mosquito Stage
#################################################################

#' PfMOI Mosquito-stage Pathogen Class Definition
#'
#' This is the mosquito-stage PfMOI module pathogen object defined in \code{\link{MicroMosquitoFemale}}.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section Methods:
#'  * **Constructor**
#'    * new: initialize a new \code{Landscape} object
#'      * Arguments:
#'        * \code{Landscape_PAR}: see \code{\link{Landscape.Parameters}} for parameter generation function.
#'  * **Getters & Setters**
#'    * get_XX: get \code{private$XX}
#'      * Arguments:
#'        * \code{ixS}: if \code{NULL} return all sites, otherwise return site indexed by \code{ixS}
#'    * set_FeedingSites: set \code{private$FeedingSites}
#'      * Arguments:
#'        * \code{FeedingSites}: replacement object (see below)
#'        * \code{ixS}: if \code{NULL} replace all sites, otherwise replace site indexed by \code{ixS}
#'  * **Pointers**
#'    * get_TilePointer: get \code{\link{MicroTile}} pointer
#'    * set_TilePointer: set \code{\link{MicroTile}} pointer
#'
#'
#'
#'
#'
#' @md
#' @export
mosquitoPfMOI <- R6::R6Class(classname="mosquitoPfMOI",
                     portable = TRUE,
                     cloneable = FALSE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #initialize
                       initialize = function(PfID = NULL, tInf = NULL, MOI = 0L, damID = NULL, sireID = NULL){
                         private$PfID = PfID
                         private$tInf = tInf
                         private$MOI = MOI
                         private$damID = damID
                         private$sireID = sireID
                       },

                       ########################################
                       #  Getters & Setters
                       ########################################

                       # MOI: Multiplicity of Infection
                       get_MOI = function(){
                         return(private$MOI)
                       },
                       set_MOI = function(MOI){
                         private$MOI = MOI
                       },
                       increment_MOI = function(){
                         private$MOI = private$MOI + 1
                       },

                       # get_clone: get the mth clonal variant as a list
                       get_clone = function(m){
                         list(
                           PfID = private$PfID[m],
                           damID = private$damID[m],
                           sireID = private$sireID[m]
                          )
                       },

                       ########################################
                       #  Pointers
                       ########################################

                       set_MosquitoPointer = function(MosquitoPointer){
                         private$MosquitoPointer = MosquitoPointer
                       },
                       get_MosquitoPointer = function(){
                         return(private$MosquitoPointer)
                       }


                     ),

                     #private members
                     private = list(

                       # Fields
                       MOI = NULL,
                       PfID = NULL,
                       sireID = NULL,
                       damID = NULL,

                       # Pointers
                       MosquitoPointer = NULL


                     )

) #end class definition


#################################################################
# Human Stage
#################################################################

#' PfMOI Human-stage Pathogen Class Definition
#'
#' This is the human-stage PfMOI module pathogen object defined in \code{\link{Human}}.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section Methods:
#'  * **Constructor**
#'    * new: initialize a new \code{Landscape} object
#'      * Arguments:
#'        * \code{Landscape_PAR}: see \code{\link{Landscape.Parameters}} for parameter generation function.
#'  * **Getters & Setters**
#'    * get_XX: get \code{private$XX}
#'      * Arguments:
#'        * \code{ixS}: if \code{NULL} return all sites, otherwise return site indexed by \code{ixS}
#'    * set_FeedingSites: set \code{private$FeedingSites}
#'      * Arguments:
#'        * \code{FeedingSites}: replacement object (see below)
#'        * \code{ixS}: if \code{NULL} replace all sites, otherwise replace site indexed by \code{ixS}
#'  * **Pointers**
#'    * get_TilePointer: get \code{\link{MicroTile}} pointer
#'    * set_TilePointer: set \code{\link{MicroTile}} pointer
#'
#' @section Fields:
#'  * **History**
#'    * tEvent: time of the event (jump time for embedded CTMC)
#'    * events: the new state (state after the jump)
#'    * MOI: current multiplicity of infection
#'
#' @md
#' @export
humanPfMOI <- R6::R6Class(classname="humanPfMOI",
                     portable = TRUE,
                     cloneable = FALSE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #initialize
                       initialize = function(PfID = NULL, tInf = NULL, MOI = 0L, b = 0.55, c = 0.15, damID = NULL, sireID = NULL){
                         if(MOI>0){
                           private$infected = TRUE
                         } else {
                           private$infected = FALSE
                         }
                         private$MOI = MOI
                         private$history$MOI = MOI
                         private$PfID = PfID
                         private$tInf = tInf
                         private$b = b
                         private$c = c
                         private$damID = damID
                         private$sireID = sireID
                         private$chemoprophylaxis = FALSE
                       },

                       ########################################
                       #  Infection Dynamics
                       ########################################

                       # add a new infection
                       add_Infection = function(PfID, damID, sireID){
                         private$damID = c(private$damID,damID)
                         private$sireID = c(private$sireID,sireID)
                         private$PfID = c(private$PfID,PfID)
                         private$MOI = private$MOI + 1L
                         if(!private$infected){
                           private$infected = TRUE
                         }
                       },

                       # completely clear the infection assoc. with index ix
                       clear_Infection = function(ix){
                         private$damID = private$damID[-ix]
                         private$sireID = private$sireID[-ix]
                         private$PfID = private$PfID[-ix]
                         private$MOI = private$MOI - 1L
                         if(private$MOI==0){
                           private$infected = FALSE
                         }
                       },

                       ########################################
                       #  Getters & Setters
                       ########################################

                       # infected: am I infected by at least one clonal variant?
                       get_infected = function(){
                         return(private$infected)
                       },
                       set_infected = function(infected){
                         private$infected = infected
                       },

                       # MOI: Multiplicity of Infection
                       get_MOI = function(){
                         return(private$MOI)
                       },
                       set_MOI = function(MOI){
                         private$MOI = MOI
                       },
                       increment_MOI = function(){
                         private$MOI = private$MOI + 1L
                       },

                       # PfID: Pf IDs; indicate liver-stage infections
                       get_PfID = function(){
                         return(private$PfID)
                       },
                       set_PfID = function(PfID){
                         private$PfID = PfID
                       },
                       push_PfID = function(PfID){
                         private$PfID = c(private$PfID,PfID)
                       },

                       # tInf: when was I infected with this clonal variant?
                       get_tInf = function(){
                         return(private$tInf)
                       },
                       set_tInf = function(tInf){
                         private$tInf = tInf
                       },
                       push_tInf = function(tInf){
                         private$tInf = c(private$tInf,tInf)
                       },

                       # b: infected mosquito to human transmission efficiency
                       get_b = function(){
                         return(private$b)
                       },
                       set_b = function(b){
                         private$b = b
                       },

                       # c: infected human to mosquito transmission efficiency
                       get_c = function(){
                         return(private$c)
                       },
                       set_c = function(c){
                         private$c = c
                       },

                       # sireID:
                       get_sireID = function(){
                         return(private$sireID)
                       },
                       set_sireID = function(sireID){
                         private$sireID = sireID
                       },
                       push_sireID = function(sireID){
                         private$sireID = c(private$sireID,sireID)
                       },

                       # damID:
                       get_damID = function(){
                         return(private$damID)
                       },
                       set_damID = function(damID){
                         private$damID = damID
                       },
                       push_damID = function(damID){
                         private$damID = c(private$damID,damID)
                       },

                       # chemoprophylaxis: am I currently protected by chemoprophylaxis?
                       get_chemoprophylaxis = function(){
                         return(private$chemoprophylaxis)
                       },
                       set_chemoprophylaxis = function(chemoprophylaxis){
                         private$chemoprophylaxis = chemoprophylaxis
                       },

                       # return the mth clonal variant (needed for human to mosquito transmission)
                       get_clone = function(m){
                         print("get the mth clonal variant as a list")
                         list(

                          )
                       },

                       ########################################
                       #  History
                       ########################################

                       track_history = function(eventT , event){
                         private$history$events = c(private$history$events,event)
                         private$history$eventT = c(private$history$eventT,eventT)
                         private$history$MOI = c(private$history$MOI,private$MOI)
                       },
                       get_history = function(){
                         return(private$history)
                       },

                       ########################################
                       #  Pointers
                       ########################################

                       # HumanPointer: point to the human I exist in!
                       get_HumanPointer = function(){
                         return(private$HumanPointer)
                       },
                       set_HumanPointer = function(HumanPointer){
                         private$HumanPointer = HumanPointer
                       }


                     ),

                     #private members
                     private = list(

                       # Pathogen and immune states
                       infected = NULL, # boolean flag; TRUE if MOI > 0
                       MOI = NULL, # my multiplicity of infection
                       PfID = NULL, # vector of PfID
                       tInf = NULL, # vector of infection times
                       b = NULL, # infected mosquito to human transmission efficiency
                       c = NULL, # infected human to mosquito transmission efficiency
                       chemoprophylaxis = NULL,
                       damID = NULL, # vector of female gametocyte parents of this clonal strain
                       sireID = NULL, # vector of male gametocyte parents of this clonal strain
                       history = list(events="init",eventT=-1L,MOI=NULL),

                       # Pointers
                       HumanPointer = NULL

                     )

) #end class definition
