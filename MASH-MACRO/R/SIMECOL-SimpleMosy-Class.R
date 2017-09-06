###############################################################################
#
#      _____ _           ______           __
#     / ___/(_)___ ___  / ____/________  / /
#     \__ \/ / __ `__ \/ __/ / ___/ __ \/ /
#    ___/ / / / / / / / /___/ /__/ /_/ / /
#   /____/_/_/ /_/ /_/_____/\___/\____/_/
#
#   MASH-MACRO
#   SimEcol: SimpleMosquito Class Definition
#   MASH Team
#   September 1, 2017
#
###############################################################################


#' SimpleMosquito Class Definition
#'
#' Generate a SimpleMosquito object. A SimpleMosquito is a lightweight class for simulating very large populations of mosquitoes and is not intended to be used in M-BITES simulations.
#' SimpleMosquitoes are advanced through the life course as a generalized semi-Markov chain; if exponential distributions are used for waiting times then it simulates a trajectory of
#' a continuous-time Markov chain. Here chain is used to specify that the mosquito always has a finite state space. These mosquitoes do not track their history, they are just used to
#' simulate vessels for pathogen transmission.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * myID: character ID "patchID_myID"
#'  * myState: character state
#'  * tStart: time to begin my simulation (given in global time)
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
#' @md
#' @export
SimpleMosquito <- R6::R6Class(classname="MosquitoRM",
                     portable = TRUE,
                     cloneable = TRUE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #################################################
                       # Constructor
                       #################################################

                       initialize = function(myID, myState, tStart){

                         private$myID = myID
                         private$state = myState
                         private$tNow = tStart

                       }

                     ),

                     #private members
                     private = list(

                       # basic fields
                       myID = character(1),
                       myState = character(1),
                       tNow = numeric(1),
                       tNext = numeric(1),

                       # pointers
                       PatchPointer = NULL,
                       HumanPopPointer = NULL

                     )
)


###############################################################################
# Getters and Setters
###############################################################################


# #' Human: Gets ID
# #'
# #' Gets ID for human
# #'
# #' More details
# #'
# get_myID_human = function(){
# return(private$myID)
# }
#
# Human$set(which="public", name="get_myID",
# 	value=get_myID_human,
# 	overwrite=TRUE)
