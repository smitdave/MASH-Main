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

#' MASH-MACRO HumanPop Class Definition
#'
#' generate a single human population; they may live in a MacroPatch or a MicroPatch
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
#'  * **get_Human**: retrieve human whose field 'myID' matches argument 'humanID' in keylist of pop field.
#'  * item 1:
#'  * item 1:
#'  * item 1:
#'  * item 1:
#'
#' @md
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

                      initialize = function(patchID, houseIDs = NULL, bDays, bWeights, tStart = 0, verbose = FALSE){

                        if(length(patchID) > 1){stop("HumanPop constructor: patchID must be a single value; a HumanPop must be uniquely defined for each patch1")}
                        if(!is.null(houseIDs)){
                          if(!all.equal(length(houseIDs),length(bWeights),length(bDays))){stop("HumanPop constructor: houseIDs, bDays, bWeights must be vectors of equal length")}
                        } else {
                          if(!all.equal(length(bWeights),length(bDays))){stop("HumanPop constructor: bDays, bWeights must be vectors of equal length")}
                        }

                        private$pop = hash::hash()
                        private$nHumans = length(bDays)

                        for(i in 1:private$nHumans){

                          id = paste0(i,"_",patchID)
                          private$pop[[id]] = Human$new(myID = id, houseID = NULL, patchID = NULL, bDay = NULL, bWeight = NULL, verbose = verbose)

                        }

                      },

                      #################################################
                      # Getters & Setters
                      #################################################

                      # get_Human: get a human from a key
                      get_Human = function(humanID){
                        if(!is.character(humanID)){stop(paste0("humanID: ",humanID,"must be a character key"))}
                        return(private$pop[[humanID]])
                      },

                      # get_History: retrieve the entire population history or a single human's history
                      get_History = function(){
                        histories = vector(mode = "list",length = self$nHumans)
                        for(i in hash::keys(private$pop)){
                          histories[[i]] = private$pop[[i]]$get_History()
                        }
                        return(histories)
                      },

                      #################################################
                      # Simulation and Events
                      #################################################

                      simHumans = function(tPause){
                        for(i in hash::keys(private$pop)){
                          private$pop[[i]]$liveLife(tPause = tPause)
                        }
                      },

                      #################################################
                      # Values
                      #################################################

                      nHumans = NULL,
                      tStart = NULL

                    ),

                    # private members
                    private = list(

                      pop = NULL

                    )
)
