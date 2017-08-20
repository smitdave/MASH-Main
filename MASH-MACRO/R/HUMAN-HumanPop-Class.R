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
#' @section **Constructor**:
#'  * item 1:
#'
#' @section **Methods**:
#'  * get_Human: retrieve human whose field 'myID' matches argument 'humanID' in keylist of pop field.
#'  * item 1:
#'  * item 1:
#'  * item 1:
#'  * item 1:
#'
#' @section **Fields**:
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

                      initialize = function(N, patchID, houseIDs = NULL, bDays, bWeights, tStart = 0){

                        if(length(patchID) > 1){stop("HumanPop constructor: patchID must be a single value; a HumanPop must be uniquely defined for each patch")}
                        if(!is.null(houseIDs)){
                          if(!all.equal(N,length(bWeights),length(bDays))){stop("HumanPop constructor: houseIDs, bDays, bWeights must be vectors of equal length")}
                        } else {
                          if(!all.equal(length(bWeights),length(bDays))){stop("HumanPop constructor: bDays, bWeights must be vectors of equal length")}
                        }
                        if(tStart != 0){print("warning: tStart is not 0, make sure you really want to do this")}

                        private$pop = MASHcpp::HashMap$new(N = N)
                        private$N = N

                        for(i in 1:private$N){

                          id = paste0(i,"_",patchID)
                          private$pop$assign(key=id,value=Human$new(myID = id, houseID = houseIDs[i], patchID = patchID, bDay = bDays[i], bWeight = bWeights[i]))

                        }

                      },

                      #################################################
                      # Getters & Setters
                      #################################################

                      # get_Human: get a human from a 'myID'
                      get_Human = function(humanID){
                        return(private$pop$get(key=humanID))
                      },

                      # get_History: retrieve the entire population history or a single human's history
                      get_history = function(){
                        return(
                          private$pop$eapply(tag="get_history",returnVal=TRUE)
                        )
                      },

                      #################################################
                      # Simulation and Events
                      #################################################

                      simHumans = function(tPause){
                        private$pop$eapply(tag="liveLife",returnVal=FALSE,tPause=tPause)
                      }

                    ),

                    # private members
                    private = list(

                      N = NULL,
                      tStart = NULL,

                      #
                      pop = NULL

                    )
)
