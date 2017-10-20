#################################################################
#
#   MASH
#   R6-ified
#   Class definition for human population
#   David Smith, Hector Sanchez, Sean Wu
#   December 10, 2016
#
#################################################################

#' HumanPop Class Definition
#'
#' This is a a human population blah blah ...
#'
#'
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
HumanPop <- R6::R6Class(classname = "HumanPop",
                    portable = TRUE,
                    cloneable = FALSE,
                    lock_class = FALSE,
                    lock_objects = FALSE,

                    # public members
                    public = list(

                      # initialize
                      initialize = function(HumanPop_PAR){

                        with(HumanPop_PAR,{

                          private$pop = vector(mode="list",length = nHumans)
                          for(ixH in 1:nHumans){
                            private$pop[[ixH]] = Human$new(myID = humanIDs[ixH], hhID = homeIDs[ixH], bDay = bDay[ixH], bWeight = bWeight[ixH])
                            # set pointers
                            private$pop[[ixH]]$set_PopPointer(private$pop)
                            private$pop[[ixH]]$set_HumansPointer(self)
                          }

                          # one day these may need to be active bindings to automatically look these values up when called;
                          # this may need to be done when we have people immigrating and emigrating from patches
                          self$nHumans = nHumans          # size of human population
                          self$tStart = tStart      # time to start simulating
                          self$homeIDs = homeIDs        # vector of home IDs corresponding to that human's home
                          self$humanIDs = humanIDs          # vector of human IDs
                        })

                      },

                      #################################################
                      # Accessor Methods
                      #################################################

                      # get_Pop: retrieve the entire population or a single member
                      get_Human = function(ixH = NULL){
                        if(is.null(ixH)){
                          return(private$pop)
                        } else {
                          if(!any(self$humanIDs == ixH)){ stop(paste0("index: ",ixH," does not match any ID!"))}
                          return(private$pop[[ixH]])
                        }
                      },

                      # get_History: retrieve the entire population history or a single human's history
                      get_History = function(ixH = NULL){
                        if(is.null(ixH)){
                          histories = vector(mode = "list",length = self$nHumans)
                          for(i in 1:self$nHumans){
                            histories[[i]] = private$pop[[i]]$get_History()
                          }
                          return(histories)
                        } else {
                          if(!any(self$humanIDs == ixH)){ stop(paste0("index: ",ixH," does not match any ID!"))}
                          return(private$pop[[ixH]]$get_History())
                        }
                      },

                      #################################################
                      # Simulation and Events
                      #################################################

                      simHumans = function(tPause){
                        for(i in 1:self$nHumans){
                          private$pop[[i]]$liveLife(tPause = tPause)
                        }
                      },

                      #################################################
                      # Values
                      #################################################

                      nHumans = NULL,
                      tStart = NULL,
                      homeIDs = NULL,
                      humanIDs = NULL

                    ),

                    # private members
                    private = list(

                      pop = NULL

                    )
)
