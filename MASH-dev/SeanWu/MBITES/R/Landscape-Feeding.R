###############################################################################
#         __                    __
#        / /   ____ _____  ____/ /_____________ _____  ___
#       / /   / __ `/ __ \/ __  / ___/ ___/ __ `/ __ \/ _ \
#      / /___/ /_/ / / / / /_/ (__  ) /__/ /_/ / /_/ /  __/
#     /_____/\__,_/_/ /_/\__,_/____/\___/\__,_/ .___/\___/
#                                            /_/
#     Landscape-Resource-Feeding
#     MBITES Team
#     February 2018
#
###############################################################################


###############################################################################
# Feeding Resource Class
###############################################################################

#' Landscape Feeding Resource Class
#'
#' A \code{Feeding_Resource} is a type of resource at a \code{\link[MBITES]{Site}} where mosquitoes can expect to find
#' human or other vertebrate hosts when seeking a blood meal.
#'
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
#'  * field: im a field!
#'
#' @export
Feeding_Resource <- R6::R6Class(classname = "Feeding_Resource",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(w){

                     private$w = w

                     private$RiskQ = make_RiskQ()

                   } # end constructor

                   # begin destructor
                   finalize = function(){
                     self$RiskQ = NULL
                   }, # end destructor

                   # closure fields
                   RiskQ = NULL

                 ), # end public members

                 # private members
                 private = list(

                   w                  = numeric(1) # weight for this resource

                 ) # end private members


) # end Feeding_Resource class definition


###############################################################################
# Risk Queue Object
###############################################################################

#' Make a Risk Queue
#'
#' Generates a closure that contains the following fields:
#'  * id: integer vector of hosts at this risk queue
#'  * weight: numeric vector of host biting weights
#'  * time: numeric vector of aggregated host times at risk
#'
#' The closure also contains the following functions:
#'  * add2Q(id_n,weight_n,time_n): add a new host to the queue
#'  * rmFromQ(id_r): id of the host to remove from the queue
#'  * clearQ(): clear id, weight, time fields (set to \code{NULL})
#'  * sampleQ(): sample a host id from the queue
#'  * printQ(): print the risk queue
#'
#' @export
make_RiskQ <- function(){

  # data for the closure
  id <- integer(1)
  weight <- numeric(1)
  time <- numeric(1)
  NEW <- TRUE

  # add2Q: add a human to the queue
  add2Q <- function(id_n,weight_n,time_n){
    # new RiskQ
    if(NEW){
      id <<- id_n
      weight <<- weight_n
      time <<- time_n
      NEW <<- FALSE
    # not a new RiskQ
    } else {
      ix <- match(x = id_n,table = id)
      # person already in the risk queue (update weight and time)
      if(!is.na(ix)){
        weight[ix] <<- weight_n
        time[ix] <<- time_n
      # new person to be added to the risk queue
      } else {
        id <<- append(id,id_n)
        weight <<- append(weight,weight_n)
        time <<- append(time,time_n)
      }
    }

  }

  # rmFromQ: remove a human from the queue
  rmFromQ <- function(id_r){
    ix <- match(x = id_r,table = id)
    if(!is.na(ix)){
      id <<- id[-ix]
      weight <<- weight[-ix]
      time <<- time[-ix]
    }
  }

  # clearQ: set elements of queue to NULL
  clearQ <- function(){
    id <<- NULL
    weight <<- NULL
    time <<- NULL
  }

  # sampleQ: sample a human id from the queue
  sampleQ <- function(){
    # check for empty queue, return -10L if empty
    out <- -10L
    if(length(id)==0L){
      return(out)
    } else {
      MBITES::sample(x = id,size = 1,replace = FALSE,prob = weight*time)
    }
  }

  # printQ: print the queue
  printQ <- function(){
    cat("priting a risk queue ... \n")
    print(id)
    print(weight)
    print(time)
  }

 list(add2Q = add2Q, rmFromQ = rmFromQ,clearQ = clearQ,sampleQ = sampleQ,printQ = printQ)
}


###############################################################################
# Feeding Resource Methods
###############################################################################


#' Feeding_Resource: Get Resource Weight
#'
#' Get the weight associated to this resource.
#'  * binding: \code{Feeding_Resource$get_w}
#'
get_w_Feeding_Resource <- function(){
  return(private$w)
}

Feeding_Resource$set(which = "public",name = "get_w",
    value = get_w_Feeding_Resource, overwrite = TRUE
)
