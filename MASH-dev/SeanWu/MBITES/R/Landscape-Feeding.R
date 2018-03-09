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
                 inherit = MBITES:::Resource,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(w,site){
                     futile.logger::flog.trace("Feeding_Resource being born at: self %s , private %s",pryr::address(self),pryr::address(private))

                     super$initialize(w,site) # construct base-class parts

                     self$RiskQ = make_RiskQ()

                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     futile.logger::flog.trace("Feeding_Resource being killed at: self %s , private %s",pryr::address(self),pryr::address(private))

                     self$RiskQ = NULL
                   }, # end destructor

                   # closure fields
                   RiskQ = NULL

                 ), # end public members

                 # private members
                 private = list() # end private members


) # end Feeding_Resource class definition


###############################################################################
# Risk Queue Object
###############################################################################

#' Make a Risk Queue
#'
#' Generates a closure that contains the following fields:
#'  * id: integer vector of human hosts at this risk queue
#'  * weight: numeric vector of human host biting weights
#'  * time: numeric vector of aggregated human host times at risk
#'  * n_o: integer count of zoo host (or generic blood host) types at this risk queue
#'  * id_o: integer vector of zoo hosts (or generic blood host) at this risk queue
#'  * weight_o: numeric vector of zoo host (or generic blood host) biting weights
#'
#' The closure also contains the following functions:
#'  * add2Q(id_n,weight_n,time_n): add a new human host to the queue
#'  * add2Q(id_n,weight_n): add a new zoo host (or generic blood host) to the queue
#'  * rmFromQ(id_r): id of the host to remove from the queue
#'  * clearQ(): clear id, weight, time fields (set to \code{NULL})
#'  * sampleQ(): sample a host id from the queue
#'  * printQ(): print the risk queue
#'
#' @export
make_RiskQ <- function(){

  # data for the closure
  # human blood hosts
  id <- integer(1)
  weight <- numeric(1)
  time <- numeric(1)
  NEW <- TRUE
  # zoo blood hosts
  n_o <- 0L
  id_o <- integer(1)
  weight_o <- numeric(1)
  NEWZOO <- TRUE

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
  } # end add2Q

  # add2Q_zoo: add a zoo blood host (or generic host) to a queue
  add2Q_zoo <- function(id_n,weight_n){
    # check id is negative integer
    if(id_n > 0){
      stop("when adding zoo hosts to a risk queue the id must be < 0; input: ",id_n)
    }
    # new RiskQ
    if(NEWZOO){
      n_o <<- 1L
      id_o <<- id_n
      weight_o <<- weight_n
      NEWZOO <<- FALSE
    # not a new RiskQ
    } else {
      ix <- match(x = id_n,table = id_o)
      # zoo host already in the risk queue
      if(!is.na(ix)){
        weight_o[ix] <<- weight_n
      # new zoo host to be added to the risk queue
      } else {
        n_o <<- n_o + 1L
        id_o <<- append(id_o,id_n)
        weight_o <<- append(weight_o,weight_n)
      }
    }
  } # end add2Q_zoo

  # rmFromQ: remove a human from the queue
  rmFromQ <- function(id_r){
    ix <- match(x = id_r,table = id)
    if(!is.na(ix)){
      id <<- id[-ix]
      weight <<- weight[-ix]
      time <<- time[-ix]
    }
  } # end rmFromQ

  # clearQ: set elements of queue to NULL
  clearQ <- function(){
    id <<- NULL
    weight <<- NULL
    time <<- NULL
  } # end clearQ

  # sampleQ: sample a host id from the queue
  sampleQ <- function(){

    # if zoo hosts present
    if(n_o > 0L){

      # sample from both sets of hosts
      probs = weight*time
      probs = append(probs,weight_o)
      ids = c(id,id_o)
      return(MBITES::sample(x = ids,size = 1,replace = FALSE,prob = probs))

    # no zoo hosts present
    } else {
      # check for empty queue
      if(length(id)==0L){
        return(0L)
      # sample human hosts
      } else {
        return(MBITES::sample(x = id,size = 1,replace = FALSE,prob = weight*time))
      }
    }

  } # end sampleQ

  # printQ: print the queue
  printQ <- function(){
    cat("printing a risk queue ... humans first \n")
    print(id)
    print(weight)
    print(time)
    cat(" ... printing zoo hosts ... \n")
    print(id_o)
    print(weight_o)
  } # end printQ

 list(add2Q = add2Q, add2Q_zoo = add2Q_zoo, rmFromQ = rmFromQ,clearQ = clearQ,sampleQ = sampleQ,printQ = printQ)
}
