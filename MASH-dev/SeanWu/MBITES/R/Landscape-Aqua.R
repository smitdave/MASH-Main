###############################################################################
#         __                    __
#        / /   ____ _____  ____/ /_____________ _____  ___
#       / /   / __ `/ __ \/ __  / ___/ ___/ __ `/ __ \/ _ \
#      / /___/ /_/ / / / / /_/ (__  ) /__/ /_/ / /_/ /  __/
#     /_____/\__,_/_/ /_/\__,_/____/\___/\__,_/ .___/\___/
#                                            /_/
#     Landscape-Resource-Aquatic Habitat
#     MBITES Team
#     February 2018
#
###############################################################################


###############################################################################
# Aquatic Habitat Resource Class
###############################################################################

#' Aquatic Habitat Resource Base Class
#'
#' A \code{Aqua_Resource} is a type of resource at a \code{\link[MBITES]{Site}} where mosquitoes travel for oviposition of eggs
#' and from which new imagos (adult mosquitoes) emerge from. This abstract base class defines an interface which all models of aquatic ecology
#' must inherit from to generate concrete implementations of the interface methods.
#'
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
#'  * add_egg: function that must take an egg batch and add it to the \code{EggQ}
#'  * one_day: function that updates daily aquatic population dynamics
#'  * push_imago: function that takes emerging imagos from the \code{ImagoQ} and pushes them to the adult mosquito population
#'
#' @section **Fields**:
#'  * EggQ: a closure of egg batches (see \code{\link[MBITES]{make_EggQ}})
#'  * ImagoQ: a closure of imago cohorts (see \code{\link[MBITES]{make_ImagoQ}})
#'
#' @export
Aqua_Resource <- R6::R6Class(classname = "Aqua_Resource",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(){
                     self$EggQ = make_EggQ()
                     self$ImagoQ = make_ImagoQ()
                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     self$EggQ = NULL
                     self$ImagoQ = NULL
                   }, # end destructor

                   # one day of aquatic population
                   one_day = function(){
                     stop("one_day should never be called from abstract base class 'Aqua_Resource'!")
                   },

                   # send emerging imagos to adult population
                   push_imago = function(){
                     stop("push_imago should never be called from abstract base class 'Aqua_Resource'!")
                   },

                   # closure fields
                   EggQ = NULL, # closure of egg batches
                   ImagoQ = NULL # closure of imago cohorts

                 ),

                 # private members
                 private = list(

                   w = numeric(1) # search weight

                 )

) # end Aqua_Resource class definition


###############################################################################
# Egg Queue Object
###############################################################################

#' Make a Egg Queue
#'
#' Generates a closure that contains the following fields:
#'  * batches: integer vector of egg batch sizes
#'  * times: numeric vector of egg hatching times
#'
#' The closure also contains the following functions:
#'  * add2Q(batch_n,time_n): add a new egg batch to the queue
#'  * popQ(time_p): return a vector of egg batches that have hatched
#'  * printQ(): print the egg queue
#'
#' @export
make_EggQ <- function(){

  # data for the closure
  batches <- integer(1)
  times <- numeric(1)
  NEW <- TRUE

  # add2Q: add a batch to the queue
  add2Q <- function(batch_n,time_n){
    # new EggQ
    if(NEW){
      batches <<- c(-99L,batch_n)
      times <<- c(2e16,time_n)
      NEW <<- FALSE
    # not a new EggQ
    } else {
      batches <<- append(batches,batch_n)
      times <<- append(times,time_n)
    }

  }

  # popQ: remove batches from the queue
  popQ <- function(time_p){
    ix <- which(times <= time_p) # which batches are ready to go
    out <- 0L # return 0 if no eggs ready to go or queue is depleted
    if(length(ix)>0){
      out <- batches[ix] # prepare them as the return value
      batches <<- batches[-ix] # delete from queue
      times <<- times[-ix]
    }
    return(out) # return batches
  }

  # printQ: print the queue
  printQ <- function(){
    cat("priting a egg queue ... \n")
    print(batches)
    print(times)
  }

 list(add2Q = add2Q, popQ = popQ,printQ = printQ)
}


###############################################################################
# Imago Queue Object
###############################################################################

#' Make a Imago Queue
#'
#' Generates a closure that contains the following fields:
#'  * imagos: integer vector of emerging imago cohort sizes
#'  * times: numeric vector of imago emergence times
#'
#' The closure also contains the following functions:
#'  * add2Q(batch_n,time_n): add a new imago cohort to the queue
#'  * popQ(time_e): return a vector of imago cohorts that have emerged
#'  * printQ(): print the imago queue
#'
#' @export
make_ImagoQ <- function(){

  # data for the closure
  imagos <- integer(1)
  times <- numeric(1)
  NEW <- TRUE

  # add2Q: add a batch to the queue
  add2Q <- function(imago_n,time_n){
    # new ImagoQ
    if(NEW){
      imagos <<- c(-99L,imago_n)
      times <<- c(2e16,time_n)
      NEW <<- FALSE
    # not a new ImagoQ
    } else {
      imagos <<- append(imagos,imago_n)
      times <<- append(times,time_n)
    }

  }

  # popQ: remove imagos from the queue
  popQ <- function(time_e){
    ix <- which(times <= time_p) # which imagos are ready to go
    out <- 0L # return 0 if no imagos ready to go or queue is depleted
    if(length(ix)>0){
      out <- imagos[ix] # prepare them as the return value
      imagos <<- imagos[-ix] # delete from queue
      times <<- times[-ix]
    }
    return(out) # return imagos
  }

  # printQ: print the queue
  printQ <- function(){
    cat("priting a imago queue ... \n")
    print(imagos)
    print(times)
  }

 list(add2Q = add2Q, popQ = popQ,printQ = printQ)
}


###############################################################################
# Aquatic Habitat Resource Methods
###############################################################################

#' Aqua_Resource: Get Resource Weight
#'
#' Get the weight associated to this resource.
#'  * binding: \code{Aqua_Resource$get_w}
#'
get_w_Aqua_Resource <- function(){
  return(private$w)
}

Aqua_Resource$set(which = "public",name = "get_w",
    value = get_w_Aqua_Resource, overwrite = TRUE
)
