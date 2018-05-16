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
#' A \code{Aqua_Resource} is a type of resource at a \code{\link{Site}} where mosquitoes travel for oviposition of eggs
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
#'  * w: a numeric search weight
#'  * site: a reference to a \code{\link{Site}} object
#'
#' @section **Methods**:
#'  * add_egg: function that must take an egg batch and add it to the \code{EggQ}
#'  * one_day: function that updates daily aquatic population dynamics
#'  * push_imago: function that takes emerging imagos from the \code{ImagoQ} and pushes them to the adult mosquito population
#'  * reset: function that resets the aquatic habitat between simulation runs
#'
#' @section **Fields**:
#'  * EggQ: a closure of egg batches (see \code{\link{make_EggQ}})
#'  * ImagoQ: a closure of imago cohorts (see \code{\link{make_ImagoQ}})
#'
#' @export
Aqua_Resource <- R6::R6Class(classname = "Aqua_Resource",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,
                 inherit = MBITES:::Resource,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(w, site){
                     futile.logger::flog.trace("Aqua_Resource being born at: self %s , private %s",pryr::address(self),pryr::address(private))

                     super$initialize(w,site) # construct the base-class parts

                     # set local closures
                     self$EggQ = make_EggQ()
                     self$ImagoQ = make_ImagoQ()

                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     futile.logger::flog.trace("Aqua_Resource being killed at: self %s , private %s",pryr::address(self),pryr::address(private))

                     self$EggQ = NULL
                     self$ImagoQ = NULL
                   }, # end destructor

                   add_egg = function(){
                     stop("add_egg should never be called from abstract base class 'Aqua_Resource'!")
                   },

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
                 private = list()

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
    cat("printing a egg queue ... \n")
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
#'  * female: boolean vector indicating female or male cohort
#'  * times: numeric vector of imago emergence times
#'
#' The closure also contains the following functions:
#'  * add2Q(batch_n,time_e,imago_f): add a new imago cohort of size \code{imago_n} and boolean sex \code{imago_f} to the queue that will emerge at \code{time_e}
#'  * popQ(time_e,imago_f): return a vector of imago cohorts that are due to emerge by \code{time_e} and of boolean sex \code{imago_f}
#'  * printQ(): print the imago queue
#'
#' @export
make_ImagoQ <- function(){

  # data for the closure
  imagos <- integer(1)
  female <- logical(1)
  times <- numeric(1)
  NEW <- TRUE

  # add2Q: add a batch to the queue
  add2Q <- function(imago_n,time_e,imago_f){
    # new ImagoQ
    if(NEW){
      imagos <<- c(-99L,imago_n)
      female <<- c(NA,imago_f)
      times <<- c(2e16,time_e)
      NEW <<- FALSE
    # not a new ImagoQ
    } else {
      imagos <<- append(imagos,imago_n)
      female <<- append(female,imago_f)
      times <<- append(times,time_e)
    }
  }

  # popQ: remove imagos from the queue
  popQ <- function(time_e){
    ix <- which(times <= time_e) # which imagos are ready to go
    out_imago <- 0L # return 0 if no imagos ready to go or queue is depleted
    out_sex <- NA
    if(length(ix)>0){
      out_imago <- imagos[ix] # prepare them as the return value
      out_sex <- female[ix]
      imagos <<- imagos[-ix] # delete from queue
      female <<- female[-ix]
      times <<- times[-ix]
    }
    return(list(imagos=out_imago,female=out_sex)) # return imagos
  }

  # printQ: print the queue
  printQ <- function(){
    cat("printing a imago queue ... \n")
    print(imagos)
    print(female)
    print(times)
  }

 list(add2Q = add2Q, popQ = popQ,printQ = printQ)
}
