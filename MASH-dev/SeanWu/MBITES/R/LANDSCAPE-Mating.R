###############################################################################
#         __                    __
#        / /   ____ _____  ____/ /_____________ _____  ___
#       / /   / __ `/ __ \/ __  / ___/ ___/ __ `/ __ \/ _ \
#      / /___/ /_/ / / / / /_/ (__  ) /__/ /_/ / /_/ /  __/
#     /_____/\__,_/_/ /_/\__,_/____/\___/\__,_/ .___/\___/
#                                            /_/
#     Landscape-Resource-Mating Swarm
#     MBITES Team
#     February 2018
#
###############################################################################


###############################################################################
# Mating Resource Class
###############################################################################

#' Mating Resource Base Class
#'
#' A \code{Mating_Resource} is a type of resource at a \code{\link[MBITES]{Site}} where mosquitoes travel mating.
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
#'  * MatingQ: a closure of male swarms (see \code{\link[MBITES]{make_MatingQ}}
#'
#' @export
Mating_Resource <- R6::R6Class(classname = "Mating_Resource",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,
                 inherit = MBITES:::Resource,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(w){
                     futile.logger::flog.trace("Mating_Resource being born at: self %s , private %s",pryr::address(self),pryr::address(private))

                     super$initialize(w,NULL) # construct base-class parts

                     self$MatingQ = make_MatingQ()
                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     futile.logger::flog.trace("Mating_Resource being born at: self %s , private %s",pryr::address(self),pryr::address(private))

                     self$MatingQ = NULL
                   }, # end destructor

                   # closure fields
                   MatingQ = NULL # closure of male swarms

                 ),

                 # private members
                 private = list(

                   w = numeric(1) # search weight

                 )

) # end Mating_Resource class definition


###############################################################################
# Mating Queue Object
###############################################################################

#' Make a Mating Queue
#'
#' Generates a closure that contains the following fields:
#'  * id: integer vector of males at this mating queue
#'  * weight: numeric vector of male mating weights
#'
#' The closure also contains the following functions:
#'  * add2Q(id_n,weight_n): add a new mosquito to the queue
#'  * clearQ(): clear the mating queue
#'  * sampleQ(): sample a mosquito id from the queue
#'  * printQ(): print the mating queue
#'
#' @export
make_MatingQ <- function(){

  # data for the closure
  id <- integer(1)
  weight <- numeric(1)
  NEW <- TRUE

  # add2Q: add a mosquito to the queue
  add2Q <- function(id_n,weight_n){
    # new MatingQ
    if(NEW){
      id <<- id_n
      weight <<- weight_n
      NEW <<- FALSE
    # not a new MatingQ
    } else {
      ix <- match(x = id_n,table = id)
      # mosquito already in the mating queue (update weight)
      if(!is.na(ix)){
        weight[ix] <<- weight_n
      # new mosquito to be added to the mating queue
      } else {
        id <<- append(id,id_n)
        weight <<- append(weight,weight_n)
      }
    }

  }

  # clearQ: set elements of queue to NULL
  clearQ <- function(){
    id <<- NULL
    weight <<- NULL
  }

  # sampleQ: sample a mosquito id from the queue
  sampleQ <- function(){
    # check for empty queue, return -10L if empty
    out <- -10L
    if(length(id)==0L){
      return(out)
    } else {
      MBITES::sample(x = id,size = 1,replace = FALSE,prob = weight)
    }
  }

  # printQ: print the queue
  printQ <- function(){
    cat("printing a mating queue ... \n")
    print(id)
    print(weight)
  }

 list(add2Q = add2Q, rmFromQ = rmFromQ,clearQ = clearQ,sampleQ = sampleQ,printQ = printQ)
}
