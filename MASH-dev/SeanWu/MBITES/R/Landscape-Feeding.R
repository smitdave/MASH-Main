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

                   } # end constructor

                 ),

                 # private members
                 private = list(

                   w                  = numeric(1), # weight for this resource

                   RiskQ              = NULL


                 )


) # end Feeding_Resource class definition


###############################################################################
# Risk Queue Object
###############################################################################

make_RiskQ <- function(){

  # data for the closure
  id <- integer(1)
  weight <- numeric(1)
  time <- numeric(1)

  return(
    list(
      add2Q = function(id,weight,time){
        ix <- match(x = id,table = id)
        # person already in the risk queue
        if(length(ix)>0L){
          weight[ix] <<- weight
          time[ix] <<- time
        # new person to be added to the risk queue
        } else {
          id <<- append(id,id)
          weight <<- append(weight,weight)
          time <<- append(time,time)
        }
      },
      rmFromQ = function(id){
        ix <- match(x = id,table = id)
        if(length(ix)>0L){
          id <<- id[-ix]
          weight <<- weight[-ix]
          time <<- time[-ix]
        }
      },
      sampleQ = function(){
        return(sample(x = id,size = 1,replace = FALSE,prob = weight*time))
      },
      printQ = function(){
        cat("priting a risk queue ... \n")
        print(get("ix",envir = parent.env(environment())));print(get("weight",envir = parent.env(environment())));print(get("time",envir = parent.env(environment())))
      }
    )
  )
}

make_RiskQ <- function(){

  # data for the closure
  id <- integer(1)
  weight <- numeric(1)
  time <- numeric(1)

  # add2Q: add a human to the queue
  add2Q <- function(id,weight,time){
    ix <- match(x = id,table = id)
    # person already in the risk queue
    if(length(ix)>0L){
      weight[ix] <<- weight
      time[ix] <<- time
    # new person to be added to the risk queue
    } else {
      id <<- append(id,id)
      weight <<- append(weight,weight)
      time <<- append(time,time)
    }
  }

  # rmFromQ: remove a human from the queue
  rmFromQ <- function(id){
    ix <- match(x = id,table = id)
    if(length(ix)>0L){
      id <<- id[-ix]
      weight <<- weight[-ix]
      time <<- time[-ix]
    }
  }

  # sampleQ: sample a human id from the queue
  sampleQ <- function(){
    return(MBITES::sample(x = id,size = 1,replace = FALSE,prob = weight*time))
  }

  # printQ: print the queue
  printQ <- function(){
    cat("priting a risk queue ... \n")
    print(get("ix"));print(get("weight"));print(get("time"))
  }

  return(
    list(
      add2Q = add2Q,
      rmFromQ = rmFromQ,
      sampleQ = sampleQ,
      printQ = printQ
    )
  )
}


make_RiskQ <- function() {

  # data for the closure
  id <- integer(1)
  weight <- numeric(1)
  time <- numeric(1)

  # add2Q: add a human to the queue
  add2Q <- function(id,weight,time){
    ix <- match(x = id,table = id)
    # person already in the risk queue
    if(length(ix)>0L){
      weight[ix] <<- weight
      time[ix] <<- time
    # new person to be added to the risk queue
    } else {
      id <<- append(id,id)
      weight <<- append(weight,weight)
      time <<- append(time,time)
    }
  }

  # rmFromQ: remove a human from the queue
  rmFromQ <- function(id){
    ix <- match(x = id,table = id)
    if(length(ix)>0L){
      id <<- id[-ix]
      weight <<- weight[-ix]
      time <<- time[-ix]
    }
  }

  # sampleQ: sample a human id from the queue
  sampleQ <- function(){
    return(sample(x = id,size = 1,replace = FALSE,prob = weight*time))
  }

  # printQ: print the queue
  printQ <- function(){
    cat("priting a risk queue ... \n")
    print(dynGet("ix"))
    print(dynGet("weight"))
    print(dynGet("time"))
  }


 list(add2Q = add2Q, rmFromQ = rmFromQ,sampleQ=sampleQ,printQ=printQ)
}

queue1 <- make_RiskQ()
queue2 <- make_RiskQ()
queue1$add2Q(5L,52.32,1.32)
queue1$add2Q(1L,7.092,7.54)
queue1$printQ()


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
