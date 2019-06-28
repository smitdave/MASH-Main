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
