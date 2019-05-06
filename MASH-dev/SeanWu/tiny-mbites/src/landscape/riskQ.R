# from the mbites package
# change from a closure to something more reasonable later; works for now
make_RiskQ <- function(){

  # data for the closure
  # human blood hosts
  n_h <- 0L
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
      n_h <<- 1L
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
        n_h <<- n_h + 1L
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
    id <<- integer(1)
    weight <<- numeric(1)
    time <<- numeric(1)
    NEW <<- TRUE
  } # end clearQ

  # sampleQ: sample a host id from the queue
  sampleQ <- function(){

    # if zoo hosts present
    if(n_o > 0L){

      # sample from both sets of hosts
      probs = weight*time
      probs = append(probs,weight_o)
      ids = c(id,id_o)
      sample(x = ids,size = 1,replace = FALSE,prob = probs)

    # no zoo hosts present
    } else {
      # check for empty queue
      if(n_h==0L){
        return(0L)
      # sample human hosts
      } else {
        sample(x = id,size = 1,replace = FALSE,prob = weight*time)
      }
    }

  } # end sampleQ

  # typewtsQ: return named numeric vector of weights for each type of host
  typewtsQ <- function(){

    # return object (Humans,Zoo,Trap,Other)
    # out <- c("H"=0,"Z"=0,"T"=0,"O"=0)
    out <- rep(0,4)
    # out[["H"]] <- sum(weight)
    # out[["Z"]] <- sum(weight_o)
    out[1] <- sum(weight)
    out[2] <- sum(weight_o)

    return(out)
  }

  # printQ: print the queue
  printQ <- function(){
    cat("printing a risk queue ... printing human hosts ... \n")
    print(id)
    print(weight)
    print(time)
    cat(" ... printing zoo hosts ... \n")
    print(id_o)
    print(weight_o)
  } # end printQ

 list(add2Q = add2Q, add2Q_zoo = add2Q_zoo, rmFromQ = rmFromQ,
      clearQ = clearQ,sampleQ = sampleQ,typewtsQ = typewtsQ,
      printQ = printQ)
}
