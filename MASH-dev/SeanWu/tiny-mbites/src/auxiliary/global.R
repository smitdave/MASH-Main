# global stuff
make_globals <- function(){

  out <- list()

  # mosquito IDs
  mosy_id <- 0L

  get_mosy_id <- function(){
    mosy_id <<- mosy_id + 1L
    return(mosy_id)
  }

  out$get_mosy_id <- get_mosy_id

  # simulation time
  tnow <- 0L

  get_tnow <- function(){
    return(tnow)
  }

  increment_tnow <- function(){
    tnow <<- tnow + 1L
  }

  out$get_tnow <- get_tnow
  out$increment_tnow <- increment_tnow

  list2env(out,hash=TRUE)
}
