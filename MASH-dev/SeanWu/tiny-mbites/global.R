# global stuff
make_globals <- function(){

  out <- list()

  mosy_id <- 0L

  get_mosy_id <- function(){
    mosy_id <<- mosy_id + 1L
    return(mosy_id)
  }

  out$get_mosy_id <- get_mosy_id

  list2env(out,hash=TRUE)
}
