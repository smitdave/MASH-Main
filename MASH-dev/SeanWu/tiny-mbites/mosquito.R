# make a mosquito (just a hashed environment)
# to print it like a list, just as.list(mosquito)
make_mosquito <- function(bday, site, state, search){

  mosy <- list()

  # basic parameters
  mosy$id <- get("globals",.GlobalEnv)$get_mosy_id()
  mosy$alive <- TRUE

  # location
  mosy$site <- site
  mosy$rspot <- character(1)

  # timing
  mosy$bday <- bday
  mosy$tnow <- bday
  mosy$tnext <- bday

  # behavioral state parameters
  mosy$state <- state
  mosy$statenext <- state
  mosy$fail <- 0L

  # energetics and survival
  mosy$starved <- FALSE
  mosy$energy <- 1
  mosy$mature <- FALSE
  mosy$damage <- 0

  # history



  list2env(mosy,hash=TRUE)
}

# n: set up some reserve space for these vectors (default size is n)
# feed: detailed tracking of feeding?
make_history <- function(n = 20, feed = FALSE){

  hist <- list()

  hist$nevent <- 0L
  hist$time <- numeric(n)
  hist$site <- integer(n)
  hist$state <- character(n)
  hist$cod <- character(1) # for mosquito autopsies (cod: cause of death)

  if(feed){
    hist$feedt <-
  }
}
