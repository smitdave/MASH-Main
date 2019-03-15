# make a mosquito (just a hashed environment)
# to print it like a list, just as.list(mosquito)
make_mosquito <- function(bday, site, state, search){

  mosy <- list()

  # basic parameters
  mosy$id <- get("globals",.GlobalEnv)$get_mosy_id()

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
  mosy$mated <- FALSE
  mosy$gravid <- FALSE

  # energetics and survival
  mosy$starved <- FALSE
  mosy$energy <- 1
  mosy$mature <- FALSE
  mosy$damage <- 0
  mosy$energyPreG <- 0

  # bloodfeeding & oogenesis
  mosy$hostID <- integer(1)
  mosy$bmSize <- 0
  mosy$batch <- 0
  mosy$eggT <- Inf
  mosy$eggP <- 0

  # history


  track_history(mosy) # track the emergence of this mosquito

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
    hist$nfeed <- 0L
    hist$feedtime <- numeric(n)
    hist$feedsite <- integer(n)
    hist$feedhost <- integer(n)
    hist$probefeed <- logical(n)
  }

  list2env(hist,hash=TRUE)
}

# basic history tracking
track_history <- function(mosy){

  mosy$hist$nevent <- mosy$hist$nevent + 1L

  nevent <- mosy$hist$nevent
  n <- length(mosy$hist$time)
  if(mosy$hist$nevent > n){
    mosy$hist$time <- c(mosy$hist$time,numeric(n))
    mosy$hist$site <- c(mosy$hist$site,integer(n))
    mosy$hist$state <- c(mosy$hist$state,character(n))
  }

  mosy$hist$time[nevent] <- mosy$tnext
  mosy$hist$site[nevent] <- mosy$site
  mosy$hist$state[nevent] <- mosy$statenext

}

# track feeding
track_probe <- function(mosy){

  mosy$hist$nfeed <- mosy$hist$nfeed + 1L

  n <- length(mosy$hist$feedtime)
  if(mosy$hist$nfeed > n){

  }

}
