###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES - logging and history
#     Sean Wu
#     March 2019
#
###############################################################################


###############################################################################
# basic history tracking for bionomics
###############################################################################

# n: set up some reserve space for these vectors (default size is n)
# feed: detailed tracking of feeding?
make_history <- function(n = 20, feed = FALSE, ovi = FALSE){

  hist <- list()

  hist$nevent <- 0L
  hist$time <- numeric(n)
  hist$site <- integer(n)
  hist$state <- character(n)
  hist$cod <- character(1) # for mosquito autopsies (cod: cause of death)

  # are we tracking feeding?
  hist$feed <- feed

  if(feed){
    hist$nfeed <- 0L
    hist$feedtime <- numeric(n)
    hist$feedsite <- integer(n)
    hist$feedhost <- integer(n)
    hist$probefeed <- logical(n)
  }

  # are we tracking oviposition?
  hist$ovi <- ovi

  if(ovi){
    hist$negg <- 0L
    hist$eggtime <- numeric(n)
    hist$eggsite <- integer(n)
    hist$eggbatch <- numeric(n)
  }

  # list2env(hist,hash=TRUE)
  return(hist)
}

# basic history tracking
track_history <- function(mosy){

  mosy$hist$nevent <- mosy$hist$nevent + 1L

  nevent <- mosy$hist$nevent
  n <- length(mosy$hist$time)
  if(nevent > n){
    mosy$hist$time <- c(mosy$hist$time,numeric(n))
    mosy$hist$site <- c(mosy$hist$site,integer(n))
    mosy$hist$state <- c(mosy$hist$state,character(n))
  }

  mosy$hist$time[nevent] <- mosy$tnext
  mosy$hist$site[nevent] <- mosy$site
  mosy$hist$state[nevent] <- mosy$statenext

}

# write output when im done
# no 'endsim' argument because we just toss out mosquitoes that are still alive at end of simulation
mbites_exit_female <- function(mosy){

  # dead on first bout; advance time (its cadlag, so they spent at least the first bout alive, die the instant they jump out of that state)
  if(mosy$hist$nevent == 1L){
    timing(mosy)
  }

  track_history(mosy)

  out <- mosy$hist

  # take out that flag
  out <- out[names(out)!="feed"]
  # take out that flag
  out <- out[names(out)!="ovi"]

  # write out
  cat(jsonlite::toJSON(x = out, pretty = TRUE),",\n",sep="",file=get("globals")$mbites_f_out)
}


###############################################################################
# detailed tracking of feeding events
###############################################################################

# track probing
track_probe <- function(mosy){

  mosy$hist$nfeed <- mosy$hist$nfeed + 1L

  nfeed <- mosy$hist$nfeed
  n <- length(mosy$hist$feedtime)
  if(nfeed > n){
    mosy$hist$feedtime <- c(mosy$hist$feedtime,numeric(n))
    mosy$hist$feedsite <- c(mosy$hist$feedsite,integer(n))
    mosy$hist$feedhost <- c(mosy$hist$feedhost,integer(n))
    mosy$hist$probefeed <- c(mosy$hist$probefeed,logical(n))
  }

  mosy$hist$feedtime[nfeed] <- mosy$tnow
  mosy$hist$feedsite[nfeed] <- mosy$hist$site[mosy$hist$nevent]
  mosy$hist$feedhost[nfeed] <- mosy$hostID
  mosy$hist$probefeed[nfeed] <- FALSE

}

# track feeding
track_feed <- function(mosy){
  mosy$hist$probefeed[nfeed] <- TRUE
}

# track resting
track_rest <- function(mosy){

  mosy$hist$nevent <- mosy$hist$nevent + 1L

  nevent <- mosy$hist$nevent
  n <- length(mosy$hist$time)
  if(nevent > n){
    mosy$hist$time <- c(mosy$hist$time,numeric(n))
    mosy$hist$site <- c(mosy$hist$site,integer(n))
    mosy$hist$state <- c(mosy$hist$state,character(n))
  }

  # add rest to history (use tnow because it is updated in timing_ppr)
  mosy$hist$time[private$nEvent] <- mosy$tnow
  mosy$hist$site[private$nEvent] <- mosy$hist$site[nevent-1L]
  mosy$hist$state[private$nEvent] <- "R"

}


###############################################################################
# detailed tracking of feeding events
###############################################################################


track_oviposit <- function(mosy){

  mosy$hist$negg <- mosy$hist$negg + 1L

  nevent <- mosy$hist$negg
  n <- length(mosy$hist$eggtime)
  if(nevent > n){
    mosy$hist$eggtime <- c(mosy$hist$eggtime,numeric(n))
    mosy$hist$eggsite <- c(mosy$hist$eggsite,integer(n))
    mosy$hist$eggbatch <- c(mosy$hist$eggbatch,character(n))
  }

  mosy$hist$eggtime[nevent] <- mosy$tnow
  mosy$hist$eggsite[nevent] <- mosy$site[mosy$hist$nevent]
  mosy$hist$eggbatch[nevent] <- mosy$batch

}
