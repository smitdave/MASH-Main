###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES - the mosquito object
#     Sean Wu
#     March 2019
#
###############################################################################

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
  mosy$search <- FALSE
  mosy$fail <- 0L
  mosy$mated <- FALSE
  mosy$gravid <- FALSE
  mosy$bloodfed <- FALSE

  # energetics and survival
  mosy$starved <- FALSE
  mosy$energy <- 1
  mosy$mature <- FALSE
  mosy$damage <- 0
  mosy$energyPreG <- 0

  # bloodfeeding & oogenesis
  mosy$hostID <- integer(1)
  mosy$bmSize <- 0
  mosy$habitatID <- integer(1)
  mosy$batch <- 0
  mosy$eggT <- Inf
  mosy$eggP <- 0

  # history
  mosy$hist <- make_history()


  track_history(mosy) # track the emergence of this mosquito

  list2env(mosy,hash=TRUE)
}
