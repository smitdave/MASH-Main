# choose a habitat
choosehabitat <- function(mosy){

  id <- get("landscape",.GlobalEnv)[[mosy$site]]$id_l # habitat id's
  p <- get("landscape",.GlobalEnv)[[mosy$site]]$prob_l # habitat weights

  sample(x=id,size=1,prob=p)
}

layeggs_emerge <- function(mosy){

  # track_oviposit(mosy) # if you want

  mosy$gravid <- FALSE
  mosy$batch <- 0
  mosy$statenext <- "B"

  # set these flags to very large numbers
  mosy$eggT <- 2e16
  mosy$eggP <- 2e16
}
