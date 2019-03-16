# choose a habitat
choosehabitat <- function(mosy){

  id <- get("landscape",.GlobalEnv)[[mosy$site]]$id_l # habitat id's
  p <- get("landscape",.GlobalEnv)[[mosy$site]]$prob_l # habitat weights

  sample(x=id,size=1,prob=p)
}

layeggs_emerge <- function(mosy){
  mosy$gravid <- FALSE
  mosy$batch <- 0
  mosy$statenext <- "B"
}
