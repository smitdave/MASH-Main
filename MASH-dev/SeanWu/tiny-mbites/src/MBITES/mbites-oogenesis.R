###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES - oogenesis & refeeding
#     Sean Wu
#     March 2019
#
###############################################################################

# oogenesis
oogenesis <- function(mosy){
  # zombie mosquitos cant make eggs, also they cant make them on an empty stomach
  if(mosy$statenext != "D"){

    eggBatch(mosy)

    checkEggMaturation(mosy)
  }

}

# time-to-maturation
eggBatch_time <- function(mosy){

  if(fequal(mosy$batch,0) & mosy$bloodfed){
    mosy$batch <- rBatchSize(mosy)
    mosy$eggT <- mosy$tnow + rEggMaturationTime()
  }

  # mosy$bmSize <- 0
  mosy$bloodfed <- FALSE

}

checkEggMaturation_time <- function(mosy){

  # check the time
  if(mosy$eggT <= mosy$tnow){
    mosy$gravid <- TRUE
  } else {
    mosy$gravid <- FALSE
  }

}

rEggMaturationTime_norm <- function(){
  emt_m <- get("parameters",.GlobalEnv)$emt_m
  emt_sd <- get("parameters",.GlobalEnv)$emt_sd
  max(0,rnorm(1,emt_m,emt_sd))
}

rEggMaturationTime_0 <- function(){0}

rEggMaturationTime <- rEggMaturationTime_0

# provision-to-maturation
eggBatch_provision <- function(mosy){

  if(fequal(mosy$batch,0) & mosy$bloodfed){
    mosy$batch <- rBatchSize(mosy)

    bloodPerEgg <- get("parameters",.GlobalEnv)$bloodPerEgg
    mosy$eggP <- bloodPerEgg * mosy$bmSize
  }

  mosy$eggP <- mosy$eggP - mosy$bmSize
  # mosy$bmSize <- 0
  mosy$bloodfed <- FALSE

}

checkEggMaturation_provision <- function(mosy){

  # check the provision
  if(mosy$eggP <= 0){
    mosy$gravid <- TRUE
  } else {
    mosy$gravid <- FALSE
  }

}

# size of egg batch
rBatchSize_bms <- function(mosy){
  maxBatch <- get("parameters",.GlobalEnv)$maxBatch
  ceiling(mosy$bmSize * maxBatch)
}

rBatchSize_norm <- function(mosy){
  bs_m <- get("parameters",.GlobalEnv)$bs_m
  bs_sd <- get("parameters",.GlobalEnv)$bs_sd
  maxBatch <- get("parameters",.GlobalEnv)$maxBatch
  min(ceiling(rnorm(1,bs_m,bs_sd)),maxBatch)
}

# rBatchSize: set this function to one of the two above; or
rBatchSize <- rBatchSize_bms



# refeeding
refeed <- function(mosy){

  p <- pReFeed(mosy)
  if(runif(1) < p){
    mosy$gravid <- FALSE
    mosy$statenext <- "B"
  } else {
    mosy$statenext <- "O"
  }

  mosy$bmSize <- 0

}

pReFeed_batch <- function(mosy){
  rf_a <- get("parameters",.GlobalEnv)$rf_a
  rf_b <- get("parameters",.GlobalEnv)$rf_b
  batchP = mosy$batch / get("parameters",.GlobalEnv)$maxBatch
  (2+rf_b)/(1+rf_b) - exp(rf_a * batchP)/(rf_b + exp(rf_a * batchP))
}

pReFeed_bm <- function(mosy){
  rf_a <- get("parameters",.GlobalEnv)$rf_a
  rf_b <- get("parameters",.GlobalEnv)$rf_b
  (2+rf_b)/(1+rf_b) - exp(rf_a * mosy$bmSize)/(rf_b + exp(rf_a * mosy$bmSize))
}

pReFeed <- pReFeed_bm
