# parameters
make_parameters <- function(
  # timing
  time_BFSB = 1/(6/24),
  time_BFAB = 1/(3/24),
  time_ELSB = 1/(6/24),
  time_ELAB = 1/(3/24),
  time_ppr = 1/(18/24),
  # ppr survival
  ppr_a = 15,
  ppr_b = 300,
  # host encounters
  surviveH                = 1,
  probeH                  = 1,
  surviveprobeH           = 1,
  feedH                   = 1,
  surviveZ                = 1,
  feedZ                   = 1,
  # disperse
  disperse = 0,
  # blood meal size
  bm_a                 = 7.5,
  bm_b                 = 2.5,
  # overfeeding
  of_a                 = 5,
  of_b                 = 5e3,
  # Energetics
  energyPreG           = 0,
  preGsugar            = 0,
  energyFromBlood      = 0.25,
  S_u                  = 1/7,
  # landing & resting spot
  InAndOut             = matrix(data = c(4,2,2,1,6,
                                       2,1,1,1,4,
                                       1,1,1,1,2,
                                       0,0,0,0,1,
                                       1,1,2,1,0),
                                       nrow = 5,ncol = 5,byrow = TRUE,dimnames = list(c("i","w","v","r","l"),c("i","w","v","r","l"))),
  rwts = matrix(data = 1,nrow = 4,ncol = 5,dimnames = list(c("F","B","L","O"),c("i","w","v","r","l"))),
  rspot = c("i","w","v","r","l"),
  # baseline survival
  F_surv = 0.95,
  B_surv = 0.98,
  L_surv = 0.90,
  O_surv = 0.98,
  M_surv = 0.98,
  S_surv = 0.98,
  # energy-related survival
  S_a                  = 20, # mbites_pEnergySurvival
  S_b                  = 10, # mbites_pEnergySurvival
  # wing tattering
  ttsz_p               = 0.5, # wing tattering (size of damage)
  ttsz_a               = 5,
  ttsz_b               = 95,
  ttr_a                = 15, # wing tattering (probability of death)
  ttr_b                = 500,
  # senescence
  sns_a                = 0.085, # senescence parameters
  sns_b                = 100, # senescence parameters
  # oogenesis
  bs_m = 50,
  bs_sd = 5,
  maxBatch = 60,
  bloodPerEgg = 0.05,
  emt_m                = 3, # mean of normally dist. maturation time
  emt_sd               = 1, # sd of normally dist. maturation time
  # refeeding
  rf_a                 = 10, # refeeding probability
  rf_b                 = 3,



){

  out <- list()

  # timing
  out$time_BFSB <- time_BFSB
  out$time_BFAB <- time_BFAB
  out$time_ELSB <- time_ELSB
  out$time_ELAB <- time_ELAB
  out$time_ppr <- time_ppr

  # ppr survival
  out$ppr_a <- ppr_a
  out$ppr_b <- ppr_b

  # host encounters
  out$surviveH <- surviveH
  out$probeH <- probeH
  out$surviveprobeH <- surviveprobeH
  out$feedH <- feedH
  out$surviveZ <- surviveZ
  out$feedZ <- feedZ

  # disperse
  out$disperse <- disperse

  # blood meal size
  out$bm_a <- bm_a
  out$bm_b <- bm_b

  # overfeeding
  out$of_a <- of_a
  out$of_b <- of_b

  # energetics
  out$energyPreG <- energyPreG
  out$preGsugar <- preGsugar
  out$energyFromBlood <- energyFromBlood
  out$S_u <- S_u

  # baseline survival
  out$F_surv <- F_surv
  out$B_surv <- B_surv
  out$L_surv <- L_surv
  out$O_surv <- O_surv
  out$M_surv <- M_surv
  out$S_surv <- S_surv

  # energy survival
  out$S_a <- S_a
  out$S_b <- S_b

  # wing tattering
  out$ttsz_p <- ttsz_p
  out$ttsz_a <- ttsz_a
  out$ttsz_b <- ttsz_b
  out$ttr_a <- ttr_a
  out$ttr_b <- ttr_b

  # senescence
  out$sns_a <- sns_a
  out$sns_b <- sns_b

  # oogenesis
  out$bs_m <- bs_m
  out$bs_sd <- bs_sd
  out$maxBatch <- maxBatch
  out$bloodPerEgg <- bloodPerEgg
  out$emt_m <- emt_m
  out$emt_sd <- emt_sd

  # refeeding
  out$rf_a <- rf_a
  out$rf_b <- rf_b

  # return a hash table
  list2env(out,hash=TRUE)
}
