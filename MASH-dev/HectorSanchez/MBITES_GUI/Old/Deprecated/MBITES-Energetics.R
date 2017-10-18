#################################################################
#
#   MASH/MBITES
#   Energetics
#   Blood meals, sugar feeding, and egg production
#   R version
#   Sean Wu
#   January 25, 2017
#
#################################################################


##########################################
# Sugar Energetics
##########################################

# energetics:
energetics <- function(M,P){
  if(isAlive(M)){

    energyNew = M$energy - P$S.u
    M$energy = max(energyNew,0)

    if(rbinom(1,1,1-pEnergySurvival(M$energy,P$S.a,P$S.b))){
      M$stateNew = "D"
      return(M)
    }

    if(P$model %in% c("full.move","full.nomove")){
      M = queueSugarBout(M,P)
    }

  }

  return(M)
}

# pEnergySurvival: Incremental mortality as a function of energy reserves
pEnergySurvival <- function(energy, S.a, S.b){
  return(exp(S.a*energy)/(S.b + exp(S.a*energy)))
}

# rEnergySurvive: boolean survival as function of energy reserves
rEnergySurvive <- function(s, S.a, S.b){
  rbinom(n=1,size=1,pSugarSurvival(s,S.a, S.b))
}

# pSugarBout: probability to queue sugar bout as function of energy reserves
pSugarBout <- function(energy, S.sa, S.sb){
  return((2+S.sb)/(1+S.sb)-exp(S.sa*energy)/(S.sb+exp(S.sa*energy)))
}

# queueSugarBout:
queueSugarBout <- function(M,P){
  with(P,{
    if(runif(1) < pSugarBout(M$energy,S.sa,S.sb)){
      M$stateNew = "S"
    }
    return(M)
  })
}


##########################################
# Bloodmeal Energetics
##########################################

# rBloodMealSize
rBloodMealSize <- function(P){
  #. rBloodMealSize
  with(P,{
    return(rbeta(n=1,bm.a,bm.b))
  })
}

# bloodEnergetics
bloodEnergetics <- function(M,P){
  #. bloodEnergetics:
  with(P,{
    M$energy = max(1, (M$energy + B.energy))

    if(!M$mature){
      M$energyPreG = M$energyPreG - preGsugar
      if(M$energyPreG <= 0){
        M$mature = TRUE
      }
    }

    return(M)
  })
}

# BloodMeal:
BloodMeal <- function(M,P){
  M$bmSize = rBloodMealSize(P)
  M = bloodEnergetics(M,P)
  if(P$OVERFEED){ # overfeed
    M = overFeed(M,P)
    if(!isAlive(M)){
      return(M)
    }
  }
  # mated and mature mosquitoes produce eggs
  if(M$mated & M$mature){
    M$batch = switch(P$batchSize,
                     bms = BatchSize.bms(M,P),
                     norm = BatchSize.norm(M,P)
    )
    eggMaturationTime = switch(P$eggMatT,
                               off = eggMaturationTime.off(P),
                               norm = eggMaturationTime.norm(P)
    )
    M$eggT = M$tNow + eggMaturationTime
  }

  return(M)
}



##############################
#  Overfeed
##############################

pOverFeed <- function(bmSize, of.a, of.b){
  exp(of.a*bmSize)/(of.b + exp(of.a*bmSize))
}

rOverFeed <- function(bmSize, of.a, of.b){
  return(rbinom(1,1,pOverFeed(bmSize, of.a, of.b)))
}

overFeed <- function(M,P){
  with(P,{
    if(rOverFeed(M$bmSize, of.a, of.b)){
      M$bStateNew = "D"
    }
    return(M)
  })
}


##############################
#  Refeed
##############################

# reFeed: probability to re-enter blood feeding cycle after incomplete blood feeding
reFeed <- function(M,P){
  if(isAlive(M)){
    if(P$REFEED){
      M$stateNew = rReFeed(M,P)
    } else if(M$mated & M$batch > 0){
      M$stateNew = "L"
    } else {
      stop("mosquito is mature but either not mated or has no eggs")
    }

  }

  return(M)
}

pReFeed <- function(bmSize, rf.a, rf.b){
  (2+rf.b)/(1+rf.b) - exp(rf.a*bmSize)/(rf.b + exp(rf.a*bmSize))
}

rReFeed <- function(M,P){
  with(P,{
    if(rbinom(1,1,pReFeed(M$bmSize,rf.a,rf.b))){
      return("B")
    } else {
      return("L")
    }
  })
}


######################################
#  Egg Batch
######################################

BatchSize.norm <- function(M,P){
  ceiling(rnorm(1, P$bs.m, P$bs.v))
}

BatchSize.bms <- function(M,P){
  return(M$bmSize*P$maxBatch)
}

eggMaturationTime.norm <- function(P){
  max(0,rnorm(1, P$emt.m, P$emt.V))
}

eggMaturationTime.off <- function(P){
  return(0)
}

makeBatches <- function(M){
  #. makeBatches: make an egg batch and deposit on the landscape
  #batch: size of egg batch
  #ixM: mosquito ID
  #tNow: current time
  addBatch2Q(M$batch, M$ix, M$tNow, M$id, M$sire) # aquaticEcology.R
}
