#################################################################
#
#   MASH/MBITES
#   Survival
#   Senescence, flight and resting survival
#   R version
#   Sean Wu
#   January 25, 2017
#
#################################################################


##########################################
# Flight Survival
##########################################

# getSFp: get baseline survival probability
getSFp <- function(M,P){
  with(P,{
    switch(M$state,
      F = F.p,
      B = B.p,
      R = R.p,
      L = L.p,
      O = O.p,
      M = M.p,
      S = S.p,
      E = 1
    )
  })
}

# surviveFlight:
surviveFlight <- function(M,P){
  if(isActive(M)){
    p = getSFp(M,P) # baseline survival
    if(P$TATTER){
      M$damage = M$damage + rTatterSize(P)
      p = p * pTatter(M,P)
    }
    if(P$SENESCE){
      p = p * pSenesce(M,P)
    }
    if(!rbinom(1,1,p)){
      M$stateNew = "D"
    }
  }
  return(M)
}


##########################################
# Resting Survival
##########################################

# myHaz: get landscape hazards
myHaz <- function(M){
  switch(P$model,
    # BRO - landscape
    bro.move = switch(M$inPointSet,
                      f = LANDSCAPE$feedSites[[M$ix]]$haz,
                      l = LANDSCAPE$aquaSites[[M$ix]]$haz
    ),
    # BRO - no landscape
    bro.nomove = switch(M$inPointSet,
                        f = P$feedHaz.mean,
                        l = P$aquaHaz.mean
    ),
    # FBRLO - landscape
    fbrlo.move = switch(M$inPointSet,
                        f = LANDSCAPE$feedSites[[M$ix]]$haz,
                        l = LANDSCAPE$aquaSites[[M$ix]]$haz
    ),
    # FBRLO - no landscape
    fbrlo.nomove = switch(M$inPointSet,
                          f = P$feedHaz.mean,
                          l = P$aquaHaz.mean
    ),
    # Full - landscape
    full.move = switch(M$inPointSet,
                         f = LANDSCAPE$feedSites[[M$ix]]$haz,
                         l = LANDSCAPE$aquaSites[[M$ix]]$haz,
                         m = LANDSCAPE$swarmSites[[M$ix]]$haz,
                         s = LANDSCAPE$sugarSites[[M$ix]]$haz
    ),
    # Full - no landscape
    full.nomove = switch(M$inPointSet,
                         f = P$feedHaz.mean,
                         l = P$aquaHaz.mean,
                         m = P$swarmHaz.mean,
                         s = P$sugarHaz.mean
    )
  )
}

surviveResting <- function(M,P){
  if(isActive(M)){
    if(!rbinom(1,1,myHaz(M))){
      M$stateNew = "D"
    }
  }

  return(M)
}


#############################################
#  Wing Tattering
#############################################

# rTatterSize: zero-inflated per-bout additive wing damage from tattering
rTatterSize <- function(P){
  with(P,{
    if(runif(1) > ttsz.p){
      return(0)
    } else {
      return(rbeta(1,ttsz.a,ttsz.b))
    }
  })
}

# pTatter: probability of death due to tattering
pTatter <- function(M,P){
  with(P,{
    return((2+ttr.b)/(1+ttr.b) - exp(M$damage*ttr.a)/(ttr.b + exp(M$damage*ttr.a)))
  })
}


#############################################
#  Senescence
#############################################

# pSenesce:
pSenesce <- function(M,P){
  age = M$tNow - M$bDay
  with(P,{
    return((2+sns.b)/(1+sns.b) - exp(sns.a*age)/(sns.b + exp(sns.a*age)))
  })
}
