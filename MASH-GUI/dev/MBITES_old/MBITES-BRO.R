#################################################################
#
#   MASH/MBITES
#   MBITES-BRO
#   Blood Feeding, Resting, Oviposition Life Cycle Model
#   Sean Wu
#   May 5, 2017
#
#################################################################

##########################################
# Blood Feeding Attempt Bout
##########################################
boutF = function(P){with(P,{
  if(rbinom(1,1,bfa.p)){
    if(rbinom(1,1,bfa.s)){
      "R"
    } else {
      "F"
    }
  } else {
    "D"
  }
})}

##########################################
# Post Prandial Resting
##########################################
boutR = function(P){with(P,{
  if(rbinom(1,1,ppr.p)){
    if(rbinom(1,1,reFeed)){
      "F"
    } else {
      "L"
    }
  } else {
    "D"
  }
})}

##########################################
# Egg Laying Attempt Bout
##########################################
boutL = function(P){with(P,{
  if(rbinom(1,1,ela.p)){
    if(rbinom(1,1,ela.s)){
      "F"
    } else {
      "L"
    }
  } else {
    "D"
  }
})}

##########################################
# DHM - Basic :: One Mosquito
##########################################

DHM.Basic = function(P){
  S = hist.S = boutF(P)
  T = hist.T = 0
  feed.T = feed.N = Hfeed.N = Hfeed.T = 0

  while(S != "D"){

    ##########################################
    # Blood Feeding Attempt Bout
    ##########################################
    while(S == "F"){
      S = boutF(P)
      T = T + rexp(1,1/P$bfa.t)
      hist.S = c(hist.S,S)
      hist.T = c(hist.T,T)
    }

    ##########################################
    # Bionomic Counters
    ##########################################
    if(S == "R"){
      feed.T = c(feed.T, T)
      feed.N = feed.N + 1
      if(rbinom(1,1,P$Q)){
        Hfeed.N = Hfeed.N + 1
        Hfeed.T = c(Hfeed.T, T)
      }
    }

    ##########################################
    # Post Prandial Resting
    ##########################################
    while(S == "R"){
      S = boutR(P)
      hist.S = c(hist.S,S)
      T = T + rexp(1,1/P$ppr.t)
      hist.T = c(hist.T,T)
    }


    ##########################################
    # Egg Laying Attempt Bout
    ##########################################
    while(S == "L"){
      S = boutL(P)
      hist.S = c(hist.S,S)
      T = T + rexp(1,1/P$ela.t)
      hist.T = c(hist.T,T)
    }
  }
  list(S=hist.S, T=hist.T, f = diff(feed.T[-1]), a = diff(Hfeed.T[-1]), Hfeed.N=Hfeed.N, feed.N=feed.N, libfa.span = T)
}

##########################################
# Run DHM-Basic on a Cohort of M mosquitoes
# and Summarize their Basic Bionomics
##########################################
DHM.Basic.Cohort= function(P, M=1000){
  smry = list(1)
  for(i in 1:M)
    smry[[i+1]] = DHM.Basic(P)

  f.i=a.i=N=H=L=0
  for(i in 1:M){
    SS = smry[[i+1]]
    if(length(SS$f)>0) f.i = c(f.i,SS$f)
    if(length(SS$a)>0) a.i = c(a.i,SS$a)
    N = c(N,SS$feed.N)
    H = c(H,SS$Hfeed.N)
    L = c(L,SS$libfa.span)
  }
  f=f.i[-1]; a=a.i[-1]; N=N[-1]; L=L[-1]; H=H[-1]
  list(data = list(f=f,a=a,N=N,L=L), smry = list(f=mean(f), a=mean(a), N=mean(N), H=mean(H), L=mean(L)))
}

##########################################
# Plot the output of DHM.Basic.Cohort
##########################################
plot.DHM.Basic.Cohort = function(cohortOut){with(cohortOut,{
  par(mfrow = c(2,2))
  with(data,{
    hist(f,main = paste("BM Interval = ", signif(smry$f,4)))
    hist(a,main = paste("Human BM Interval =", signif(smry$a,4)))
    hist(N,main = paste("Human BM = ", signif(smry$H,4)))
    hist(L,main = paste("Lifespan = ", signif(smry$L,4)))
  })
  par(mfrow=c(1,1))
})}

DHM.Basic.derivs = function(t,y,p){
  with(as.list(c(y,p)),{
    dF = ela.p*ela.s*L/ela.t + ppr.p*reFeed*R/ppr.t - bfa.s*F/bfa.t
    dR = bfa.p*bfa.s*F/bfa.t - R/ppr.t
    dL = ppr.p*ppr.p*R/ppr.t - ela.s*L/ela.t
    list(c(dF,dR,dL))
  })
}

DHM.Basic.ode = function(P, Tx=50, plotit="n"){

  y = c(F=1, R=0, L=0)
  Age = seq(0, Tx, by=.1)
  out = data.frame(ode(y, Age, DHM.Basic.derivs,P))

  par(mfrow=c(1,2))
  with(out,{
    alive = F+R+L
    plot(Age, alive, type = "l", xlab = "Age (in Days)", ylab = "Proportion Surviving")
    matplot(out[,c("F","R","L")],type="l",col = )
  })
  par(mfrow=c(1,1))
}

DHM.Basic.plotODE = function(odeOut,P){

}

DHM.Basic.Pset = function(
  ##########################################
  # Blood Feeding Attempt
  ##########################################
  bfa.p = .9,   # Blood Feed Attempt . Prob Survives (Base)
  bfa.s = .3,   # Blood Feed Attempt . Prob Succeeds
  bfa.t = 0.65, # Blood Feed Attempt . Mean Time Elapsed (in Days)
  Q = 0.9,      # Proportion of Bites on Humans

  ##########################################
  # Post Prandial Resting
  ##########################################
  ppr.p = .9,   # Resting . Prob Survives (Base)
  ppr.t = 4/5,  # Resting . Mean Time Elapsed (in Days)
  reFeed = 0,   # The Probability of Re-Feeding

  ##########################################
  # Egg Laying Attempt
  ##########################################
  ela.p = .95,   # Egg Laying Attempt . Prob Survives (Base)
  ela.s = .7,   # Egg Laying Attempt . Prob Success
  ela.t = 1    # Egg Laying Attempt . Mean Time Elapsed
){
  list(
    ##########################################
    # Blood Feeding Attempt
    ##########################################
    bfa.p=bfa.p, bfa.s=bfa.s, bfa.t=bfa.t, Q=Q,

    ##########################################
    # Post Prandial Resting
    ##########################################
    ppr.p=ppr.p, ppr.t=ppr.t, reFeed=reFeed,

    ##########################################
    # Egg Laying Attempt
    ##########################################
    ela.p=ela.p, ela.s=ela.s, ela.t=ela.t
  )
}

####################################
#  Run the Code (example)
####################################

# PP = DHM.Basic.Pset(ela.p=.8)
# DHM.Basic(PP)
# test = DHM.Basic.Cohort(PP)
# print(test)
# plot.DHM.Basic.Cohort(test)
# require(deSolve)
# DHM.Basic.ode(PP, Tx = 60, plotit="p")
