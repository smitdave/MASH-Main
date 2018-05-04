library("R6")
source("PfLOME_Pathogen.R")
source("PfLOME_Human.R")
##human sources ImmuneState and HealthState classes
source("Rx.R")
##source ("eventTimeSeries.R") ##source this if you want to pull from mbites


########## simulates single infection for tfin number of days ##########

simPerson = function(person, tfin, dt, p, PQ){
  t = 1
  while(t < tfin){
    person$updateHuman(t,dt)
    if(person$get_Fever()>0){
      tau = rbinom(1,1,p)
      if(tau == 1){
        ## include testing? if negative, do they not treat with ACT/PQ?
        person$Treat(t,1)
        if(PQ == T){
          person$get_pathogen()$get_Pf()[[1]]$set_Gt(0)
          person$get_pathogen()$get_Pf()[[1]]$set_Gtt(rep(0,10))
          person$get_pathogen()$update_history(T)
        }
      }
    }
    t = t+dt
  }
}

simPerson2 = function(person, tfin, dt, PQ, Treat,Delay=0){
  FeverHist = 0
  TreatTime = -100
  while(t < tfin){
    person$updateHuman(t,dt)
    if((person$get_Fever()-FeverHist)>0 && Treat==T){
      FeverHist=1
      TreatTime = t+Delay
    }
    if(t == TreatTime-1 && person$get_Fever()==T){
      person$Treat(TreatTime,1)
    }
    if(PQ == T && t == TreatTime && person$get_Fever()==T){
      person$get_pathogen()$get_Pf()[[1]]$set_Gt(0)
      Ptt = person$get_pathogen()$get_Pf()[[1]]$get_Ptt()
      Ptt[1:5] = rep(0,5)
      person$get_pathogen()$get_Pf()[[1]]$set_Ptt(Ptt)
      person$get_pathogen()$update_history(T)
    }
    t = t+dt
  }
}

######################### plotting functions #############################

plotPerson = function(person,Gt=T,Fev=F){

    Ptot = rm.na(person$get_history()$Ptot)

    Gtot = rm.na(person$get_history()$Gtot)
    Gtot = c(rep(0,10),Gtot)

    Fever = person$get_history()$Fever

    PD = person$get_history()$PD[1:length(t)]

    plot(Ptot,type="l", ylim=c(0,13), xlim=c(0,365), xlab='days', ylab='log10 iRBC')
    if(Gt == T){
      lines(Gtot,lty=2)
    }
    if(Fev == T){
      lines(Fever,col="red")
    }
    lines(PD,col="purple")
}

rm.na = function(v){
  v = v[!is.na(v)]
  return(v)
}

################### example run ###################

############## create human & parasite ################
##############      infect human       ################

#person = Human$new(1,IncImm=F,IncPfPed=F)
#pf = Pf$new(1,1,1,TRUE) ##mic, mac, pfid, seed
#person$infectHuman(0,1)

########## update infection for tFinal days ################


PQsim = function(TR=.2,SS=1000){ ##treatment rate, sample size
  ##build containers for infectious period, with PQ (IPPQ) and control (IPC)
  IPPQ = rep(0,SS)
  IPC = rep(0,SS)
  
  ##run with PQ
  for(k in 1:SS){
    
    ##set simulation parameters
    t = 1
    dt = 1
    tFinal = 200
    p = .01
    PQ = T
    Treat = F
    if(k<floor(TR*SS)){Treat=T}
    
    ##instantiate human & set infection
    person = Human$new(1,IncImm=F,IncPfPed=F)
    pf = Pf$new(1,1,1,TRUE) ##mic, mac, pfid, seed
    person$infectHuman(0,1)
    
    ##run simulation, record infectious period
    simPerson2(person,tFinal,dt,p,PQ,Treat)
    IPPQ[k] = sum(person$get_history()$TE)
    
  }
  
  ##run without PQ
  for(k in 1:SS){
    
    ##set simulation parameters
    t = 1
    dt = 1
    tFinal = 200
    p = .01
    PQ = F
    Treat = F
    if(k<floor(TR*SS)){Treat=T}
    
    ##instantiate human & set infection
    person = Human$new(1,IncImm=F,IncPfPed=F)
    pf = Pf$new(1,1,1,TRUE) ##mic, mac, pfid, seed
    person$infectHuman(0,1)
    
    ##run simulation, record infectious period
    simPerson2(person,tFinal,dt,p,PQ,Treat)
    IPC[k] = sum(person$get_history()$TE)
    
  }
  
  
  m1 = mean(IPPQ)
  m2 = mean(IPC)
  
  return(m1/m2)
}

t = 1
dt = 1
tFinal = 220
PQ = F

person = Human$new(1,IncImm=F,IncPfPed=F)
person$infectHuman(0,1)
simPerson2(person,tFinal,dt,PQ,T)
plotPerson(person)

#lines(person$get_history()$TE,lty=2)
#sum(person$get_history()$TE)

PlotTE = function(person){
  TE = person$get_history()$TE
  plot(TE,ylim=c(0,1),xlab="Days",ylab="Transmission Efficiency",type="l")
}

## function to determine treatment rate for a given p, sample size SS
## the variation is too large, even for really large SS (tested for ~10^4)
TRtest = function(p,SS){
  TR = 0
  for(i in 1:SS){
    
    dt = 1
    tFinal = 365
    PQ = T
    
    person = Human$new(1,IncImm=F,IncPfPed=F)
    pf = Pf$new(1,1,1,TRUE) ##mic, mac, pfid, seed
    person$infectHuman(0,1)
    simPerson(person,tFinal,dt,p,PQ)
    if(sum(person$get_history()$PD)>0){
      TR = TR+1
    }
  }
  TR = TR/SS
  return(TR)
}

PlotTETot = function(person,TPQ){
  
  TE = person$get_history()$TE
  t = seq(1,length(TE))
  tetot = sum(TE)
  
  if(TPQ == 1){
    plot(t,TE,type="l",xlim=c(0,200),main="Untreated")
  }
  if(TPQ == 2){
    plot(t,TE,type="l",xlim=c(0,200),main="Treated, No Primaquine")
  }
  if(TPQ == 3){
    plot(t,TE,type="l",xlim=c(0,200),main="Treated, With Primaquine")
  }
  legend(150,.7,bquote(TE == .(tetot)))
  polygon(c(t,rev(t)),c(0*TE,rev(TE)),col="blue")
  
}

PlotEffectSize = function(ES,ES2,ES3){
  p = seq(0,1,.01)
  RES = 1-(ES*(1-p)+ES3*p)/(ES*(1-p)+ES2*p)
  plot(p,RES,type="l",ylim=c(0,1),xlab="Treatment Coverage Proportion",ylab="Proportional Reduction in TE",main="Relative Reduction in TE Due to PQ")
  abline(h=1-ES3/ES2,lty=2)
}
