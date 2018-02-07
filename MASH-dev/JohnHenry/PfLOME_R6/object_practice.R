library("R6")
source("PfLOME_Pathogen.R")
source("PfLOME_Human.R")
##human sources ImmuneState and HealthState classes
source("PfLOME_PfPedigree.R")
source("Rx.R")
##source ("eventTimeSeries.R") ##source this if you want to pull from mbites

############## artificial pedigree - will exist on tile ################

pfped = PfPedigree$new()

############## create human & parasite, add parasite ################
##############   parasite to pedigree, infect human  ################

someGuy = Human$new(1)
pf = Pf$new(1,1,1,TRUE) ##mic, mac, pfid, seed
pfped$add2Pedigree(pf)
someGuy$infectHuman(0,pf$get_pfid())
someGuy$get_Ptot()
someGuy$get_Gtot()

################ update infection for t days ########################

bites = 0
tt = 0
while(tt<365*10){
  bite = rgeom(1,3/365)
  bites = c(bites,bite)
  tt = cumsum(bites)[length(bites)]
}
bites = unique(cumsum(bites))
#bites = unique(sort(make.bites(70, 10, 1, 5, wt=wt, trend = .05)))
moi = 1+rnbinom(length(bites), mu=3, size = .3)

dt = 7
tFinal = 10*365
t = 1

while(t < tFinal){
  someGuy$updateHuman(t,dt)
  s = t
  while((s >= t) & s < t+dt){
    if(s %in% bites){
      k = which(bites==s)
      for(i in 1:moi[k]){
        pfped$increment_pfid()
        pfid = pfped$get_pfid()
        mic = sample(1:(pfid-1),1)
        mac = sample(1:(pfid-1),1)
        pf = Pf$new(mic,mac,pfid)
        pfped$add2Pedigree(pf)
        someGuy$infectHuman(s,pf$get_pfid())
      }
    }
    s = s+1
  }
  if(someGuy$get_Fever()>0){
    p = rbinom(1,1,.03)
    if(p == 1){
      someGuy$Treat(t,1)
    }
  }
  t = t+dt
}

######################### plotting functions #############################

tFinal = ifelse(dt%%2==1,tFinal-1,tFinal)
t = seq(1,tFinal,dt)/365
plot(t,someGuy$get_history()$Ptot,type="l",
     ylim=c(-5,11),xlim=c(0,10),xlab='years',ylab='log10 iRBC')
lines(t,someGuy$get_history()$Gtot,lty=2)
lines(t,someGuy$get_history()$Fever)
lines(t,2*someGuy$get_history()$GenImm-3,type="l")
lines(t,someGuy$get_history()$PD,col='purple')
abline(h=c(-1,-3),lty=2)
lines(t,someGuy$get_history()$PfMOI/max(someGuy$get_history()$PfMOI)*2-5)
abline(h=-5,lty=2)
text(.1,-3.5, paste("MOI, max=", max(someGuy$get_history()$PfMOI)), col = "blue", pos = 4)

plot(t,someGuy$get_history()$GenImm,type="l",xlab='years',ylab='% of max strength of immunity')
for(i in 4:someGuy$get_immuneState()$get_nBSImmCounters()){
  lines(c(0,t),someGuy$get_history()$BSImm[[i]],lty=2)
}

plot(1:length(someGuy$get_history()$PfMOI),someGuy$get_history()$PfMOI,type="l",
     xlab='days',ylab='MOI')

##RBC
plot(1:length(someGuy$get_history()$RBC),someGuy$get_history()$RBC,type="l",xlab ='days')
##HRP2
plot(1:length(someGuy$get_history()$HRP2),someGuy$get_history()$HRP2,type="l",xlab='days')
##pLDH
plot(1:length(someGuy$get_history()$pLDH),someGuy$get_history()$pLDH,type="l",xlab='days')
