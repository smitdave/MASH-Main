library("R6")
source("PfLOME_Human.R")
##human sources ImmuneState and HealthState classes
source("PfLOME_PfPedigree.R")
source("PfLOME_Pathogen.R")
##source ("eventTimeSeries.R") ##source this if you want to pull from mbites

############## artificial pedigree - will exist on tile ################

pfped = PfPedigree$new()

############## create human & parasite, add parasite ################
############## parasite to pedigree, infect human    ################

someGuy = Human$new(1)
pf = Pf$new(1,1,1,TRUE) ##mic, mac, pfid, seed
pfped$add2Pedigree(pf)
someGuy$infectHuman(0,pf$get_pfid())
someGuy$get_Ptot()
someGuy$get_Gtot()

################ update infection for 300 days ########################

bites = c(100,200) ## unique(sort(make.bites(5, 1, 1, 5, wt=wt, trend = .05)))
moi = 1+rnbinom(length(bites), mu=3, size = .3)
pfid = 1

for(t in 1:300){
  someGuy$updateHuman(t)
  if(t %in% bites){
    k = which(bites==t)
    for(i in 1:moi[k]){
      pfid <<- pfid+1
      pf = Pf$new(1,1,pfid)
      pfped$add2Pedigree(pf)
      someGuy$infectHuman(t,pf$get_pfid())
    }
  }
}

plot(1:length(someGuy$get_history()$Ptot),someGuy$get_history()$Ptot,type="l",ylim=c(0,11),xlim=c(0,300))
lines(1:length(someGuy$get_history()$Gtot),someGuy$get_history()$Gtot,lty=2)
lines(1:length(someGuy$get_history()$Fever),someGuy$get_history()$Fever)
pfped$get_PedLength()

someGuy$get_history()
