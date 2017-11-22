source("PfLOME_Human.R")

############## artificial pedigree - will exist on tile ################

pfped = PfPedigree$new()
#pfid = 0
#nAntigenLoci = 9
#nptypes = c(3,5,4,3,6,3,2,7,9)
mu = .01 ## mutation rate (most likely should be in pf object)

someGuy = Human$new(1)
pf = Pf$new(1,1,1,TRUE) ##mic, mac, pfid, seed
pfped$add2Pedigree(pf)
someGuy$infectHuman(0,pf$get_pfid())
someGuy$get_Ptot()
someGuy$get_Gtot()
pfped$get_PedLength()


for(t in 1:300){
  someGuy$updateHuman(t)
}

plot(1:300,someGuy$get_history()$Ptot,type="l",ylim=c(0,11))