source("PfLOME_Human.R")

############## artificial pedigree - will exist on tile ################

pfped = PfPedigree$new()
#pfid = 0
#nAntigenLoci = 9
#nptypes = c(3,5,4,3,6,3,2,7,9)
mu = .01

someGuy = Human$new(1)
pf = Pf$new(1,1,1,1,TRUE)
pfped$add2Pedigree(pf)
someGuy$infectHuman(1,1)
