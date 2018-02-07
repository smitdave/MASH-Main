#################################################################
#
#   MASH
#   R6-ified
#   Testing routines for PfSI and SimBitePfSI
#   Sean Wu
#   May 21, 2017
#
#################################################################

rm(list=ls())
library(MASH)

# setup class methods for PfSI and SimBite modules
PfSI.Setup()
SimBitePfSI.Setup()


#################################################################
# simulate a single human population
#################################################################

nHumans = 50
HumanPop_PAR = HumanPop.Parameters(nSite = 1,bWeight = NULL,siteSize = nHumans,siteMin = nHumans)
pop = HumanPop$new(HumanPop_PAR)
PfSI_PAR = PfSI.Parameters(TreatPf = 1)
pop$set_PfSI_PAR(PfSI_PAR = PfSI_PAR)
pop$init_MICRO_PfSI(PfPR = 0)

tMax = 365*5

pop$queueBites_SimBitePfSI(tMax = tMax,bitingRate = 1/15)
pop$queueVaccination_SimBitePfSI(tVaccine = (365*1),tTreat = (365*1)+1,fracPop = 0.75)
pop$simHumans(tPause = tMax+10)

pfsiHist = lapply(X = pop$get_Human(),FUN = function(x){x$get_Pathogens()$get_history()})
plot_PfSI(pfsiHist)


# #################################################################
# # simulate a single human population with negbinom biting
# # see ?queueBitesNegBinom_SimBitePfSI for details
# #################################################################
#
# rm(pop)
# pop = HumanPop$new(HumanPop_PAR)
# pop$PfSI.Init(PfPR = 0)
#
# tMax = 365*5
#
# pop$queueBitesNegBinom_SimBitePfSI(tMax = tMax, meanNumberBites = 100, plot = FALSE)
# pop$simHumans(tPause = tMax+10)
# pop$get_History()
#
#
# #################################################################
# # simulate many human populations in parallel
# #################################################################
#
# library(parallel)
# simPars = replicate(n = 10,expr = PfSI.Parameters(),simplify = FALSE)
# simParOut = parallel::mclapply(X = simPars,FUN = function(x,PAR){
#   pop = HumanPop$new(PAR)
#   pop$set_PfSI_PAR(x)
#   pop$init_MICRO_PfSI(PfPR = 0)
#   tMax = 365*5
#   pop$queueBites_SimBitePfSI(tMax = tMax,bitingRate = 1/15)
#   pop$queueVaccination_SimBitePfSI(tVaccine = (365*1),tTreat = (365*1)+1,fracPop = 0.75)
#   pop$simHumans(tPause = tMax+10)
#   pop$get_History()
# },PAR=HumanPop_PAR)
