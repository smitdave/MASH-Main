#################################################################
#     _____   __ _____ _____
#    |  __ \ / _/ ____|_   _|
#    | |__) | || (___   | |
#    |  ___/|  _\___ \  | |
#    | |    | | ____) |_| |_
#    |_|    |_||_____/|_____|
#
#################################################################

rm(list=ls())
gc()
library(MASH)

seed = 43
set.seed(seed)

# initialize classes for MACRO
MACRO.Humans.Setup(pathogenModule = "PfSI")

# setup class methods for PfSI and SimBite modules
# MACRO relies on SimBitePfSI module for adding bites to humans
PfSI.Setup(mnPEPf = 365, vrPEPf = 30)
SimBitePfSI.Setup()

# MACRO Patch initialization
MACRO.Patch.Emerge.Setup() # 'Emerge' model

patchPops <- c(500, 500,500)
popTotal <- sum(patchPops)

nPatch = length(patchPops)

# lambda values?
ll = 50*2
# Set up tile parameters
tileParameters = MACRO.Tile.Parameters(N = nPatch,
                                       aquaModule = "emerge", aquaPars = list(N=nPatch,lambda=rep(ll, nPatch)),
                                       pathogenModule = "PfSI")
# Manually set tile parameters
tileParameters$HumanPop_PAR$nHumans <- popTotal
tileParameters$HumanPop_PAR$sitePops <- patchPops[1:nPatch] # might need this to be a double not an int
hIDs <- sample(as.integer(1:popTotal)) # a randomly ordered vector, so vaccines are set randomly
tileParameters$HumanPop_PAR$humanIDs <- hIDs
tileParameters$HumanPop_PAR$siteHumanIDs <- split(hIDs, rep(1:nPatch, patchPops[1:nPatch]))
tileParameters$HumanPop_PAR$homeIDs <- rep(1:nPatch, patchPops[1:nPatch])
sa <- siteAges(popTotal)
tileParameters$HumanPop_PAR$bDay <- -sa
tileParameters$HumanPop_PAR$steAges <- split(sa, rep(1:nPatch, patchPops[1:nPatch]))
tileParameters$HumanPop_PAR$bWeight <- rgamma(popTotal, 1)
tileParameters$MacroMosquitoPop_PAR$M_density = rep(200,nPatch)

# Setting up tiles - this is when memory is allocated
system.time(tile <-  MacroTile$new(MacroTile_PAR = tileParameters))

# Initialize PfPR on each tile
PfPR <- rep(.2, nPatch)
tile$init_PfSI(PfPR = PfPR)
# Here's Sean's code for queueing up some vaccinations
#tile$get_HumanPop()$queueVaccination_SimBitePfSI(tVaccine = (365*1),tTreat = (365*1)+1,fracPop = 0.75)
#tile$get_HumanPop()$queueVaccination_SimBitePfSI(tVaccine = (100),tTreat = (100)+1,fracPop = 0.5)

system.time(tile$simMacro(700))

pfsiHist = tile$get_HumanPop()$get_PfSI_history()
plot_PfSI(pfsiHist)


source("../../DanielCitron/Bioko/read_macro_pfsi_sim.R")
t_grid = seq(0,365)
prev.trajs <- pop_SI(pfsiHist, t_grid)



# Make some plots
ggplot(data = prev.trajs) +
  geom_point(aes(x=t, y=S), col = 'blue') +
  geom_point(aes(x=t, y=I),  col = 'red') +
  geom_line(aes(x=t, y=PEvaxx), col = 'green')
