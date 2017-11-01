#################################################################
#
#   Running MACRO - on Carlos's Bioko Island data, establishing a baseline with no vaccinations
#
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#     _____   __ _____ _____
#    |  __ \ / _/ ____|_   _|
#    | |__) | || (___   | |
#    |  ___/|  _\___ \  | |
#    | |    | | ____) |_| |_
#    |_|    |_||_____/|_____|
#
#   Sean Wu
#   May 25, 2017
#
#   Daniel Citron
#   October 27, 2017
#################################################################
rm(list=ls())
gc()
library(MASH)

seed = 42
set.seed(seed)

# initialize classes for MACRO
MACRO.Humans.Setup(pathogenModule = "PfSI")

# setup class methods for PfSI and SimBite modules
# MACRO relies on SimBitePfSI module for adding bites to humans
PfSI.Setup(mnPEPf = 365, vrPEPf = 30)
SimBitePfSI.Setup()

# MACRO Patch initialization
MACRO.Patch.Emerge.Setup() # 'Emerge' model

# Patch Parameters:
# Set working directory to where the bioko data is
#setwd("MASH/MASH-Main/MASH-dev/DavidSmith/Bioko/")
biokoPOP <- read.csv("bioko_areas.csv")
# load in PfPR for bioko island
biokoPFPR <- read.csv("pfprmap_area_CAGcorrectNA.csv")
# Merge the two data sets on the areaIds
bioko <- merge(biokoPOP, biokoPFPR, by = "areaId", all =FALSE)

# Try running this with only the Malabo patches
#malabo <- c(212, 218, 219, 220, 271, 272, 273, 274, 275, 276, 277, 278, 279, 330, 331, 332, 333, 334,
#            335, 336, 337, 338, 390, 391, 392, 393, 394, 395, 396, 397, 398, 448, 449, 450, 451, 452,
#            453, 454, 455, 507, 508, 509, 510, 511, 512, 513, 514)
#malabo <- data.frame(areaId = malabo)
#bioko <- merge(bioko, malabo, by = "areaId", all = FALSE)

# Adjust the population by a factor of 10
PopM <- ceiling(bioko$popm/100)
PopF <- ceiling(bioko$popf/100)
bioko$popm <- PopM
bioko$popf <- PopF
bioko$pop <- PopM + PopF

nPatch = nrow(bioko) #nPatch = 10

# Set up tile parameters
ll <- 50*2^10
tileParameters = MACRO.Tile.Parameters(N = nPatch,
                                       aquaModule = "emerge", aquaPars = list(N=nPatch,lambda=rep(ll,nPatch)),
                                       pathogenModule = "PfSI")
# Manually set tile parameters using hte bioko data
popTotal <- sum(bioko$pop[1:nPatch])
tileParameters$HumanPop_PAR$nHumans <- popTotal
tileParameters$HumanPop_PAR$sitePops <- bioko$pop[1:nPatch] # might need this to be a double not an int
hIDs <- sample(as.integer(1:popTotal)) # a randomly ordered vector, so vaccines are set randomly
tileParameters$HumanPop_PAR$humanIDs <- hIDs
tileParameters$HumanPop_PAR$siteHumanIDs <- split(hIDs, rep(1:nPatch, bioko$pop[1:nPatch]))
tileParameters$HumanPop_PAR$homeIDs <- rep(1:nPatch, bioko$pop[1:nPatch])
sa <- siteAges(popTotal)
tileParameters$HumanPop_PAR$bDay <- -sa
tileParameters$HumanPop_PAR$steAges <- split(sa, rep(1:nPatch, bioko$pop[1:nPatch]))
tileParameters$HumanPop_PAR$bWeight <- rgamma(popTotal, 1)

tileParameters$MacroMosquitoPop_PAR$M_density = rep(200,nPatch)
# Setting up tiles - this is when memory is allocated
system.time(tile <-  MacroTile$new(MacroTile_PAR = tileParameters))

# Initialize PfPR on each tile
PfPR <- bioko$pfpr[1:nPatch]
tile$init_PfSI(PfPR = PfPR)
# Here's Sean's code for queueing up some vaccinations
# Vaccinations are off for now, this is the baseline simulation
#tile$get_HumanPop()$queueVaccination_SimBitePfSI(tVaccine = (365*1),tTreat = (365*1)+1,fracPop = 0.75)

# Simulate for 5 years, see how long that takes
system.time(tile$simMacro(5*365))

pfsiHist = tile$get_HumanPop()$get_PfSI_history()

saveRDS(pfsiHist, sprintf(paste("bioko_island_baseline_output_",seed,".rds", sep="")))
