#################################################################
#
#   MASH: Testing routines for MICRO
#
#   __  __ ___ ____ ____   ___
#   |  \/  |_ _/ ___|  _ \ / _ \
#   | |\/| || | |   | |_) | | | |
#   | |  | || | |___|  _ <| |_| |
#   |_|  |_|___\____|_| \_\\___/
#
#   Sean Wu
#   August 16, 2017
#
#################################################################


#################################################################
#
#   _____
#   | ____|_ __ ___   ___ _ __ __ _  ___
#   |  _| | '_ ` _ \ / _ \ '__/ _` |/ _ \
#   | |___| | | | | |  __/ | | (_| |  __/
#   |_____|_| |_| |_|\___|_|  \__, |\___|
#                             |___/
#   Microsimulation Tile Tests 'Emerge'
#
#################################################################

rm(list=ls())
library(MASH)

#################################################################
# Init the MicroTile
#################################################################

MBITES_module = "BRO"
AQUA_module = "emerge"

# XX.Setup() functions to initialize classes for MICRO
MICRO.Humans.Setup(overwrite = TRUE)
SEARCH.MicroKernel.Setup(MBITES = "BRO",overwrite = TRUE)
MICRO.Emerge.Setup(overwrite = TRUE)
PfSI.Setup(overwrite = TRUE,
           Pf_b = 1,
           Pf_c = 1,
           FeverPf = 0.75)

# XX.Parameters() functions to generate parameters for objects in a MicroTile
nFeed=25
nAqua=25
nMosy=500
Landscape_PAR = Landscape.Parameters(nFeed = nFeed,nAqua = nAqua,pointGen = "lattice",module = AQUA_module,modulePars = list(N=nAqua,lambda=50))
# AquaEmergeLambdaPlot_utility(Landscape_PAR$AquaticSite_PAR$lambda)
HumanPop_PAR = HumanPop.Parameters(nSite = nFeed,siteSize = 5,siteMin = 3,bWeight = 1)
MosquitoPop_PAR = MicroMosquitoPop.Setup(cohort = FALSE,
                                         module = MBITES_module,
                                         aquaModule = AQUA_module,
                                         N_female = nMosy,
                                         time = 0,
                                         ix_female = rep(1,nMosy),
                                         genotype_female = rep(1,nMosy),
                                         batchSize = "bms",
                                         eggMatT = "off",
                                         PfEIP = 0.1,
                                         B_succeed = 1)

# Generate a MicroTile
tile = MicroTile$new(Landscape_PAR,
                     HumanPop_PAR,
                     MosquitoPop_PAR,
                     directory = "/Users/slwu89/Desktop/mash.out/")

#################################################################
# Run MICRO
#################################################################

# plots
AquaEmergeLambdaPlot_utility(Landscape_PAR$AquaticSite_PAR$lambda)
MicroLandscapePlot_utility(tile$get_Landscape())
MicroKernelPlot_utility(S = tile$get_Landscape()$get_AquaSites(),D = tile$get_Landscape()$get_FeedingSites())

# initialize human activity space and pfsi infections
tile$get_HumanPop()$init_ActivitySpace(nDaily = 0)
tile$get_HumanPop()$init_MICRO_PfSI(PfPR = 0.5, tStart = 0)

# debug(tile$get_Landscape()$addCohort)
# debug(tile$get_FemalePop()$clear_pop)
# debug(tile$get_FemalePop()$get_MosquitoIxM(1)$MBITES)

# run sim
tMax = 365
while(tile$get_tNow() < tMax){
  tile$simMICRO_oneStep(timeStep = 1,print = TRUE,logInterval = 10)
}

PfSI_history = tile$get_HumanPop()$get_PfSI_history()
plot_PfSI(PfSI_history)
