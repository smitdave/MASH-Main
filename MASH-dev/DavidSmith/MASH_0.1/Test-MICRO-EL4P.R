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
#   May 30, 2017
#
#################################################################


#################################################################
#   _____ _    _  _   ____
#   | ____| |  | || | |  _ \
#   |  _| | |  | || |_| |_) |
#   | |___| |__|__   _|  __/
#   |_____|_____| |_| |_|
#
#   Microsimulation Tile Tests 'EL4P'
#   basically, set up the tile, then run Cohort, then fit EL4P, then update the mosy pop.
#
#################################################################

rm(list=ls())
gc()
library(MASH)

# set up classes
MBITES_module = "BRO"
AQUA_module = "EL4P"
SEARCH.MicroKernel.Setup(MBITES = MBITES_module,overwrite = TRUE)
MICRO.EL4P.Setup(overwrite = TRUE)
PfSI.Setup(overwrite = TRUE)
MICRO.Humans.Setup(overwrite = TRUE)

nFeed = 5
nAqua = 4
Landscape_PAR = Landscape.Parameters(nFeed = nFeed,nAqua = nAqua,pointGen = "poisson",module = AQUA_module,modulePars = NULL)
HumanPop_PAR = HumanPop.Parameters(nSite = nFeed,siteSize = 3,siteMin = 1,bWeight = 1)
# set N_female equal to small number because we need to update it later.
MosquitoPop_PAR = MicroMosquitoPop.Setup(cohort = TRUE,
                                         module = MBITES_module,
                                         aquaModule = "emerge",
                                         N_female = 1,
                                         time = 0,
                                         ix_female = rep(1,1),
                                         genotype_female = rep(0,1),
                                         batchSize = "bms",
                                         eggMatT = "off",
                                         PfEIP = 0.1,
                                         B_succeed = 1, B_surv = 0.9)

# Generate a MicroTile
tile = MicroTile$new(Landscape_PAR,
                     HumanPop_PAR,
                     MosquitoPop_PAR,
                     directory = "/Users/slwu89/Desktop/mash.out/")

# plot the landscape on the tile
# MicroLandscapePlot_utility(tile$get_Landscape())


#################################################################
# figure out how to do eqAqua
#################################################################


# run cohort
cohortOut = tile$get_FemalePop()$simCohort(N=1e3,writeJSON=FALSE)


# parameters from cohort sim that we need
cohort_bionomics = calculateBionomics_utility(history = cohortOut)
eqAqua = ovipositionEq_utility(history = cohortOut,nAqua = nAqua)

# fit EL4P
EL4P_PAR = EL4P.Parameters(nAqua = nAqua,nHumans = HumanPop_PAR$nHumans,R0 = 6,
                           eqAqua = eqAqua,EIP = 12,lifespan = cohort_bionomics$summary$lifespans,
                           G = cohort_bionomics$summary$meanEggBatches,
                           nu = cohort_bionomics$summary$totalEggBatches,
                           S = cohort_bionomics$summary$humanBloodmeals)

EL4P_fit = EL4P.Mesh.Fit(mesh_N = nAqua,EL4P_PAR = EL4P_PAR,var_tol = 5,plot = TRUE)

# update the Landscape and AquaticSite
MosquitoPop_PAR = MicroMosquitoPop.Setup(cohort = FALSE,
                                         module = MBITES_module,
                                         aquaModule = AQUA_module,
                                         N_female = floor(EL4P_PAR$M),
                                         time = 0,
                                         ix_female = sample(x = nAqua,prob = eqAqua,replace = TRUE,size = floor(EL4P_PAR$M)),
                                         genotype_female = rep(x = 0,length = floor(EL4P_PAR$M)),
                                         batchSize = "bms",
                                         eggMatT = "off",
                                         PfEIP = 0.1,
                                         B_succeed = 1, B_surv = 0.95, O_surv = 0.95, R_surv = 0.95)

tile$set_FemalePop(MosquitoPop_PAR = MosquitoPop_PAR)
tile$get_Landscape()$updateLandscapeEL4P(EL4P_fit,EL4P_PAR)





# MicroLandscapePlot_utility(tile$get_Landscape())
# MicroKernelPlot_utility(S = tile$get_Landscape()$get_AquaSites(),D = tile$get_Landscape()$get_FeedingSites())

# initialize human activity space and pfsi infections
tile$get_HumanPop()$init_ActivitySpace(nDaily = 0)
tile$get_HumanPop()$init_MICRO_PfSI(PfPR = 0.5, tStart = 0)

# debug(tile$get_Landscape()$addCohort)
# debug(tile$get_FemalePop()$clear_pop)
# debug(tile$get_FemalePop()$get_MosquitoIxM(1)$MBITES)

# run sim
tMax = 365
# debug(tile$simMICRO_oneStep)
while(tile$get_tNow() < tMax){
  tile$simMICRO_oneStep(timeStep = 1,print = TRUE,logInterval = 10)
}

PfSI_history = tile$get_HumanPop()$get_PfSI_history()
plot_PfSI(PfSI_history)
