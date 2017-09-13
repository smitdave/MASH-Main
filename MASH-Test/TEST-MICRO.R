###############################################################################
#       __  ______   _____ __  __      __  _________________  ____
#      /  |/  /   | / ___// / / /     /  |/  /  _/ ____/ __ \/ __ \
#     / /|_/ / /| | \__ \/ /_/ /_____/ /|_/ // // /   / /_/ / / / /
#    / /  / / ___ |___/ / __  /_____/ /  / // // /___/ _, _/ /_/ /
#   /_/  /_/_/  |_/____/_/ /_/     /_/  /_/___/\____/_/ |_|\____/
#
#   MASH-CPP
#   Testing Ground
#   September 9, 2017
#
###############################################################################

rm(list=ls());gc()
library(MASHmicro)
# set.seed(42L)

DEBUG.MASHMICRO()
MASHcpp::DEBUG.MASHCPP()
MASHmacro::DEBUG.MASHMACRO()

# make a tile
DIR = "/Users/slwu89/Desktop/MASHOUT/"

# setup
Humans.MICRO.Setup()
PfSI.MICRO.Setup(Pf_c = 1,Pf_b = 1,LatentPf = 1,DurationPf = 20)
AQUA.Emerge.Setup()

MBITES.Generic.Setup()
MBITES.BRO.Setup(aquaModule = "emerge",timing = "exponential")
# MBITES.BRO.Cohort.Setup()

SEARCH.Kernel.Setup(MBITES = "BRO")

# landscape parameters
nAqua = 10
nFeed = 10
emerge_par = list(N = nAqua,lambda = 25, lambdaWeight = NULL, offset = NULL)
landscape_par = Landscape.Parameters(nFeed = nFeed,nAqua = nAqua,pointGen = "poisson",module = "emerge",modulePars = emerge_par)

# human parameters
human_par = MASHmacro::HumanPop.Parameters(nSite = nFeed,siteSize = 10,siteMin = 2)

# M-BITES parameters
nMosy = 50
mbites_par = MBITES.BRO.Parameters(PfEIP=1)
mosquito_par = list(
  N_female = nMosy,
  ix_female = rep(1,nMosy),
  genotype_female = rep(1,nMosy),
  MBITES_PAR = mbites_par
)

MicroTile = Tile$new(Landscape_PAR = landscape_par,HumanPop_PAR = human_par,MosquitoPop_PAR = mosquito_par,directory = DIR)

# MicroLandscapePlot_utility(Landscape = MicroTile$get_Landscape())

# MicroTile$get_FemalePop()$simCohort(N = 1e3)

MicroTile$get_HumanPop()$init_ActivitySpace()
# MicroTile$get_HumanPop()$sim_ActivitySpace()
# 
# MicroTile$get_ActivitySpace()

MicroTile$get_HumanPop()$init_PfSI(PfPR = 0.95)

# MicroTile$get_HumanPop()$get_PathogensHistory()
MicroTile$simMICRO_oneRun(tMax = 365,verbose = TRUE,trackPop = TRUE)

MicroTile$reset_FemalePop(MosquitoPop_PAR = mosquito_par)
MicroTile$reset_HumanPop(HumanPop_PAR = human_par)
MicroTile$get_HumanPop()$init_PfSI(PfPR = 0.95)
MicroTile$get_HumanPop()$init_ActivitySpace()
MicroTile$simMICRO_oneRun(tMax = 365,verbose = TRUE)

# testing one step aquatic ecology for emerge
# MicroTile$get_FemalePop()$get_pop()$ls()
# MicroTile$get_Landscape()$oneStep_AquaticEcology()
# MicroTile$get_Landscape()$addCohort()
# MicroTile$get_FemalePop()$get_pop()$ls()

# testing chooseHost
# MicroTile$get_FemalePop()$get_pop()$get("0_1_1")$set_inPointSet("f")
# debug(MicroTile$get_FemalePop()$get_pop()$get("0_1_1")$chooseHost)
# MicroTile$get_FemalePop()$get_pop()$get("0_1_1")$chooseHost()



###############################################################################
# old code
###############################################################################

# par = MBITES.BRO.Parameters()
# mosyF = MosquitoPopFemale$new(N= 10,ix_init = rep(1,10),genotype_init = rep(1,10),MBITES_PAR = par)
# 
# mosyF$get_pop()$get("0_1_1")$get_history()
# mosyF$get_pop()$get("0_1_1")$rmSelf()
# 
# mosyF$get_pop()$ls()
# 
# mosyF$push_pop(N = 5,tEmerge = 10,genotype = 5,ix = 3)
# 
# mosyF$get_pop()$ls()
