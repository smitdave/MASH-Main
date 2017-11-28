###############################################################################
#       __  ______   _____ __  __      __  _________________  ____
#      /  |/  /   | / ___// / / /     /  |/  /  _/ ____/ __ \/ __ \
#     / /|_/ / /| | \__ \/ /_/ /_____/ /|_/ // // /   / /_/ / / / /
#    / /  / / ___ |___/ / __  /_____/ /  / // // /___/ _, _/ /_/ /
#   /_/  /_/_/  |_/____/_/ /_/     /_/  /_/___/\____/_/ |_|\____/
#
###############################################################################

###############################################################################
# M-BITES: Complex Female + MBITES-Male Run with Emerge
###############################################################################

rm(list=ls());gc()
library(MASHmicro)
library(R6)
# set.seed(42L)

#DEBUG.MASHMICRO()
#MASHcpp::DEBUG.MASHCPP()
#MASHmacro::DEBUG.MASHMACRO()

# make a tile
if(system("whoami",intern=TRUE)=="slwu89"){
  DIR="/Users/slwu89/Desktop/MASHOUT/"
}else if(system("whoami",intern=TRUE)=="sanchez.hmsc"){
  DIR="/Users/sanchez.hmsc/Desktop/MASHOUT/"
  source("/Users/sanchez.hmsc/Documents/Github/MASH-Main/MASH-dev/HectorSanchez/AssortedScripts/sourceEntireFolder.R")
  sourceEntireFolder("/Users/sanchez.hmsc/Documents/Github/MASH-Main/MASH-MICRO/R/")
  sourceEntireFolder("/Users/sanchez.hmsc/Documents/Github/MASH-Main/MASH-dev/HectorSanchez/VectorControl/")
}else if(system("whoami",intern=TRUE)=="chipdelmal"){
  DIR="/Users/chipdelmal/Desktop/MASHOUT/"
  sourceEntireFolder("/Users/sanchez.hmsc/Documents/Github/MASH-Main/MASH-MICRO/R/")
}



# setup
Humans.MICRO.Setup()
PfSI.MICRO.Setup(Pf_c = 1,Pf_b = 1,LatentPf = 1,DurationPf = 20)
AQUA.Emerge.Setup()

# MBITES setup
MBITES.Generic.Setup()
MBITES.Complex.Setup(aquaModule = "emerge",timing = "exponential")
MBITES.Male.Setup(timing = "exponential")

# SEARCH setup
SEARCH.Kernel.Setup(MBITES = "FULL")

# landscape parameters
nAqua = 20
nFeed = 15
nSugar = 12
nMate = 10
emerge_par = list(N = nAqua,lambda = 25, lambdaWeight = NULL, offset = NULL)
landscape_par = Landscape.Parameters(nFeed = nFeed,nAqua = nAqua,nMate = nMate,nSugar = nSugar,pointGen = "lattice",module = "emerge",modulePars = emerge_par)

# human parameters
human_par = MASHmacro::HumanPop.Parameters(nSite = nFeed,siteSize = 10,siteMin = 2)

# M-BITES parameters
nMosy = 50
mbites_par_female = MBITES.Complex.Parameters(PfEIP = 1 )
mbites_par_male = MBITES.Male.Parameters(maleHistory = TRUE)
mosquito_par = list(
  N_female = nMosy,
  N_male = nMosy,
  ix_female = rep(1,nMosy),
  ix_male = rep(1,nMosy),
  genotype_female = rep(1,nMosy),
  genotype_male = rep(1,nMosy),
  MBITES_PAR_FEMALE = mbites_par_female,
  MBITES_PAR_MALE = mbites_par_male
)

MicroTile = Tile$new(Landscape_PAR = landscape_par,HumanPop_PAR = human_par,MosquitoPop_PAR = mosquito_par,directory = DIR)
MicroTile$get_HumanPop()$init_ActivitySpace()
MicroTile$get_HumanPop()$init_PfSI(PfPR = 0.95)

###### Distribute ######################################################################
### Sugar ######################################################
sugarSitesNumber=MicroTile$get_Landscape()$get_SugarSitesN()
for(i in 1:sugarSitesNumber){
  atsbTest=ATSB$new(id=i,killProbability=0,repelProbability=.01)
  MicroTile$get_Landscape()$get_SugarSites(i)$set_attractiveSugarBait(atsbTest)
}
### Aquatic ####################################################
aquaSitesNumber=MicroTile$get_Landscape()$get_AquaSitesN()
for(i in 1:aquaSitesNumber){
  ovitrapTest=Ovitrap$new(id=i,killProbability=0)
  MicroTile$get_Landscape()$get_AquaSites(i)$set_ovitrap(ovitrapTest)
}
###### Run #############################################################################
MicroTile$simMICRO_oneRun(tMax = 20,verbose = TRUE,trackPop = TRUE)

########################################################################################
###### TESTS ###########################################################################
########################################################################################

###### Get/Set #########################################################################
# atsbTest=ATSB$new(id=10)
# MicroTile$get_Landscape()$get_SugarSites(2)$set_attractiveSugarBait(atsbTest)
# MicroTile$get_Landscape()$get_SugarSites(2)$get_attractiveSugarBait()
# aerialTest=AerialSpray$new(id=10)
# MicroTile$get_Landscape()$get_MatingSites(2)$set_aerialSpray(aerialTest)
# MicroTile$get_Landscape()$get_MatingSites(2)$get_aerialSpray()
# swarmSprayTest=SwarmSpray$new(id=10)
# MicroTile$get_Landscape()$get_MatingSites(2)$set_swarmSpray(swarmSprayTest)
# MicroTile$get_Landscape()$get_MatingSites(2)$get_swarmSpray()
# # areaRepellentTest=SwarmSpray$new(id=10)
# # MicroTile$get_Landscape()$get_MatingSites(2)$set_swarmSpray(swarmSprayTest)
# # MicroTile$get_Landscape()$get_MatingSites(2)$get_swarmSpray()
# ovitrapTest=Ovitrap$new(id=10)
# MicroTile$get_Landscape()$get_AquaSites(2)$set_ovitrap(ovitrapTest)
# MicroTile$get_Landscape()$get_AquaSites(2)$get_ovitrap()
# larvicidingTest=Larvicide$new(id=10)
# MicroTile$get_Landscape()$get_AquaSites(2)$set_ovitrap(larvicidingTest)
# MicroTile$get_Landscape()$get_AquaSites(2)$get_ovitrap()
# sreTest=SourceReduction$new(id=10)
# MicroTile$get_Landscape()$get_AquaSites(2)$set_sourceReduction(sreTest)
# MicroTile$get_Landscape()$get_AquaSites(2)$get_sourceReduction()
# bioTest=BiologicalControl$new(id=10)
# MicroTile$get_Landscape()$get_AquaSites(2)$set_biologicalControl(bioTest)
# MicroTile$get_Landscape()$get_AquaSites(2)$get_biologicalControl()
# obtTest=OdorBaitedTrap$new(id=10)
# MicroTile$get_Landscape()$get_FeedingSites(2)$set_odorBaitedTrap(obtTest)
# MicroTile$get_Landscape()$get_FeedingSites(2)$get_odorBaitedTrap()
# eavTest=EaveTube$new(id=10)
# MicroTile$get_Landscape()$get_FeedingSites(2)$set_eaveTube(eavTest)
# MicroTile$get_Landscape()$get_FeedingSites(2)$get_eaveTube()
# homTest=ImproveHome$new(id=10)
# MicroTile$get_Landscape()$get_FeedingSites(2)$set_homeImprovement(homTest)
# MicroTile$get_Landscape()$get_FeedingSites(2)$get_homeImprovement()
# irsTest=IRS$new(id=10)
# MicroTile$get_Landscape()$get_FeedingSites(2)$set_indoorResidualSpray(irsTest)
# MicroTile$get_Landscape()$get_FeedingSites(2)$get_indoorResidualSpray()
