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
library(jsonlite)
library(markovchain)
library(circlize)
# set.seed(42L)

#DEBUG.MASHMICRO()
#MASHcpp::DEBUG.MASHCPP()
#MASHmacro::DEBUG.MASHMACRO()

# make a tile
if(system("whoami",intern=TRUE)=="slwu89"){
  DIR="/Users/slwu89/Desktop/MASHOUT/"
}else if(system("whoami",intern=TRUE)=="sanchez.hmsc"){
  #DIR="/Users/sanchez.hmsc/Desktop/MASHOUT/"
  DIR="/Users/sanchez.hmsc/Documents/Github/MASH-Master/MASH-dev/HectorSanchez/VectorControl/Debug/"
  source("/Users/sanchez.hmsc/Documents/Github/MASH-Master/MASH-dev/HectorSanchez/AssortedScripts/sourceEntireFolder.R")
  sourceEntireFolder("/Users/sanchez.hmsc/Documents/Github/MASH-Master/MASH-MICRO/R/")
  sourceEntireFolder("/Users/sanchez.hmsc/Documents/Github/MASH-Master/MASH-dev/HectorSanchez/VectorControl/")
}else if(system("whoami",intern=TRUE)=="chipdelmal"){
  DIR="/Users/chipdelmal/Desktop/MASHOUT/"
  source("/Users/chipdelmal/Documents/Github/MASH-Main/MASH-dev/HectorSanchez/AssortedScripts/sourceEntireFolder.R")
  sourceEntireFolder("/Users/chipdelmal/Documents/Github/MASH-Main/MASH-MICRO/R/")
  sourceEntireFolder("/Users/chipdelmal/Documents/Github/MASH-Main/MASH-dev/HectorSanchez/VectorControl/")
}

###############################################################################
# Chord diagram
###############################################################################
# oneHistory: a single mosquito's JSON outfile
transitionsInMosquitoStates <- function(oneHistory, stateSpace = c("D","M","F","B","R","L","O","S","E")){
  states = oneHistory$stateH
  createSequenceMatrix(stringchar = unlist(states[-1]),possibleStates = stateSpace)
}

transitionsInMosquitoPopulation <- function(popHistory, stateSpace = c("D","M","F","B","R","L","O","S","E")){
  transMatrices = lapply(X = popHistory,FUN = transitionsInMosquitoStates)
  transitions = Reduce(f = "+",x = transMatrices)
  transitions[stateSpace,stateSpace]
}

circlizeStatesTransitionMatrix <- function(history, stateSpace = c("D","M","F","B","R","L","O","S","E")){
  transitions=transitionsInMosquitoPopulation(history,stateSpace=stateSpace)
  colors=c("#555555","#95E455","pink","red","purple","cyan","blue","yellow","grey")
  chordDiagramFromMatrix(transitions,directional=1,grid.col=colors,direction.type="arrows",self.link=2)
}
###############################################################################

###############################################################################
# setup
Humans.MICRO.Setup()
PfSI.MICRO.Setup()
AQUA.Emerge.Setup()

# MBITES setup
MBITES.Setup(aquaModule = "emerge",timing = "exponential")

# SEARCH setup
MBITES.Search.Setup(module = "kernel")

# landscape parameters
nAqua = 20
nPeriDom = 0
nFeed = 15
nSugar = 12
nMate = 10
emerge_par = list(N = nAqua,lambda = 25, lambdaWeight = NULL, offset = NULL)
landscape_par = Landscape.Parameters(nFeed = nFeed,nAqua = nAqua,nMate = nMate,nSugar = nSugar,pointGen = "lattice",module = "emerge",modulePars = emerge_par)

# set up the peri-domestic bit
periDomestic = rep(FALSE,nFeed)
lambda = replicate(n = nFeed,expr = NULL,simplify = FALSE)
module = rep(NA,nFeed)

if(nPeriDom>0){
  periDomestic[1:nPeriDom] = TRUE
  lambda[1:nPeriDom] = simpleLambda_Emerge(N = nPeriDom,lambda = 5)
  module[1:nPeriDom] = "emerge"
}

landscape_par$FeedingSite_PAR$periDomestic = periDomestic
landscape_par$FeedingSite_PAR$lambda = lambda
landscape_par$FeedingSite_PAR$module = module

# human parameters
patch_humans = rep(1,nFeed)
n_humans = sum(patch_humans)
patch_id = rep(x = 1:nFeed,patch_humans)
home_id = rep(x = 1:nFeed,patch_humans)
human_ages = unlist(lapply(X = patch_humans,FUN = MASHmacro:::siteAges_HumanPop))
human_bWeight = MASHmacro:::bitingWeight_HumanPop(human_ages)
human_par = lapply(X = 1:n_humans,function(i){
  list(
    houseID = home_id[i],
    patchID = patch_id[i],
    homeHouseID = home_id[i],
    homePatchID = patch_id[i],
    age = human_ages[i],
    bWeight = human_bWeight[i]

  )
})

# M-BITES parameters
nMosy = 50
mbites_par_female = MBITES.Complex.Parameters(PfEIP = 1,SUGAR = TRUE,MATE = TRUE)
mosquito_par = list(
N_female = nMosy,
ix_female = rep(1,nMosy),
genotype_female = rep(1,nMosy),
MBITES_PAR_FEMALE = mbites_par_female
)

MicroTile = Tile$new(Landscape_PAR = landscape_par,HumanPop_PAR = human_par,MosquitoPop_PAR = mosquito_par,directory = DIR)
MicroTile$get_HumanPop()$init_ActivitySpace()

# PfPR
pfpr = rep(0.5,nFeed)
MicroTile$simMICRO_oneRun(tMax = 50,PfPAR = pfpr,verbose = TRUE,trackPop = TRUE)

###### Distribute ######################################################################
### Sugar ######################################################
sugarSitesNumber=MicroTile$get_Landscape()$get_SugarSitesN()
for(i in 1:sugarSitesNumber){
  atsbTest=ATSB$new(id=i,killProbability=1,repelProbability=0)
  MicroTile$get_Landscape()$get_SugarSites(i)$set_attractiveSugarBait(atsbTest)
}
### Aquatic ####################################################
aquaSitesNumber=MicroTile$get_Landscape()$get_AquaSitesN()
for(i in 1:aquaSitesNumber){
  ovitrapTest=Ovitrap$new(id=i,killProbability=1,repelProbability=0)
  MicroTile$get_Landscape()$get_AquaSites(i)$set_ovitrap(ovitrapTest)
}
### Feeding ####################################################
feedingSitesNumber=MicroTile$get_Landscape()$get_FeedingSitesN()
for(i in 1:feedingSitesNumber){
  irsTest=IRS$new(id=i,killProbability=0,repelProbability=0)
  MicroTile$get_Landscape()$get_FeedingSites(i)$set_indoorResidualSpray(irsTest)
}
### Mating #####################################################
matingSitesNumber=MicroTile$get_Landscape()$get_MatingSitesN()
for(i in 1:matingSitesNumber){
  swarmSprayTest=SwarmSpray$new(id=i,killProbability=0,repelProbability=0)
  MicroTile$get_Landscape()$get_MatingSites(i)$set_swarmSpray(swarmSprayTest)
}
###### Run #############################################################################
MicroTile$simMICRO_oneRun(tMax = 500,PfPAR = pfpr,verbose = TRUE,trackPop = TRUE)

########################################################################################
###### TESTS ###########################################################################
########################################################################################
# files from each simulation
files_l = system(command = paste0("ls ",DIR,"MOSQUITO/"),intern = TRUE)
hist_l  = files_l[grep(pattern="History",x=files_l)] # individual json histories
pop_l = files_l[grep(pattern = "Pop",x = files_l)] # population csv
mHist   = fromJSON(txt=paste0(DIR,"MOSQUITO/",hist_l[[1]]),flatten = FALSE,simplifyVector=FALSE)
nullIx = which(vapply(X = mHist,FUN = function(x){x$ID[[1]]},FUN.VALUE = character(1)) == "NULL")
mHist = mHist[-nullIx]
circlizeStatesTransitionMatrix(history = mHist)
#mPop = read.table(file = paste0(DIR,"MOSQUITO/",pop_l),header = TRUE,sep = ",")

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
