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

###############################################################################
# M-BITES: BRO Female Only Run with Emerge and PfSI
###############################################################################

rm(list=ls());gc()
library(MASHmicro)
# set.seed(42L)

# DEBUG.MASHMICRO(MASHCPP = TRUE)
# MASHcpp::DEBUG.MASHCPP()
# MASHmacro::DEBUG.MASHMACRO()

# make a tile
if(system("whoami",intern=TRUE)=="slwu89"){
  DIR="/Users/slwu89/Desktop/MICRO/"
}else if(system("whoami",intern=TRUE)=="chipdelmal"){
  DIR = "/Users/chipdelmal/Desktop/MASHOUT/"
}

# setup
Humans.MICRO.Setup()
PfSI.MICRO.Setup()
AQUA.Emerge.Setup()

# MBITES setup
MBITES.Generic.Setup()
MBITES.BRO.Setup(aquaModule = "emerge",timing = "exponential")

# SEARCH setup
SEARCH.Kernel.Setup(MBITES = "BRO")

# landscape parameters
nAqua = 20
nFeed = 20
emerge_par = list(N = nAqua,lambda = 25, lambdaWeight = NULL, offset = NULL)
landscape_par = Landscape.Parameters(nFeed = nFeed,nAqua = nAqua,pointGen = "lattice",module = "emerge",modulePars = emerge_par)

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
patch_humans = rpois(n = nFeed,lambda = 20)
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
mbites_par = MBITES.BRO.Parameters(PfEIP=1)
mosquito_par = list(
  N_female = nMosy,
  ix_female = rep(1,nMosy),
  genotype_female = rep(1,nMosy),
  MBITES_PAR_FEMALE = mbites_par
)

MicroTile = Tile$new(Landscape_PAR = landscape_par,HumanPop_PAR = human_par,MosquitoPop_PAR = mosquito_par,directory = DIR)

# MicroLandscapePlot_utility(Landscape = MicroTile$get_Landscape())

MicroTile$get_HumanPop()$init_ActivitySpace()
# MicroTile$get_ActivitySpace()

# PfPR
pfpr = rep(0.5,nFeed)

MicroTile$simMICRO_oneRun(tMax = 365,PfPAR = pfpr,verbose = TRUE,trackPop = TRUE)
MicroTile$resetMicro(MosquitoPar = mosquito_par,HumanPar = human_par,EL4P = FALSE,mating = FALSE)
MicroTile$get_HumanPop()$init_ActivitySpace()
MicroTile$simMICRO_oneRun(tMax = 365,PfPAR = pfpr,verbose = TRUE,trackPop = TRUE)

detach("package:MASHmicro", unload=TRUE)

###############################################################################
# M-BITES: BROMS Female + MBITES-Male Run with Emerge
###############################################################################

rm(list=ls());gc()
library(MASHmicro)
# set.seed(42L)

# DEBUG.MASHMICRO()
# MASHcpp::DEBUG.MASHCPP()
# MASHmacro::DEBUG.MASHMACRO()

# make a tile
if(system("whoami",intern=TRUE)=="slwu89"){
  DIR="/Users/slwu89/Desktop/MICRO/"
}else if(system("whoami",intern=TRUE)=="chipdelmal"){
  DIR = "/Users/chipdelmal/Desktop/MASHOUT/"
}

# setup
Humans.MICRO.Setup()
PfSI.MICRO.Setup()
AQUA.Emerge.Setup()

# MBITES setup
MBITES.Generic.Setup()
MBITES.BRO.Setup(aquaModule = "emerge",timing = "exponential",SUGAR = TRUE,MATE = TRUE)
MBITES.Male.Setup(timing = "exponential")

# SEARCH setup
SEARCH.Kernel.Setup(MBITES = "BRO")

# landscape parameters
nAqua = 20
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
patch_humans = rpois(n = nFeed,lambda = 20)
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
mbites_par_female = MBITES.BRO.Parameters(PfEIP=1,SUGAR = TRUE,MATE = TRUE)
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

# MicroLandscapePlot_utility(Landscape = MicroTile$get_Landscape())

# MicroTile$get_FemalePop()$simCohort(N = 1e3)

MicroTile$get_HumanPop()$init_ActivitySpace()
# MicroTile$get_ActivitySpace()

# PfPR
pfpr = rep(0.5,nFeed)

MicroTile$simMICRO_oneRun(tMax = 365,PfPAR = pfpr,verbose = TRUE,trackPop = TRUE)

MicroTile$resetMicro(MosquitoPar = mosquito_par,HumanPar = human_par,EL4P = FALSE,mating = TRUE)
MicroTile$get_HumanPop()$init_ActivitySpace()
MicroTile$simMICRO_oneRun(tMax = 365,PfPAR = pfpr,verbose = TRUE,trackPop = TRUE)

detach("package:MASHmicro", unload=TRUE)


###############################################################################
# M-BITES: Complex Female Only Run with Emerge
###############################################################################

rm(list=ls());gc()
library(MASHmicro)
# set.seed(42L)

# make a tile
DIR = "/Users/slwu89/Desktop/MICRO/"

# setup
Humans.MICRO.Setup()
PfSI.MICRO.Setup()
AQUA.Emerge.Setup()

# MBITES setup
MBITES.Generic.Setup()
MBITES.Complex.Setup(SUGAR = FALSE, MATE = FALSE, aquaModule = "emerge",timing = "exponential")

# SEARCH setup
SEARCH.Kernel.Setup(MBITES = "FULL")

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
patch_humans = rpois(n = nFeed,lambda = 20)
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
mbites_par_female = MBITES.Complex.Parameters(PfEIP = 1,SUGAR = FALSE,MATE = FALSE)
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

MicroTile$resetMicro(MosquitoPar = mosquito_par,HumanPar = human_par,EL4P = FALSE,mating = TRUE)
MicroTile$get_HumanPop()$init_ActivitySpace()
MicroTile$simMICRO_oneRun(tMax = 50,PfPAR = pfpr,verbose = TRUE,trackPop = TRUE)

detach("package:MASHmicro", unload=TRUE)


###############################################################################
# M-BITES: Complex Female + MBITES-Male Run with Emerge
###############################################################################

rm(list=ls());gc()
library(MASHmicro)
# set.seed(42L)

# DEBUG.MASHMICRO()
# MASHcpp::DEBUG.MASHCPP()
# MASHmacro::DEBUG.MASHMACRO()

# make a tile
DIR = "/Users/slwu89/Desktop/MICRO/"

# setup
Humans.MICRO.Setup()
PfSI.MICRO.Setup()
AQUA.Emerge.Setup()

# MBITES setup
MBITES.Generic.Setup()
MBITES.Complex.Setup(aquaModule = "emerge",timing = "exponential")
MBITES.Male.Setup(timing = "exponential")

# SEARCH setup
SEARCH.Kernel.Setup(MBITES = "FULL")

# landscape parameters
nAqua = 20
nPeriDom = 2
nFeed = 15
nSugar = 12
nMate = 10
emerge_par = list(N = nAqua,lambda = 25, lambdaWeight = NULL, offset = NULL)
landscape_par = Landscape.Parameters(nFeed = nFeed,nAqua = nAqua,nMate = nMate,nSugar = nSugar,pointGen = "lattice",module = "emerge",modulePars = emerge_par)

# set up the peri-domestic bit
periDomestic = rep(FALSE,nFeed)
lambda = replicate(n = nFeed,expr = NULL,simplify = FALSE)
module = rep(NA,nFeed)

periDomestic[1:nPeriDom] = TRUE
lambda[1:nPeriDom] = simpleLambda_Emerge(N = nPeriDom,lambda = 5)
module[1:nPeriDom] = "emerge"

landscape_par$FeedingSite_PAR$periDomestic = periDomestic
landscape_par$FeedingSite_PAR$lambda = lambda
landscape_par$FeedingSite_PAR$module = module

# human parameters
patch_humans = rpois(n = nFeed,lambda = 20)
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

# PfPR
pfpr = rep(0.5,nFeed)

MicroTile$simMICRO_oneRun(tMax = 50,PfPAR = pfpr,verbose = TRUE,trackPop = TRUE)

MicroTile$resetMicro(MosquitoPar = mosquito_par,HumanPar = human_par,EL4P = FALSE,mating = TRUE)
MicroTile$get_HumanPop()$init_ActivitySpace()
MicroTile$simMICRO_oneRun(tMax = 50,PfPAR = pfpr,verbose = TRUE,trackPop = TRUE)

detach("package:MASHmicro", unload=TRUE)