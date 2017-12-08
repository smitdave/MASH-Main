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

###????? Why there is macro here. the first function doesn't have that arguement. so
########there is an error
#DEBUG.MASHMICRO(MASHCPP = TRUE)
MASHcpp::DEBUG.MASHCPP()
#MASHmacro::DEBUG.MASHMACRO()

# make a tile
if(system("whoami",intern=TRUE)=="slwu89"){
  DIR="/Users/slwu89/Desktop/MASHOUT/"
}else if(system("whoami",intern=TRUE)=="chipdelmal"){
  DIR = "/Users/chipdelmal/Desktop/MASHOUT/"
}else if(system("whoami",intern=TRUE)=="qianzh"){
  DIR = "/Users/qianzh/Desktop/MASHOUT/"} # add my dir here

# setup
Humans.MICRO.Setup() #Initiallize human methods for micro
PfSI.MICRO.Setup(Pf_c = 1,Pf_b = 1,LatentPf = 1,DurationPf = 20) #Initialize PfSI Pathogen Module
AQUA.Emerge.Setup() #Initialize Emerge Aquatic Ecology Module

# MBITES setup
MBITES.Generic.Setup() #Initialize Generic Methods
MBITES.BRO.Setup(aquaModule = "emerge",timing = "exponential") #Initialize M-BITES BRO (Blood Feeding, Resting, Oviposition) lifecycle model.
# MBITES.BRO.Cohort.Setup()

# SEARCH setup
SEARCH.Kernel.Setup(MBITES = "BRO") #Initialize MICRO Search Kernels module of mosquito search behavior

# landscape parameters
nAqua = 20
nFeed = 20
emerge_par = list(N = nAqua,lambda = 25, lambdaWeight = NULL, offset = NULL)
landscape_par = Landscape.Parameters(nFeed = nFeed,nAqua = nAqua,pointGen = "lattice",module = "emerge",modulePars = emerge_par)

# human parameters
human_par = MASHmacro::HumanPop.Parameters(nSite = nFeed,siteSize = 10,siteMin = 2)

# M-BITES parameters
nMosy = 50
mbites_par = MBITES.BRO.Parameters(PfEIP=11)
mosquito_par = list(
  N_female = nMosy,
  ix_female = rep(1,nMosy),
  genotype_female = rep(1,nMosy),
  MBITES_PAR_FEMALE = mbites_par
)

MicroTile = Tile$new(Landscape_PAR = landscape_par,HumanPop_PAR = human_par,MosquitoPop_PAR = mosquito_par,directory = DIR)

# MicroLandscapePlot_utility(Landscape = MicroTile$get_Landscape())

# MicroTile$get_FemalePop()$simCohort(N = 1e3)

MicroTile$get_HumanPop()$init_ActivitySpace()

# MicroTile$get_ActivitySpace()

MicroTile$get_HumanPop()$init_PfSI(PfPR = 0.95)

MicroTile$simMICRO_oneRun(tMax = 365,verbose = TRUE,trackPop = TRUE)

MicroTile$reset_FemalePop(MosquitoPop_PAR = mosquito_par)
MicroTile$reset_HumanPop(HumanPop_PAR = human_par)
MicroTile$get_HumanPop()$init_PfSI(PfPR = 0.95)
MicroTile$get_HumanPop()$init_ActivitySpace()
MicroTile$simMICRO_oneRun(tMax = 365,verbose = TRUE)


###############################################################################
# M-BITES: BROMS Female + MBITES-Male Run with Emerge
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

# human parameters
human_par = MASHmacro::HumanPop.Parameters(nSite = nFeed,siteSize = 10,siteMin = 2)

# M-BITES parameters
nMosy = 50
mbites_par_female = MBITES.BRO.Parameters(PfEIP=11,SUGAR = TRUE,MATE = TRUE)
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

MicroTile$get_HumanPop()$init_PfSI(PfPR = 0.95)

MicroTile$simMICRO_oneRun(tMax = 365,verbose = TRUE,trackPop = TRUE)

MicroTile$reset_FemalePop(MosquitoPop_PAR = mosquito_par)
MicroTile$reset_HumanPop(HumanPop_PAR = human_par)
MicroTile$get_HumanPop()$init_PfSI(PfPR = 0.95)
MicroTile$get_HumanPop()$init_ActivitySpace()
MicroTile$simMICRO_oneRun(tMax = 365,verbose = TRUE)


###############################################################################
# M-BITES: Complex Female + MBITES-Male Run with Emerge
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
mbites_par_female = MBITES.Complex.Parameters(PfEIP = 11 )
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

MicroTile$simMICRO_oneRun(tMax = 365,verbose = TRUE,trackPop = TRUE)







###############################################################################
#
#   TEST MAKING KERNEL CDF/PDF GRAPHICS FOR MANUSCRIPT
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

# MBITES setup
MBITES.Generic.Setup()
MBITES.BRO.Setup(aquaModule = "emerge",timing = "exponential")
# MBITES.BRO.Cohort.Setup()

# SEARCH setup
SEARCH.Kernel.Setup(MBITES = "BRO")

# landscape parameters
nAqua = 3
nFeed = 3
emerge_par = list(N = nAqua,lambda = 25, lambdaWeight = NULL, offset = NULL)
landscape_par = Landscape.Parameters(nFeed = nFeed,nAqua = nAqua,pointGen = "lattice",module = "emerge",modulePars = emerge_par)

# human parameters
human_par = MASHmacro::HumanPop.Parameters(nSite = nFeed,siteSize = 10,siteMin = 2)

# M-BITES parameters
nMosy = 50
mbites_par = MBITES.BRO.Parameters(PfEIP=11)
mosquito_par = list(
  N_female = nMosy,
  ix_female = rep(1,nMosy),
  genotype_female = rep(1,nMosy),
  MBITES_PAR_FEMALE = mbites_par
)

MicroTile = Tile$new(Landscape_PAR = landscape_par,HumanPop_PAR = human_par,MosquitoPop_PAR = mosquito_par,directory = DIR)


pointSet = MicroTile$get_Landscape()$get_PointSet()


sigma = 3
eps = 0.1
beta = 1.3
startXY = pointSet$AquaSites[[1]]$xy
endPts = pointSet$FeedingSites

dist = numeric(length = length(endPts))
for(i in 1:length(endPts)){
  dist[i] = sqrt((startXY[1]-endPts[[i]]$xy[1])^2 + (startXY[2]-endPts[[i]]$xy[2])^2)
}

endWts = vapply(X = endPts,FUN = function(x){x$wt},FUN.VALUE = numeric(1),USE.NAMES = FALSE)
prob = endWts^(-beta*dist) * (eps + dist)^-sigma
prob = prob/sum(prob)

# plot(x = dist,y = prob,pch=16)
# x = ecdf(x = prob)

cols = viridis::viridis(n = length(prob))
plot(x = dist,y = prob,pch=16)

kern = cbind(dist,prob)
kern = kern[order(kern[,1],decreasing = FALSE),]
kern = cbind(kern,cumProb = cumsum(kern[,"prob"]))

plot(x = kern[,"dist"],y = kern[,"cumProb"],type="b",pch=16,ylim=c(0,1))
points(kern[,"dist"],kern[,"prob"],pch=16,col="steelblue")

# # compute pdf and cdf of kernel for Q matrix
# 
# # vectorize the matrix
# Q_vec = as.vector(Q)
# 
# # make bins according to unique distances
# bins = unique(as.vector(dist.F))
# 
# # empirical pdf and cdf
# Q_pdf.emp = normalize(sapply(bins, function(dd) sum(Q_vec[which(distVec.F == dd)])))
# Q_cdf.emp = sapply(bins, function(dd) sum(Q_vec[which(distVec.F <= dd)]))
# Q_cdf.emp = Q_cdf.emp / max(Q_cdf.emp)
# 
# # smoothed cdf
# Q_cdf.sth = ksmooth(bins[2 : length(bins)], Q_cdf.emp[2 : length(bins)], kernel = 'normal', bandwidth = 5 * dpill(bins, Q_cdf.emp))
# 
# # differentiate the smoothed cdf to obtain a smoothed pdf
# Q_pdf.sth = list(x = Q_cdf.sth$x[3 : (length(Q_cdf.sth$x) - 2)],
#                  y = normalize(numDiff(Q_cdf.sth$x, Q_cdf.sth$y)))
# 
# # redefine the smoothed cdf to be exactly consistent with the smoothed pdf
# Q_cdf.sth$x = Q_pdf.sth$x
# Q_cdf.sth$y = cumsum(Q_pdf.sth$y)


###############################################################################
#
#   DEPRECATED TEST CODE
#
###############################################################################


# testing one step aquatic ecology for emerge
# MicroTile$get_FemalePop()$get_pop()$ls()
# MicroTile$get_Landscape()$oneStep_AquaticEcology()
# MicroTile$get_Landscape()$addCohort()
# MicroTile$get_FemalePop()$get_pop()$ls()

# testing chooseHost
# MicroTile$get_FemalePop()$get_pop()$get("0_1_1")$set_inPointSet("f")
# debug(MicroTile$get_FemalePop()$get_pop()$get("0_1_1")$chooseHost)
# MicroTile$get_FemalePop()$get_pop()$get("0_1_1")$chooseHost()

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
