###############################################################################
#
# MASH TEAM: BIOKO ISLAND UNIT
# preliminary runs for Malabo only with MASH(MACRO) v0.1
# October 27, 2017
#
###############################################################################

###############################################################################
# Setup data and parameters for simulation
###############################################################################

rm(list=ls());gc()
library(MASH)

seed = 42
set.seed(seed)

# initialize classes for MACRO
MACRO.Humans.Setup(pathogenModule = "PfSI")

# setup class methods for PfSI and SimBite modules
# MACRO relies on SimBitePfSI module for adding bites to humans
PfSI.Setup()
SimBitePfSI.Setup()

# MACRO Patch initialization
MACRO.Patch.Emerge.Setup() # 'Emerge' model

# Patch Parameters:
# Set working directory to where the bioko data is
# setwd("/Users/slwu89/Desktop/MASH-dev/DavidSmith/Bioko")
biokoPOP <- read.csv("/Users/slwu89/Desktop/MASH-Main/MASH-dev/DavidSmith/Bioko/bioko_areas.csv")
# load in PfPR for bioko island
biokoPFPR <- read.csv("/Users/slwu89/Desktop/MASH-Main/MASH-dev/DavidSmith/Bioko/pfprmap_area_CAGcorrectNA.csv")
# Merge the two data sets on the areaIds
bioko <- merge(biokoPOP, biokoPFPR, by = "areaId", all =FALSE)

# Try running this with only the Malabo patches
malabo <- c(212, 218, 219, 220, 271, 272, 273, 274, 275, 276, 277, 278, 279, 330, 331, 332, 333, 334,
            335, 336, 337, 338, 390, 391, 392, 393, 394, 395, 396, 397, 398, 448, 449, 450, 451, 452,
            453, 454, 455, 507, 508, 509, 510, 511, 512, 513, 514)

malabo <- data.frame(areaId = malabo)
bioko <- merge(bioko, malabo, by = "areaId", all = FALSE)

nPatch = nrow(bioko) #nPatch = 10

# Turn off treatment, to get the baseline...
#PfSI.Setup(TreatPf = 0.0)
# Set up tile parameters
tileParameters = MACRO.Tile.Parameters(N = nPatch,
                                       aquaModule = "emerge", aquaPars = list(N=nPatch,lambda=rep(50,nPatch)),
                                       pathogenModule = "PfSI")
# Manually set tile parameters using hte bioko data
popTotal <- sum(bioko$pop[1:nPatch])
tileParameters$HumanPop_PAR$nHumans <- popTotal
tileParameters$HumanPop_PAR$sitePops <- bioko$pop[1:nPatch] # might need this to be a double not an int
tileParameters$HumanPop_PAR$humanIDs <- as.integer(1:popTotal)
tileParameters$HumanPop_PAR$siteHumanIDs <- split(1:popTotal, rep(1:nPatch, bioko$pop[1:nPatch]))
tileParameters$HumanPop_PAR$homeIDs <- rep(1:nPatch, bioko$pop[1:nPatch])
sa <- siteAges(popTotal)
tileParameters$HumanPop_PAR$bDay <- -sa
tileParameters$HumanPop_PAR$steAges <- split(sa, rep(1:nPatch, bioko$pop[1:nPatch]))
tileParameters$HumanPop_PAR$bWeight <- rgamma(popTotal, 1)

tileParameters$MacroMosquitoPop_PAR$M_density = rep(200,nPatch)

# Initialize PfPR on each tile
PfPR <- bioko$pfpr[1:nPatch]

###############################################################################
# Run simulations in parallel
###############################################################################

library(parallel)
library(snow)
library(foreach)
library(doSNOW)

n = parallel::detectCores()
cluster <- snow::makeCluster(spec = n,type = "SOCK") # make a cluster
doSNOW::registerDoSNOW(cluster) # register the cluster with the SNOW package so that it will work with foreach()

out = foreach(i=1:n,.inorder = FALSE,.packages = c("MASH"),.export = c("PfPR","tileParameters"),.verbose = TRUE) %dopar% {
  set.seed(i)

  # initialize classes for MACRO
  MACRO.Humans.Setup(pathogenModule = "PfSI")
  PfSI.Setup()
  SimBitePfSI.Setup()
  MACRO.Patch.Emerge.Setup() # 'Emerge' model

  tile <-  MacroTile$new(MacroTile_PAR = tileParameters)
  tile$init_PfSI(PfPR = PfPR)
  tile$simMacro(5*365)
  pfsiHist = tile$get_HumanPop()$get_PfSI_history()

  # PID = Sys.getpid()
  # saveRDS(object = pfsiHist,file = paste0("/Users/slwu89/Desktop/MASH-dev/SeanWu/bioko_island_baseline_output_",PID,"_",i,".rds"))
}
saveRDS(object = out,file = paste0("/Users/slwu89/Desktop/MASH-dev/SeanWu/bioko_island_baseline_output.rds"))

# # try to do the above thing in parallel
# library(parallel)
# library(snow)
# library(iterators)
# library(foreach)
# library(doSNOW)
# cluster <- snow::makeCluster(spec = parallel::detectCores(),type = "SOCK") # make a cluster
# doSNOW::registerDoSNOW(cluster) # register the cluster with the SNOW package so that it will work with foreach()
# parallel::clusterEvalQ(cl = cluster,expr = {
#   dyn.load("/Users/slwu89/Desktop/code/qinlong/model_full.so")
# })
#
# result <- foreach(param=iter(obj = pass, by='row'), .combine = "rbind", .inorder = FALSE, .packages = c("deSolve"),
#                   .export = c("PeakValuePredict","pass","Temp","PrecipOrigin","DiapauseFlag","Evaporation","SpillOverDay","NewCase","CalculateRealWater","initState","day","times"),
#                   .verbose = TRUE) %:%
#   foreach(j=1:274,.combine = "rbind") %dopar% {
#     import3 = j + 790
#     outbreakSize = PeakValuePredict(param = param,import3 = import3)
#     data.frame(import3=import3,t(outbreakSize),simid=as.integer(rownames(param)))
#   }
#
#
#
#
#
# # Setting up tiles - this is when memory is allocated
# system.time(tile <-  MacroTile$new(MacroTile_PAR = tileParameters))
#
#
# tile$init_PfSI(PfPR = PfPR)
# # Here's Sean's code for queueing up some vaccinations
# # Vaccinations are off for now, this is the baseline simulation
# #tile$get_HumanPop()$queueVaccination_SimBitePfSI(tVaccine = (365*1),tTreat = (365*1)+1,fracPop = 0.75)
#
# # Simulate for 5 years, see how long that takes
# system.time(tile$simMacro(5*365))
#
# pfsiHist = tile$get_HumanPop()$get_PfSI_history()
#
# saveRDS(pfsiHist, sprintf(paste("bioko_island_baseline_output_",seed,".rds", sep="")))
#
