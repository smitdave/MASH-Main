# Set up: run for 900 people on 3 patches, the lowest, the highest, and the middle (and some others) \

rm(list=ls())
gc()
library(MASH)
source("../../DanielCitron/Bioko/read_macro_pfsi_sim.R")

seed = 45
set.seed(seed)

# initialize classes for MACRO
MACRO.Humans.Setup(pathogenModule = "PfSI")

# setup class methods for PfSI and SimBite modules
# MACRO relies on SimBitePfSI module for adding bites to humans
PfSI.Setup(mnPEPf = 365, vrPEPf = 30)
SimBitePfSI.Setup()

# MACRO Patch initialization
MACRO.Patch.Emerge.Setup() # 'Emerge' model

# Patch Parameters
# Let's start figuring out how to load in our population data for bioko island
setwd("/Users/dtcitron/Documents/MASH/MASH-Main/MASH-dev/DavidSmith/Bioko")
biokoPOP <- read.csv("bioko_areas.csv")
# load in PfPR for bioko island
biokoPFPR <- read.csv("pfprmap_area_CAGcorrectNA.csv")
# load in Lambda values for bioko island
biokoLambda <- read.csv("data_area.csv")
# Merge the two three sets on the areaIds
bioko <- merge(biokoPOP, biokoLambda, by = "areaId", all =FALSE)

# Adjust the population by a factor of 200
#PopM <- ceiling(bioko$popm/200)
#PopF <- ceiling(bioko$popf/200)
#bioko$popm <- PopM
#bioko$popf <- PopF
#bioko$pop <- PopM + PopF

#malabo <- c(218, 219, 220, 272, 273, 274, 275, 276, 277, 278, 279, 330, 331, 332, 333, 334,
#            335, 336, 337, 338, 390, 391, 392, 393, 394, 395, 396, 397, 398, 448, 449, 450, 451, 452,
#            453, 454, 455, 507, 508, 509, 510, 511, 512, 513, 514) # 212, 271
#malabo <- data.frame(areaId = malabo)

names(bioko)

hist(bioko$pfpr)

bioko[bioko$areaId==2083,] # this is at the top, with pfpr of .3775, population 302
# adjust lambda by a factor of 16
bioko[bioko$areaId==571,] # this is the upper quintile, with pfpr of 0.210375, pop 600
# adjust lambda by a factor of 32
bioko[bioko$areaId==220,] # this is at the middle, with pfpr of .154, population 3300
# adjust lambda by a factor of 256
bioko[bioko$areaId==1990,] # this is at the lower quintile, with pfpr of .071, population 100
# adjust lambda by a factor of 8
bioko[bioko$areaId==2152,] # this is at the bottom, with pfpr of .02775, population 300
# adjust lambda by a factor of 18

patchPops <- c(100,100,100)
popTotal <- sum(patchPops)

nPatch = length(patchPops)

# pfpr and lambda values:
xind <- 2152
ll <- bioko[bioko$areaId==xind,]$lambda/18
# pfpr values
pp <- bioko[bioko$areaId==xind,]$pfpr
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
system.time(tile <- MacroTile$new(MacroTile_PAR = tileParameters))

# Initialize PfPR on each tile
PfPR <- rep(pp, nPatch)
tile$init_PfSI(PfPR = PfPR)

# Here's Sean's code for queueing up some vaccinations
tile$get_HumanPop()$queueVaccination_SimBitePfSI(tVaccine = (365*1),tTreat = (365*1)+1,fracPop = 0.9)
#tile$get_HumanPop()$queueVaccination_SimBitePfSI(tVaccine = (100),tTreat = (100)+1,fracPop = 0.5)

system.time(tile$simMacro(365*5))

pfsiHist = tile$get_HumanPop()$get_PfSI_history()
plot_PfSI(pfsiHist)

pfpr <- "00277"#"007175"#"0154"#"0212"#"037775"#"0007"#
print(pfpr)
#saveRDS(pfsiHist, sprintf(paste("../../DanielCitron/Bioko/Bioko_adjusted_baseline_",pfpr,"_seed_",seed,".rds", sep="")))
#saveRDS(pfsiHist, sprintf(paste("../../DanielCitron/Bioko/Bioko_adjusted_pevaxx50_",pfpr,"_seed_",seed,".rds", sep="")))
#saveRDS(pfsiHist, sprintf(paste("../../DanielCitron/Bioko/Bioko_adjusted_pevaxx80_",pfpr,"_seed_",seed,".rds", sep="")))
#saveRDS(pfsiHist, sprintf(paste("../../DanielCitron/Bioko/Bioko_adjusted_pevaxx90_",pfpr,"_seed_",seed,".rds", sep="")))

t_grid = seq(0,365*5)
#pfsiHist <- readRDS("../../DanielCitron/Bioko/Bioko_pevaxx80_037775_seed_44.rds")
prev.trajs <- pop_SI(pfsiHist, t_grid)
mean(prev.trajs$I)
pp*popTotal

### These are the prevalence (SI) curves
#prev.trajs <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_baseline_0007_seed_44.rds")
# Make some plots of (S,I) vs time
# Also shows the number of vaccinations at each time step
ggplot(data = prev.trajs) +
  geom_point(aes(x=t, y=S), col = 'blue') +
  geom_point(aes(x=t, y=I),  col = 'red') +
  geom_line(aes(x=t, y=PEvaxx), col = 'green')
# And to get the prevalence vs. time:
#prev.trajs$I

print(pfpr)
#saveRDS(prev.trajs, sprintf(paste("../../DanielCitron/Bioko/BiokoPrevalence_adjusted_baseline_",pfpr,"_seed_",seed,".rds", sep="")))
#saveRDS(prev.trajs, sprintf(paste("../../DanielCitron/Bioko/BiokoPrevalence_adjusted_pevaxx50_",pfpr,"_seed_",seed,".rds", sep="")))
#saveRDS(prev.trajs, sprintf(paste("../../DanielCitron/Bioko/BiokoPrevalence_adjusted_pevaxx80_",pfpr,"_seed_",seed,".rds", sep="")))
#saveRDS(prev.trajs, sprintf(paste("../../DanielCitron/Bioko/BiokoPrevalence_adjusted_pevaxx90_",pfpr,"_seed_",seed,".rds", sep="")))



### Now, we are going to sort these into the relevant spots on Bioko Island:
v1 <- bioko[bioko$areaId==2083,]$pfpr # this is at the top, with pfpr of .3775, population 302
v2 <- bioko[bioko$areaId==571,]$pfpr # this is the upper quintile, with pfpr of 0.210375, pop 600
v3 <- bioko[bioko$areaId==220,]$pfpr # this is at the middle, with pfpr of .154, population 3300
v4 <- bioko[bioko$areaId==1990,]$pfpr # this is at the lower quintile, with pfpr of .071, population 100
v5 <- bioko[bioko$areaId==2152,]$pfpr # this is at the bottom, with pfpr of .02775, population 300

# Divide up the pfpr range into intervals, so we can ballpark the 5 different parameter regimes
sorter <- c(0, (v5+v4)/2, (v4+v3)/2, (v3+v2)/2, (v1+v2)/2)
# Here's how we're going to sort each of the areas according to pfpr
sorter <- findInterval(bioko$pfpr, sorter)

# Read in a stack of baselines:
b1 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_baseline_00277_seed_45.rds")
b2 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_baseline_007175_seed_45.rds")
b3 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_baseline_0154_seed_45.rds")
b4 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_baseline_0212_seed_45.rds")
b5 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_baseline_037775_seed_45.rds")

baselines <- list(b1$I/300,b2$I/300,b3$I/300,b4$I/300,b5$I/300)

b5$I

# Want: a data frame where one col is area ID, the next is pfpr, the next is the I curve?

Bioko.baseline <- data.frame(areaId = bioko$areaId, PfPR = bioko$pfpr, PfPR.t = sorter)
for (i in 1:length(sorter)){
  Bioko.baseline[i,]$PfPR.t <- baselines[Bioko.baseline[i,]$PfPR.t[[1]]]
}

saveRDS(Bioko.baseline, "../../DanielCitron/Bioko/Bioko_Damage_Control/Bioko_baseline_PfPR_vs_t.rds")

# Read in a stack of 50% vaccination scenarios:
a1 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_pevaxx50_00277_seed_45.rds")
a2 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_pevaxx50_007175_seed_45.rds")
a3 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_pevaxx50_0154_seed_45.rds")
a4 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_pevaxx50_0212_seed_45.rds")
a5 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_pevaxx50_037775_seed_45.rds")

plot(t_grid, a5$I/300)

baselines <- list(a1$I/300,a2$I/300,a3$I/300,a4$I/300,a5$I/300)

Bioko.pevaxx50 <- data.frame(areaId = bioko$areaId, PfPR = bioko$pfpr, PfPR.t = sorter)
for (i in 1:length(sorter)){
  Bioko.pevaxx50[i,]$PfPR.t <- baselines[Bioko.pevaxx50[i,]$PfPR.t[[1]]]
}

saveRDS(Bioko.pevaxx50, "../../DanielCitron/Bioko/Bioko_Damage_Control/Bioko_pevaxx50_PfPR_vs_t.rds")

# Read in a stack of 80% vaccination scenarios:
a1 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_pevaxx80_00277_seed_45.rds")
a2 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_pevaxx80_007175_seed_45.rds")
a3 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_pevaxx80_0154_seed_45.rds")
a4 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_pevaxx80_0212_seed_45.rds")
a5 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_pevaxx80_037775_seed_45.rds")

plot(t_grid, a1$I/300)

baselines <- list(a1$I/300,a2$I/300,a3$I/300,a4$I/300,a5$I/300)

Bioko.pevaxx80 <- data.frame(areaId = bioko$areaId, PfPR = bioko$pfpr, PfPR.t = sorter)
for (i in 1:length(sorter)){
  Bioko.pevaxx80[i,]$PfPR.t <- baselines[Bioko.pevaxx80[i,]$PfPR.t[[1]]]
}

saveRDS(Bioko.pevaxx80, "../../DanielCitron/Bioko/Bioko_Damage_Control/Bioko_pevaxx80_PfPR_vs_t.rds")



# Read in a stack of 90% vaccination scenarios:
a1 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_pevaxx90_00277_seed_45.rds")
a2 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_pevaxx90_007175_seed_45.rds")
a3 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_pevaxx90_0154_seed_45.rds")
a4 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_pevaxx90_0212_seed_45.rds")
a5 <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/BiokoPrevalence_adjusted_pevaxx90_037775_seed_45.rds")

plot(t_grid, a4$I/300)

baselines <- list(a1$I/300,a2$I/300,a3$I/300,a4$I/300,a5$I/300)

Bioko.pevaxx90 <- data.frame(areaId = bioko$areaId, PfPR = bioko$pfpr, PfPR.t = sorter)
for (i in 1:length(sorter)){
  Bioko.pevaxx90[i,]$PfPR.t <- baselines[Bioko.pevaxx90[i,]$PfPR.t[[1]]]
}

saveRDS(Bioko.pevaxx90, "../../DanielCitron/Bioko/Bioko_Damage_Control/Bioko_pevaxx90_PfPR_vs_t.rds")

holder <- as.array(rep(0, nrow(Bioko.pevaxx50)))
for (i in 1:nrow(Bioko.pevaxx50)){
  holder[i] <- Bioko.pevaxx50[i,]$PfPR.t
}

as.matrix(holder)

holder <- matrix(0, nrow = nrow(Bioko.pevaxx50), ncol = 5*365)
for (i in 1:nrow(Bioko.pevaxx50)){
  for (j in 1:365){
    holder[i,j] <- Bioko.pevaxx50[i,]$PfPR.t[[1]][j]
  }
}

dim(holder)


### Now, how did we turn this into an image?