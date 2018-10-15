# MACRO Calibration for Bioko Island  ----
# Set up single-patch simulation
# Make sure that we can calibrate to PR, that our mathematics were correct
####

# Initialize libraries and directories ----
rm(list=ls());gc()
library(data.table)
library(ggplot2)
library(MASHmacro)
set.seed(0)
setwd("/Users/dtcitron/Documents/MASH/Bioko_Macro/TAG_2018")
# Source analysis functions
source("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/Multipatch_data_transform.R")


####
# Read in data ----
travel.model.data <- fread("Bioko_EIR_Surfaces/EIR_surfaces.csv")
# TaR matrix 1
TaR.1 <- read.csv(file = "/Users/dtcitron/Documents/MASH/Bioko_Macro/TAG_2018/Bioko_EIR_Surfaces/TAR1.csv")
TaR.1 <- as.matrix(TaR.1[, 2:202])
TaR.2 <- read.csv(file = "/Users/dtcitron/Documents/MASH/Bioko_Macro/TAG_2018/Bioko_EIR_Surfaces/TAR2.csv")
TaR.2 <- as.matrix(TaR.2[, 2:202])
# Fitting to reservoir data, 201-row data.table that includes pfpr, pop, h.1, h.2...
reservoir.data <- fread(file = "/Users/dtcitron/Documents/MASH/Bioko_Macro/TAG_2018/Bioko_EIR_Surfaces/pfpr_input.csv")
#  parameters
a = 0.3*0.9
b = 0.55
c = 0.15
r = 1./200 # rate at which people become cured
eta = 1./30 # rate at which prophylaxis wears off
p = 0.9 # fraction of surviving mosquitoes
g = 1 - p # fraction of dying mosquitoes
peip = p^11 # fraction of mosquitoes who survive incubation period
fever.pf = 0.1116336
treat.pf = 0.6025641
# output directory
directory = "BI_Macro_Calibration/"

####
# Some mathematical checks ahead of time ####
####
#odds.vector <- r/(1-rho)*pfpr.input/(1-(1+rho*r/eta/(1-rho))*pfpr.input)
#hist(abs(TaR.1 %*% reservoir.data$h.1 - odds.vector))
#hist(abs(TaR.2 %*% reservoir.data$h.2 - odds.vector))


####
# Define PfSI model parameters ----
####
PfSI.Setup(
  DurationPf = 200, mnChemoprophylaxisPf = 30, # setting duration of infectious period and Prophylaxis period
  FeverPf = fever.pf, TreatPf = treat.pf # setting fever/treatment parameters
)
SimBitePfSI.Setup()

####
# Set up Human Population
####
areaIds = sort(travel.model.data$areaId)
patch_ix = which(areaIds == 273)
patch.human.populations <- c(reservoir.data$pop.input[patch_ix], rep(0,7))
MACRO.Human.Setup(pathogen = "PfSI", tripFrequency = 0, tripDuration = 1) # define tripFreq and tripDur below


####
# Define Patch parameters ----
# Number of patches
n = 1 + 7 # four areas, with 7 regions for travel
# Aquatic ecology parameters
patch.lambda <- c(travel.model.data[areaId==273]$lambda.1, rep(0,7))
patch.lambda[1] <- lambda.1[patch_ix] ### this is a kluge for now
aquaPar = AquaPop_Emerge.Parameters(nPatch = n,lambda = patch.lambda, seasonality = FALSE)
# Create the movement matrix ####
# Needs to have zero diagonals, and all rows normalized
moveMat <- diag(n)
moveMat[1,] <- with(travel.model.data[areaId==areaIds[patch_ix]], c(0, p.off, p.ban, p.lub, p.mal, p.mok, p.ria, p.ure))
#rowSums(moveMat)
# Patch Parameters ####
patchPar = lapply(X = 1:n,FUN = function(i){
  list(
    bWeightZoo = 0,
    bWeightZootox = 0,
    travelWeight = moveMat[i,],
    reservoir = FALSE,
    resEIR = NULL
  )
})
# Designate the last 7 patches as reservoirs ####
eir.1 <- reservoir.data$h.1/b
eir.2 <- reservoir.data$h.2/b
eir <- eir.1[c(patch_ix,195:201)]
for(i in 2:n){
  patchPar[[i]]$reservoir <- TRUE
  patchPar[[i]]$resEIR <- eir[i]
}

# PfPR ----
pfpr = reservoir.data$pfpr.input[c(patch_ix,195:201)]


####
# Define Mosquito parameters ----
# (Some optional math checks first)
#EIR = h/b = maz
#travel.model.data$m.1*a*travel.model.data$z.1 - (reservoir.data$h.1/b)[1:194]
#travel.model.data$m.1*a*travel.model.data$z.1 - eir.1[1:194]
#Z = z*M = z*lambda*p/(1-p) = EIR*H/a
#eir.1[1:194]/a*reservoir.data$pop.input[1:194] - travel.model.data$z.1*travel.model.data$lambda.1*p/(1-p)

Z.1 = eir.1/a*reservoir.data$pop.input
Z <- c(Z.1[patch_ix], rep(0, 7) )
psi = diag(n)
mosquitoPar = list(model="RM", M=patch.lambda*p/(1-p),EIP = rep(11,365),
                   Y=Z/peip, Z=Z,
                   p=0.9, f=0.3, Q=0.9, v=20, psi = psi)

####
# Define Human Parameters ----
n_humans = sum(patch.human.populations)
patch_id = rep(x = 1:n,patch.human.populations)
home_id = rep(x = 1:n,patch.human.populations)
human_ages = unlist(lapply(X = patch.human.populations[1],FUN = siteAges_HumanPop))
# set biting weights
human_bWeight = rep(1, n_humans)
# Human Parameters
humanPar = lapply(X = 1:n_humans,function(i){
  list(
    houseID = home_id[i],
    patchID = patch_id[i],
    homeHouseID = home_id[i],
    homePatchID = patch_id[i],
    age = human_ages[i],
    bWeight = human_bWeight[i],
    tripDuration = c(10,3,3,3,3,3,3,3,3,3,3),# this is if we use TaR matrix 1
    # tripDuration = c(travel.model.data$to.time.e[patch_ix],3,3,3,3,3,3,3,3,3,3), # This is if we use TaR matrix 2
    tripFrequency = travel.model.data$freq.model.fit[patch_ix]
  )
})


####
# Create a tile! ----
tile = MacroTile$new(nPatch = n,
                     AquaPar = aquaPar,
                     PatchPar = patchPar,
                     MosquitoPar = mosquitoPar,
                     HumanPar = humanPar,
                     directory = directory)

####
# Run a single simulation ----
set.seed(3)
tile$simMacro(tMax = 730, PfPAR = pfpr)


####
# Analyze ----
human.pathogen.path <- paste0(directory, "/HumanPathogen_Run0.csv")
human.move.path <- paste0(directory, "/HumanMove_Run0.csv")
t4 <- SIP.Conversion.Curves(human.pathogen.path, human.move.path, patch.human.populations, 730)
ggplot(data = t4) +
  geom_line(mapping = aes(x = time, y = N, color = status)) +
  facet_wrap(~location, ncol = 3, labeller = label_parsed) +
  scale_color_manual(name = "Status",
                     values = c("#ff0000", "#000cff", "#00ff1d"),
                     breaks = c("I", "S", "P"),
                     labels = c("Infected (PR)", "Susceptible", "Protected")) +
  xlim(0,730) + ylim(0,300) +
  xlab("Time") + ylab("N")


mos.path <- paste0(directory, "/Mosquito_Run0.csv")
m <- fread(mos.path)
mean(m[state=="Z"]$patch1)
mean(m[state=="M"]$patch1)
patch.lambda[1]*9

tile$get_Patch(1)

####
# Checking on EIR and such for this patch - what does it need to be?  And what inputs am I giving it?
# At the very least, I should be able to calibrate in the no-travel limit!

# These things match just fine!
odds.vector[patch_ix]
r*pfpr[1]/(1-rho)/(1 -(1 + rho*r/eta/(1-rho))*pfpr[1] )
as.vector(TaR.1[patch_ix,c(patch_ix, 195:201)] ) %*% as.vector(b*eir)
as.vector(TaR.1[patch_ix,c(patch_ix, 195:201)] ) %*% h.1[c(patch_ix, 195:201)]

# What to check next, then?
# Possible that our calculation of kappa was wrong: there are no visitors coming here because there are no visitors
# So the mosquitoes who get infected at the patch of interest never themselves see any new humans
# Check - kappa would be much smaller in this case - what does that mean?


