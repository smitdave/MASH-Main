# MACRO Calibration for Bioko Island  ----
# Set up single-patch simulation
# Make sure that we can calibrate to PR, that our mathematics were correct
####

# Initialize libraries and directories ----
rm(list=ls());gc()
library(data.table)
library(ggplot2)
library(MASS)
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
rho = fever.pf*treat.pf

####
# Pick an area to simulate ----
####
areaIds = sort(travel.model.data$areaId)
patch_areaId = 2087#2083 #735 #582 #273 #2324#
# output directory
directory = paste0("BI_Macro_Calibration/patch_",patch_areaId,"/")

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

patch_ix = which(areaIds == patch_areaId)
patch.human.populations <- c(reservoir.data$pop.input[patch_ix], rep(0,7))
MACRO.Human.Setup(pathogen = "PfSI", tripFrequency = 0, tripDuration = 1) # define tripFreq and tripDur below


####
# Define Patch parameters ----
# Number of patches
n = 1 + 7 # four areas, with 7 regions for travel
# Aquatic ecology parameters
patch.lambda <- c(travel.model.data[areaId==patch_areaId]$lambda.1/.9, rep(0,7)) ### Kluge for now - not sure where the discrepancy comes from...
aquaPar = AquaPop_Emerge.Parameters(nPatch = n,lambda = patch.lambda, seasonality = FALSE)
# Create the movement matrix ####
# Needs to have zero diagonals, and all rows normalized
moveMat <- diag(n)
moveMat[1,] <- with(travel.model.data[areaId==patch_areaId], c(0, p.off, p.ban, p.lub, p.mal, p.mok, p.ria, p.ure))
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

Z.1 = eir.1/a*reservoir.data$pop.input/diag(TaR.1)
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
    tripDuration = c(1,10,3,3,3,3,3,3),# this is if we use TaR matrix 1
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

#View(t4)
# Rough check on occupancies
holder <- dcast(t4[time > 200, sum(N), by = c("time", "location")], time ~ location)
holder[is.na(holder)] <- 0

c(mean(holder$'1')/n_humans, mean(holder$'2')/n_humans, mean(holder$'3')/n_humans, mean(holder$'4')/n_humans,
  mean(holder$'5')/n_humans, mean(holder$'6')/n_humans, mean(holder$'7')/n_humans, mean(holder$'8')/n_humans)

TaR.1[patch_ix,c(patch_ix, 195:201)]

# Rough check on total pfpr - not too far away, well within margin of error
mean(t4[time > 200 & status == "I", sum(N), by = "time"]$V1)
sd(t4[time > 200 & status == "I", sum(N), by = "time"]$V1)
pfpr[1]*n_humans

pr.actual <- mean(t4[status == "I" & time > 200][, sum(N), by = c("time")]$V1)/n_humans
pr.actual

psi.h <- TaR.1[patch_ix, c(patch_ix, c(195:201))] %*% reservoir.data$h.1[c(patch_ix, c(195:201))]
psi.h/(r/(1-rho) + (1+rho*r/eta/(1-rho))*psi.h)
pfpr[1]

# and looking at total FOI -
r/(1-rho)*pr.actual/(1-(1+rho*r/eta/(1-rho))*pr.actual)
r/(1-rho)*pfpr[1]/(1-(1+rho*r/eta/(1-rho))*pfpr[1])
TaR.1[patch_ix, c(patch_ix, c(195:201))] %*% reservoir.data$h.1[c(patch_ix, c(195:201))]


####
# Simulation Ensemble!
set.seed(0)
tile = MacroTile$new(nPatch = n,
                     AquaPar = aquaPar,
                     PatchPar = patchPar,
                     MosquitoPar = mosquitoPar,
                     HumanPar = humanPar,
                     directory = directory)
for (i in 1:25){
  set.seed(i + 100)
  tile$simMacro(tMax = 5*365, PfPAR = pfpr)
  tile$resetMacro(patchPar, mosquitoPar, humanPar)
}

#directory = "BI_Macro_Calibration/patch_273/"#"BI_Macro_Calibration/patch_582/"#"BI_Macro_Calibration/patch_2083/"#
human.movement.outputs <- list.files(directory, pattern = "HumanMove_Run*")
human.pathogen.outputs <- list.files(directory, pattern = "HumanPathogen_Run*")

# Assemble matrix of results
m <- matrix(NA, ncol = length(human.movement.outputs), nrow = 3*n*5*365)
for (i in 1:length(human.movement.outputs)){
  human.pathogen.path <- paste0(directory, "/", human.pathogen.outputs[i], sep = "")
  human.move.path <- paste0(directory, "/", human.movement.outputs[i], sep = "")
  t <- SIP.Conversion.Curves(human.pathogen.path, human.move.path, patch.human.populations, 5*365)
  #paste0("run.",as.character(i))
  h <- SIP.FULL(t, n, 5*365, status.list = c("S", "I", "P"))
  m[,i] <- h$N
}

m.sds <- apply(m, 1, sd)
m.means <- apply(m, 1, mean)
h$N.means <- m.means
h$N.sds <- m.sds
#h.2083 <- h
#h.582 <- h
#h.273 <- h

fwrite(h, file = paste0(directory,"combined_results_datatable.csv"))

# Now we can plot the mean and standard deviations of the ensemble!
ggplot(data = h) +
  geom_line(mapping = aes(x = time, y = N.means, color = status)) +
  geom_line(mapping = aes(x = time, y = N.means+N.sds, color = status)) +
  geom_line(mapping = aes(x = time, y = N.means-N.sds, color = status)) +
  facet_wrap(~location, ncol = 3, labeller = label_parsed) +
  scale_color_manual(name = "Status",
                     values = c("#ff0000", "#000cff", "#00ff1d"),
                     breaks = c("I", "S", "P"),
                     labels = c("Infected (PR)", "Susceptible", "Protected")) +
  xlim(0,5*365) + ylim(0,300) +
  xlab("Time") + ylab("N")

# Check baseline PR
h[time > 500 & status == "I", sum(N.means), by = time]

plot(h[status == "I", sum(N.means), by = time])

mean(h[time > 500 & status == "I", sum(N.means), by = time]$V1)
sd(h[time > 500 & status == "I", sum(N.means), by = time]$V1)
pfpr[1]*n_humans

pr.actual <- mean(h[time > 1000 & status == "I", sum(N.means), by = time]$V1)/mean(h[time > 1000 & status %in% c("I","S"), sum(N.means), by = time]$V1)
pr.actual



psi.h <- TaR.1[patch_ix, c(patch_ix, c(195:201))] %*% reservoir.data$h.1[c(patch_ix, c(195:201))]
psi.h/(r/(1-rho) + (1+rho*r/eta/(1-rho))*psi.h)
pfpr[1]

# and looking at total FOI -
r/(1-rho)*pr.actual/(1-(1+rho*r/eta/(1-rho))*pr.actual)
r/(1-rho)*pfpr[1]/(1-(1+rho*r/eta/(1-rho))*pfpr[1])

#What it should be
TaR.1[patch_ix, c(patch_ix, c(195:201))] %*% reservoir.data$h.1[c(patch_ix, c(195:201))]
#What it is in the simulation
occupancy[2:8] %*% reservoir.data$h.1[c(195:201)] + occupancy[1] * b*a*mean(mos0[time > 1000]$Z)/n_humans

# If we replace the local FOI with what it needs to be?
occupancy[2:8] %*% reservoir.data$h.1[c(195:201)] + occupancy[1] * travel.model.data[areaId==patch_areaId]$h.1

travel.model.data[areaId==patch_areaId]$m.1*travel.model.data[areaId==patch_areaId]$z.1*a*b
travel.model.data[areaId==patch_areaId]$h.1

# This is Z
travel.model.data[areaId==patch_areaId]$z.1*travel.model.data[areaId==patch_areaId]$lambda.1*9
mean(mos0[time > 1000]$Z)

# Ignorning the home patch? Very close
# Home patch is the problem here
TaR.1[patch_ix, c(195:201)] %*% reservoir.data$h.1[c(195:201)]
occupancy[2:8] %*% reservoir.data$h.1[c(195:201)]

# Check on occupancy
# See if the fraction of people spending time in different locations matches with the TaR matrix?
occupy <- dcast(h[time > 1000, sum(N.means), by = c("time", "location")], time ~ location)

occupancy <- c(mean(occupy$'1')/n_humans, mean(occupy$'2')/n_humans, mean(occupy$'3')/n_humans,
               mean(occupy$'4')/n_humans, mean(occupy$'5')/n_humans, mean(occupy$'6')/n_humans,
               mean(occupy$'7')/n_humans, mean(occupy$'8')/n_humans)
occupancy.sd <- c(sd(occupy$'1')/n_humans, sd(occupy$'2')/n_humans, sd(occupy$'3')/n_humans,
                  sd(occupy$'4')/n_humans, sd(occupy$'5')/n_humans, sd(occupy$'6')/n_humans,
                  sd(occupy$'7')/n_humans, sd(occupy$'8')/n_humans)
occupancy
occupancy.sd

TaR.1[patch_ix, c(patch_ix, 195:201)]


  
### Trip times
trip.times <- c()
human.movement.outputs <- list.files(directory, pattern = "HumanMove_Run*")
for (file in human.movement.outputs){
  move0 <- fread(paste0(directory, file))
  for (human.ix in 1:n_humans){
    trip.times <- c(trip.times, 
                    move0[order(humanID)][humanID==human.ix][(which(move0[order(humanID)][humanID==human.ix]$location == 2) + 1)]$time - 
                      move0[order(humanID)][humanID==human.ix][which(move0[order(humanID)][humanID==human.ix]$location == 2)]$time)
  }
}
summary(trip.times)
hist(trip.times)

### Home times
home.times <- c()
human.movement.outputs <- list.files(directory, pattern = "HumanMove_Run*")
for (file in human.movement.outputs){
  move0 <- fread(paste0(directory, file))
  for (human.ix in 1:n_humans){
    home.times <- c(home.times, move0[order(humanID)][humanID==human.ix][which(move0[order(humanID)][humanID==human.ix]$location == 1)+1]$time-move0[order(humanID)][humanID==human.ix][which(move0[order(humanID)][humanID==human.ix]$location == 1)]$time)
  }
}
summary(home.times)
hist(home.times)
1/travel.model.data$freq.model.fit[patch_ix]
