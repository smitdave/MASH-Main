########################################
# Setting up a simulation using the Bioko Island travel model
# Pick clusters 1, 5, 14, 18 - four clusters + 7 regions for now
########################################

rm(list=ls());gc()

## Read in arguments after "--args":
arguments <- commandArgs(trailingOnly = T)
dir <- arguments[1] # DIRECTORY TO USE
cluster.ids <- as.integer(arguments[2]) # CLUSTER IDS TO USE

cluster_identifier <- paste(cluster.ids, collapse = '_')

# dir <- "/homes/georgoff/MASH-Main/MASH-dev/AlecGeorgoff/Bioko/Bioko_Island_Cluster_Simulations"
output_dir_single <- paste0(dir, "/BI_n_patch_trial_outputs/Single_trajectory_", cluster_identifier)
output_dir_ensemble <- paste0(dir, "/BI_n_patch_trial_outputs/Ensemble_trajectories_", cluster_identifier)

library(data.table)
library(ggplot2)
library(MASHmacro)
source(paste0(dir, "/Multipatch_data_transform.R"))
set.seed(0)


########################################
# Data from calibration
########################################
source(paste0(dir, "/Bioko_Island_Simulation_Setup.R"))


########################################
# List of Clusters used
########################################
cluster.ids <- cluster.ids # Redundant, but for clarity!
n.clusters <- length(cluster.ids)

########################################
# Define PfSI model parameters
########################################
PfSI.Setup(
  DurationPf = 200, mnChemoprophylaxisPf = 30, # setting duration of infectious period and Prophylaxis period
  FeverPf = fever.pf, TreatPf = treat.pf # setting fever/treatment parameters
)
SimBitePfSI.Setup()


########################################
# Define Human parameters
########################################
# Vector of human populations in each patch
patch.human.populations <- cluster.human.pops[c(cluster.ids, c(42:48))]
MACRO.Human.Setup(pathogen = "PfSI",tripFrequency = travel.freq, tripDuration = 3) # travel.freq is travel frequency


########################################
# Define Patch parameters
########################################
# Number of patches
n = n.clusters + 7 # 10 areas, with 7 regions for travel

# Aquatic ecology parameters
patch.lambda <- c(area.lambda[cluster.ids], rep(0,7))
aquaPar = AquaPop_Emerge.Parameters(nPatch = n,lambda = patch.lambda, seasonality = FALSE)

# Create the movement matrix
# Needs to have zero diagonals, and all rows normalized
moveMat <- P.ij[c(cluster.ids, c(42:48)), c(cluster.ids, c(42:48))]
diag(moveMat)[1:n] <- 0
moveMat[1:n,] <- moveMat[1:n,]/travel.freq
# Patch Parameters
patchPar = lapply(X = 1:n,FUN = function(i){
  list(
    bWeightZoo = 0,
    bWeightZootox = 0,
    travelWeight = moveMat[i,],
    reservoir = FALSE,
    resEIR = NULL
  )
})
# Designate the last 7 patches as reservoirs
eir <- area.EIR[c(cluster.ids, c(42:48))]
for(i in (n.clusters+1):n){
  patchPar[[i]]$reservoir <- TRUE
  patchPar[[i]]$resEIR <- eir[i]
}

# PfPR
pfpr = x.pfpr.input[c(cluster.ids, c(42:48))]


########################################
# Define Mosquito parameters
########################################
# numbers of infectious mosquitoes
Z = eir/a*patch.human.populations
psi = diag(n)
mosquitoPar = list(model="RM", M=patch.lambda*p/(1-p),EIP = rep(11,365),
                   Y=Z/peip, Z=Z,
                   p=0.9, f=0.3, Q=0.9, v=20, psi = psi)


########################################
# Define Human Parameters
########################################
n_humans = sum(patch.human.populations)
patch_id = rep(x = 1:n,patch.human.populations)
home_id = rep(x = 1:n,patch.human.populations)
human_ages = unlist(lapply(X = patch.human.populations[1:n.clusters],FUN = siteAges_HumanPop))
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
    bWeight = human_bWeight[i]
  )
})


########################################
# Create a tile!
########################################
# Where the outputs go
tile = MacroTile$new(nPatch = n,AquaPar = aquaPar,PatchPar = patchPar,MosquitoPar = mosquitoPar,HumanPar = humanPar,directory = output_dir_single)


########################################
# Run a single simulation
########################################
set.seed(0)
tile$simMacro(tMax = 750, PfPAR = pfpr)


########################################
# Create a plot of the single simulation
########################################
# Convert results and make a plot
human.pathogen.path <- paste0(output_dir_single, "/HumanPathogen_Run0.csv")
human.move.path <- paste0(output_dir_single, "/HumanMove_Run0.csv")
t <- SIP.Conversion.Curves(human.pathogen.path, human.move.path, patch.human.populations, 750)
h <- SIP.FULL(t, n, 750, status.list = c("S", "I", "P"))
# Add location names, to properly label the output
# Add pfpr values, to check on our calibration
location.names.table <- data.table(location = c(1:n),
                                   loc.name = c(as.character(cluster.ids), "Off", "Baney", "Luba", "Malabo", "Moka", "Riaba", "Ureka"),
                                   loc.pfpr = pfpr*patch.human.populations # expected number of infected humans
                                   )
h <- merge(h, location.names.table, by = "location")
# Make a plot
pdf(file = paste0(output_dir_single, "/results_pdf_single.pdf"))
ggplot(data = h) +
  geom_line(mapping = aes(x = time, y = N, color = status)) +
  facet_wrap(~loc.name, ncol = 3) +
  geom_hline(data=location.names.table[location %in% c(1:n.clusters, 11,14)],
             aes(yintercept = loc.pfpr)) +
  scale_color_manual(name = "Status",
                     values = c("#ff0000", "#000cff", "#00ff1d"),
                     breaks = c("I", "S", "P"),
                     labels = c("Infected (PR)", "Susceptible", "Protected")) +
  xlim(0,750) + ylim(0,250) +
  xlab("Time") + ylab("N")

dev.off()

# Mean PfPR in the simulation
h[time > 500 & status == "I"][, mean(N), by = location]$V1[1:n.clusters]/patch.human.populations[1:n.clusters]
# PfPR from the calibration
pfpr[1:n.clusters]

# Check your output against the output saved at:
# "/Users/dtcitron/Documents/MASH/Bioko_Macro/Bioko_Island_Simulation_Setup/BI_n_patch_trial_outputs/Single_trajectory_verify"

########################################
# Simulation Ensemble
########################################
# reset the tile, with a new directory
tile = MacroTile$new(nPatch = n,AquaPar = aquaPar,PatchPar = patchPar,MosquitoPar = mosquitoPar,HumanPar = humanPar,directory = output_dir_ensemble)
for (i in 1:10){
  set.seed(i + 3)
  tile$simMacro(tMax = 750, PfPAR = pfpr)
  tile$resetMacro(patchPar, mosquitoPar, humanPar)
}


# Assemble matrix of results
human.movement.outputs <- list.files(output_dir_ensemble, pattern = "HumanMove_Run*")
human.pathogen.outputs <- list.files(output_dir_ensemble, pattern = "HumanPathogen_Run*")
m <- matrix(NA, ncol = 10, nrow = 3*n*750)
for (i in 1:10){
  human.pathogen.path <- paste0(output_dir_ensemble, "/", human.pathogen.outputs[i], sep = "")
  human.move.path <- paste0(output_dir_ensemble, "/", human.movement.outputs[i], sep = "")
  t <- SIP.Conversion.Curves(human.pathogen.path, human.move.path, patch.human.populations, 750)
  h <- SIP.FULL(t, n, 750, status.list = c("S", "I", "P"))
  m[,i] <- h$N
}

# Calculate statistics over the ensemble, and add those to your data table
m.sds <- apply(m, 1, sd)
m.means <- apply(m, 1, mean)
h$N.means <- m.means
h$N.sds <- m.sds

# Add location names, to properly label the output
# Add pfpr values, to check on our calibration
location.names.table <- data.table(location = c(1:n),
                                   loc.name = c(as.character(cluster.ids), "Off", "Baney", "Luba", "Malabo", "Moka", "Riaba", "Ureka"),
                                   loc.pfpr = pfpr*patch.human.populations # expected number of infected humans
)
h <- merge(h, location.names.table, by = "location")

# Now we can plot the mean and standard deviations of the ensemble!
pdf(file = paste0(output_dir_ensemble, "/results_pdf_ensemble.pdf"))
ggplot(data = h[location %in% c(1:n.clusters, n.clusters+1,n.clusters+4)]) +
  geom_line(mapping = aes(x = time, y = N.means, color = status)) +
  geom_line(mapping = aes(x = time, y = N.means+N.sds, color = status)) +
  geom_line(mapping = aes(x = time, y = N.means-N.sds, color = status)) +
  facet_wrap(~loc.name, ncol = 3) +
  geom_hline(data=location.names.table[location %in% c(1:n.clusters, n.clusters+1,n.clusters+4)],
             aes(yintercept = loc.pfpr)) +
  scale_color_manual(name = "Status",
                     values = c("#ff0000", "#000cff", "#00ff1d"),
                     breaks = c("I", "S", "P"),
                     labels = c("Infected (PR)", "Susceptible", "Protected")) +
  xlim(0,750) + ylim(0,250) +
  xlab("Time") + ylab("N")

dev.off()

# Means and standard deviations in PfPR
h[time > 500 & status == "I"][, mean(N.means), by = location]$V1[1:n.clusters]/patch.human.populations[1:n.clusters]
h[time > 500 & status == "I"][, mean(N.sds), by = location]$V1[1:n.clusters]/patch.human.populations[1:n.clusters]
pfpr[1:n.clusters]

# Check your output against the output saved at:
# "/Users/dtcitron/Documents/MASH/Bioko_Macro/Bioko_Island_Simulation_Setup/BI_n_patch_trial_outputs/Ensemble_trajectories_verify"
