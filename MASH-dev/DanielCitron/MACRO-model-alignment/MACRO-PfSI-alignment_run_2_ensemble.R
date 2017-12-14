rm(list=ls());gc()
library(MASHmacro)

# Define the directory for the output:
directory = "/Users/dtcitron/Documents/MASH/MACRO-alignment/run_2_ensemble"

# Set the random seed
set.seed(0)

# PFSI MODULE PARAMETERS
PfSI.Setup(DurationPf = 200, LatentPf = 0, FeverPf = 0, TreatPf = 0)
SimBitePfSI.Setup()
MACRO.Human.Setup(pathogen = "PfSI",tripFrequency = 1/1000000, tripDuration = 1)

# PATCH PARAMETERS
# Number of Patches
n = 1

# aquatic ecology parameters
aquaPar = AquaPop_Emerge.Parameters(nPatch = n,lambda = rep(50,n),seasonality = FALSE)

# Human movement matrix (in general, for n>1, make movement follow AR(1) covariance structure, then subtract out the diagonal and renormalize)
moveMat = diag(1, n)
patchPar = lapply(X = 1:n,FUN = function(i){
  list(
    bWeightZoo = 0,
    bWeightZootox = 0,
    travelWeight = moveMat[i,]
  )
})

# MOSQUITO PARAMTERS
mosquitoPar = list(model="RM", M=rep(450),EIP = rep(11,365),p=0.9, f=0.3, Q=0.9, v=20, psi = diag(n))

# HUMAN PARAMETERS
# Number of humans in each patch
patch_humans = rep(500, n)
# Total number of humans
n_humans = sum(patch_humans)
# Assign patch and home IDs to humans
patch_id = rep(x = 1:n,patch_humans)
home_id = rep(x = 1:n,patch_humans)
human_ages = unlist(lapply(X = patch_humans,FUN = siteAges_HumanPop))
# Set biting weights
human_bWeight = rep(1, n_humans)

# Generate lists of parameters
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

# PfPR - defines initial number infected
pfpr = rep(0.9,n)

# GENERATE TILE
tile = MacroTile$new(nPatch = n,AquaPar = aquaPar,PatchPar = patchPar,MosquitoPar = mosquitoPar,HumanPar = humanPar,directory = directory)

# RUN AN ENSEMBLE OF SIMULATIONS
nrun = 100
tsteps = 2000
for (i in 1:nrun){
  tile$simMacro(tMax = tsteps, PfPAR = pfpr)
  tile$resetMacro(PatchPar = patchPar,MosquitoPar = mosquitoPar,HumanPar = humanPar)
}

# CALCULATE STATISTICS OF THE ENSEMBLE
M.data = matrix(0, nrow = nrun, ncol = tsteps)
Y.data = matrix(0, nrow = nrun, ncol = tsteps)
Z.data = matrix(0, nrow = nrun, ncol = tsteps)
files = list.files(path = directory, pattern= "Mosquito_Run*")
for (i in 1:nrun){
  file = files[i]
  moshist<- read.csv(paste0(directory, "/", file))
  M <- moshist$patch1[moshist$state == "M"]
  Y <- moshist$patch1[moshist$state == "Y"]
  Z <- moshist$patch1[moshist$state == "Z"]
  # Save mos data
  M.data[i,] <- M
  Y.data[i,] <- Y
  Z.data[i,] <- Z
}
M.mean <- sapply(X = 1:tsteps, FUN = function(X){return(mean(M.data[,X]))})
Y.mean <- sapply(X = 1:tsteps, FUN = function(X){return(mean(Y.data[,X]))})
Z.mean <- sapply(X = 1:tsteps, FUN = function(X){return(mean(Z.data[,X]))})

M.std <- sapply(X = 1:tsteps, FUN = function(X){return(sd(M.data[,X]))})
Y.std <- sapply(X = 1:tsteps, FUN = function(X){return(sd(Y.data[,X]))})
Z.std <- sapply(X = 1:tsteps, FUN = function(X){return(sd(Z.data[,X]))})

I.data = matrix(0, nrow = nrun, ncol = tsteps+1)
files = list.files(path = directory, pattern= "HumanPathogen_Run*")
for (j in 1:nrun) {
  p <- read.csv(paste0(directory, "/", files[j]))
  e <- table(data.frame(time = ceiling(p$time), event = p$event))
  h <- matrix(0, nrow = tsteps+1, ncol = 2)
  for (i in 1:nrow(e)){
    tIndex <- as.numeric(rownames(e))[i] + 1
    h[tIndex,1] <- e[i,1] # infecteds
    h[tIndex,2] <- e[i,2] # susceptibles
  }
  I.data[j,] = append(cumsum(h[1,1]), cumsum(h[1,1]) + cumsum(h[-1,1]) - cumsum(h[-1,2]))
}
I.data.mean <- sapply(X = 1:tsteps, FUN = function(X){return(mean(I.data[,X]))})
I.data.std <- sapply(X = 1:tsteps, FUN = function(X){return(sd(I.data[,X]))})

# write output
out <- data.frame(M = M.mean, Y = Y.mean, Z = Z.mean, I = I.data.mean,
                  Ms = M.std, Ys = Y.std, Zs = Z.std, Is = I.data.std)
write.table(out, file = paste0(directory,"/ensemble_average_data.csv"), sep=",")

