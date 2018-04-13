rm(list=ls());gc()
library(MASHmacro)

set.seed(0)

rho = 0.0# set the probability of receiving treatment

PfSI.Setup(
  DurationPf = 200, mnChemoprophylaxisPf = 30, # setting duration of infectious period and Prophylaxis period
  FeverPf = rho, TreatPf = 1 # setting fever/treatment parameters
)
SimBitePfSI.Setup()
MACRO.Human.Setup(pathogen = "PfSI",tripFrequency = 1/10, tripDuration = 1)

directory = "/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data"

n = 2

# aquatic ecology parameters
aquaPar = AquaPop_Emerge.Parameters(nPatch = n,lambda = rep(50,n),seasonality = FALSE)

# patch parameters
# make movement follow AR(1) covariance structure, then subtract out the diagonal and renormalize
moveMat = matrix(1, nrow = n, ncol = n) - diag(1,n) #diag(1, n)

patchPar = lapply(X = 1:n,FUN = function(i){
  list(
    bWeightZoo = 0,
    bWeightZootox = 0,
    travelWeight = moveMat[i,]
  )
})

# mosquito parameters
Y = rep(0, n)
Z = rep(0, n)
psi = diag(n)
mosquitoPar = list(model="RM", M=rep(450,n),EIP = rep(11,365),
                   Y=Y, Z=Z,
                   p=0.9, f=0.3, Q=0.9, v=20, psi = psi)

# human parameters
patch_humans = rep(100, n)
n_humans = sum(patch_humans)
patch_id = rep(x = 1:n,patch_humans)
home_id = rep(x = 1:n,patch_humans)
human_ages = unlist(lapply(X = patch_humans,FUN = siteAges_HumanPop))
### ADJUST BITING WEIGHTS HERE ###
human_bWeight = rep(1, n_humans)

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

# PfPR
pfpr = rep(0.1,n)

# make a tile
tile = MacroTile$new(nPatch = n,AquaPar = aquaPar,PatchPar = patchPar,MosquitoPar = mosquitoPar,HumanPar = humanPar,directory = directory)

# tile$get_HumansPointer()$init_PfSI(pfpr)

# run simulations
tile$simMacro(tMax = 1000, PfPAR = pfpr)
