rm(list=ls());gc()
library(MASHmacro)

# set random seed
set.seed(43)

# Set up PfSI module for modeling infection - adds methods to HumanPop/Human classes to define eventQ
PfSI.Setup()
SimBitePfSI.Setup()
MACRO.Human.Setup(pathogen = "PfSI",tripFrequency = 1/28,tripDuration = 14)

# directory = "/Users/dtcitron/Documents/MASH/MASH-Main/MASH-dev/DanielCitron/MashMACRO_testing"
directory <- "/Users/slwu89/Desktop/macro"

# Number of Patches
n = 10
# Aquatic ecology parameters
aquaPar = AquaPop_Emerge.Parameters(nPatch = n,lambda = rep(50,n),seasonality = TRUE)

# patch parameters
patchPar = replicate(n = n,expr = list(bWeightZoo=1,bWeightZootox=0),simplify = FALSE)
# make movement follow AR(1) covariance structure, then subtract out the diagonal and renormalize
rho = 0.75
element = function(i,j){rho^abs(i-j)}
moveMat = outer(1:n,1:n,FUN=function(i,j) element(i,j))
diag(moveMat) = 0
for(i in 1:nrow(moveMat)){
  moveMat[i,] = moveMat[i,]/sum(moveMat[i,])
}
patchPar = lapply(X = 1:n,FUN = function(i){
  list(
    bWeightZoo = 1,
    bWeightZootox = 0,
    travelWeight = moveMat[i,]
  )
})

# mosquito parameters
mosquitoPar = list(model="RM", M=rep(50,n),EIP = rep(11,365),p=0.9, f=0.3, Q=0.9, v=20, psi = diag(n))

# human parameters
patch_humans = rpois(n = n,lambda = 20)
n_humans = sum(patch_humans)
patch_id = rep(x = 1:n,patch_humans)
home_id = rep(x = 1:n,patch_humans)
human_ages = unlist(lapply(X = patch_humans,FUN = siteAges_HumanPop))
human_bWeight = bitingWeight_HumanPop(human_ages)

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
pfpr = rep(0.5,n)

# make a tile
tile = MacroTile$new(nPatch = n,AquaPar = aquaPar,PatchPar = patchPar,MosquitoPar = mosquitoPar,HumanPar = humanPar,directory = directory)

# tile$get_HumansPointer()$init_PfSI(pfpr)

# run simulations
tile$simMacro(tMax = 365,PfPAR = pfpr)
# tile$resetMacro(PatchPar = patchPar,MosquitoPar = mosquitoPar)
# tile$simMacro(tMax = 1000)
# tile$resetMacro(PatchPar = patchPar,MosquitoPar = mosquitoPar)

# plot the output
# pfsihist <- tile$get_HumansPointer()$get_PathogensHistory()
# plot_PfSI(pfsihist)
