rm(list=ls());gc()
library(MASHmacro)

directory = "/Users/slwu89/Desktop/MACRO"
n = 10
aquaPar = AquaPop_Emerge.Parameters(nPatch = n,lambda = rep(50,n),seasonality = TRUE)
patchPar = replicate(n = n,expr = list(bWeightZoo=1,bWeightZootox=0),simplify = FALSE)
mosquitoPar = list(model="RM", M=rep(50,n),EIP = rep(11,365),p=0.9, f=0.3, Q=0.9, v=20, psi = diag(n))

tile = MacroTile$new(nPatch = n,AquaPar = aquaPar,PatchPar = patchPar,MosquitoPar = mosquitoPar,directory = directory)

tile$simMacro(tMax = 1000)
tile$resetMacro(PatchPar = patchPar,MosquitoPar = mosquitoPar)
tile$simMacro(tMax = 1000)
tile$resetMacro(PatchPar = patchPar,MosquitoPar = mosquitoPar)


patch_humans = rpois(n = n,lambda = 100)
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

