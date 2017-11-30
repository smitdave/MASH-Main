rm(list=ls());gc()
library(MASHmacro)

n = 10
aquaPar = AquaPop_Emerge.Parameters(nPatch = n,lambda = rep(50,n),seasonality = TRUE)
patchPar = replicate(n = n,expr = list(bWeightZoo=1,bWeightZootox=0),simplify = FALSE)
mosquitoPar = list(model="RM", M=rep(50,n),EIP = rep(11,365),p=0.9, f=0.3, Q=0.9, v=20, psi = diag(n))

tile = MacroTile$new(nPatch = n,AquaPar = aquaPar,PatchPar = patchPar,MosquitoPar = mosquitoPar)
tile$simMacro(tMax = 1000)
