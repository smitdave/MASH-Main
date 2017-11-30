rm(list=ls());gc()
library(MASHmacro)

n = 10
aquaPar = AquaPop_Emerge.Parameters(nPatch = n,lambda = rep(50,n),seasonality = TRUE)
patchPar = replicate(n = n,expr = list(bWeightZoo=1,bWeightZootox=0),simplify = FALSE)

tile = MacroTile$new(nPatch = n,AquaPar = aquaPar,PatchPar = patchPar)
