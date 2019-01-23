################################################################################
#   runfile for original simulations from "Power Laws ..." paper
################################################################################

rm(list=ls());gc()

library(here)

# read in data
tororo <- read.table(here("data/TOROROnew.txt"), header=T)
jinja <- read.table (here("data/JINJAnew.txt"), header=T)
kanungu <- read.table(here("data/KANUNGUnew.txt"), header=T)

# plotting parameters
ccx =0.9
tpc = 16
kpc = 4
jpc = 18

# helper functions

# mean/var for month
getMV = function(month, monthyear, obs){
  ix = which(monthyear == month) 
  c(mn=mean(obs[ix]),vr=var(obs[ix])) 
}

# mean/var for household
getMVid = function(id, hhid, obs){
  ix = which(hhid == id) 
  c(mean(obs[ix]), var(obs[ix])) 
}

# plot power law relations
plotPower = function(mn, vr, clr, mtl=NULL, ppch=19, ccx=0.65, add=FALSE){
  if(add==FALSE) {
    plot(mn, vr, log = "xy", 
         col = clr, pch = ppch, cex=ccx,
         xaxt = "n", yaxt = "n", 
         xlab = paste(expression(m), ", means", sep=""), 
         ylab= paste(expression(V), ", variances", sep = ""),
         xlim = c(.005, max(mn, 12)), 
         ylim = c(.005, 1.05*max(vr)), main = mtl)
    axis(1, 10^c(-2:2), 10^c(-2:2))
    axis(2, 10^c(-2:4), 10^c(-2:4))
  } 
  if(add==TRUE) {
    points(mn,vr,col=clr, pch=ppch, cex=ccx)
  }
}

powerFit = function(mn,vr){
  ix = which(mn>0)
  fitlineT = lm(log(vr[ix])~log(mn[ix]))
  a = coef(fitlineT)[1]
  b = coef(fitlineT)[2]
  c(exp(a), b)
}

plotPowerFit = function(mn, vr, clr, mtl=NULL, add=FALSE){
  ix = which(mn>0)
  mn = mn[ix]; vr=vr[ix]
  xx = 10^seq(min(log10(mn), na.rm=TRUE),max(log10(mn), na.rm=TRUE), by = 0.1)
  ab = powerFit(mn,vr)
  if(add==FALSE){
    plot(xx, ab[1]*xx^ab[2], log= "xy", col = clr, lwd=2, 
         type="l", main = mtl)
    axis(1, 10^c(-2:2), 10^c(-2:2))
    axis(2, 10^c(-2:4), 10^c(-2:4))
  }
  if(add==TRUE){
    lines(xx, ab[1]*xx^ab[2], col = clr, lwd=1)
  }
}

powerMonth = function(DT, counts, clr, mtl=NULL,  ppch=19, ccx=0.65, add=FALSE){
  tix = range(DT$monthyear)
  tix = min(tix):max(tix)
  out = sapply(tix, getMV, monthyear=DT$monthyear, obs=counts)
  
  plotPower(out[1,], out[2,], clr, mtl, ppch, ccx, add)
  plotPowerFit(out[1,], out[2,], clr, mtl, TRUE)
  par = powerFit(out[1,], out[2,])
  list(mn = out[1,], vr=out[2,], par=par)
}


powerHouse = function(DT, counts, clr, mtl, ppch=19, ccx=.65, add=FALSE){
  id = unique(DT$hhid)
  
  out = sapply(id, getMVid, hhid=DT$hhid, obs=counts)
  zeros = which(out[1,] == 0)
  if(length(zeros)>0) out = out[,-zeros]
  nas = which(is.na(out[2,])) 
  if(length(nas)>0) out = out[,-nas]
  
  plotPower(out[1,], out[2,], clr, mtl, ppch, ccx, add)
  plotPowerFit(out[1,], out[2,], clr, mtl, TRUE)
  par = powerFit(out[1,], out[2,])
  list(mn = out[1,], vr=out[2,], par=par)
}

# 
Tab = powerMonth(tororo, tororo$obs, "darkred", "Anopheles, by Month", ppch=tpc, ccx=ccx)
Kab = powerMonth(kanungu, kanungu$obs, "darkblue", NULL, kpc, ccx, add=TRUE)
Jab = powerMonth(jinja, jinja$obs, "darkgreen", NULL, jpc, ccx, add=TRUE)
ALLmn = c(Tab$mn, Kab$mn, Jab$mn)
ALLvr = c(Tab$vr, Kab$vr, Jab$vr)
ALLab = plotPowerFit(ALLmn, ALLvr, "black", NULL, add=TRUE)

TabE = powerMonth(tororo, tororo$EIR, "darkred", "Infectious Mosquito Counts by Month", tpc, ccx)
KabE = powerMonth(kanungu, kanungu$EIR, "darkblue", NULL, kpc, ccx, 1)
JabE = powerMonth(jinja, jinja$EIR, "darkgreen", NULL, jpc, ccx, 1)
ALLmnE = c(TabE$mn, KabE$mn, JabE$mn)
ALLvrE = c(TabE$vr, KabE$vr, JabE$vr)
ALLabE = plotPowerFit(ALLmnE, ALLvrE, "black", NULL, add=TRUE)
  
Tab = powerHouse(tororo, tororo$obs, "darkred", "Counts by Household", tpc, ccx)
Kab = powerHouse(kanungu, kanungu$obs, "darkblue", NULL, kpc, ccx, 1)
Jab = powerHouse(jinja, jinja$obs, "darkgreen", NULL, kpc, ccx, 1)
ALLmn = c(Tab$mn, Kab$mn, Jab$mn)
ALLvr = c(Tab$vr, Kab$vr, Jab$vr)
ALLab = plotPowerFit(ALLmn, ALLvr, "black", NULL, add=TRUE)
  
  Tab = powerHouse(tororo, tororo$EIR, "darkred", "Infectious Counts by Household", tpc, ccx)
Kab = powerHouse(kanungu, kanungu$EIR, "darkblue", NULL, kpc, ccx, 1)
Jab = powerHouse(jinja, jinja$EIR, "darkgreen", NULL, jpc, ccx, 1)
ALLmn = c(Tab$mn, Kab$mn, Jab$mn)
ALLvr = c(Tab$vr, Kab$vr, Jab$vr)
ALLab = plotPowerFit(ALLmn, ALLvr, "black", NULL, add=TRUE)