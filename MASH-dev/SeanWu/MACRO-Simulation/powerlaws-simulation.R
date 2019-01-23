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


################################################################################
#   Power Laws for Mosquito Count Data
################################################################################

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


################################################################################
#   Epidemiological Power Laws
################################################################################

# loads a data frame 'data.plot
load(here("data/data_plot_dave.Rdata"))

getEff = function(loc){with(data.plot,{
  ix = which(site==loc)
  eir = 10^log10_eir[ix]
  foi = yearly_rate[ix]
  mean(eir/foi)
})} 

with(data.plot,{
  eir = 10^log10_eir
  aeff = eir/yearly_rate
  plot(eir, aeff, type = "n", xlab = "Annual EIR", ylab =
         "Inefficiency (aEIR : aFOI)", main = "d)", xaxt = "n", yaxt = "n", log = "xy")
  axis(1, 10^c(0, 1, 2, 3), c(1,10,100, 1000)) 
  axis(2, c(1/2, 2, 10, 50), c("1:2","2:1","10:1","50:1")) 
  
  cx = .8
  pc = 20
  
  text(10,70,"Exposure vs. Infection", cex = 1.2)  
  
  ix = which(site=="Kanungu")
  points(eir[ix], aeff[ix], col = "darkblue", pch=kpc)
  
  ix = which(site=="Tororo")
  points(eir[ix], aeff[ix], col = "darkred", pch=tpc)
  
  ix = which(site=="Jinja")
  points(eir[ix], aeff[ix], col = "darkgreen", pch=jpc)
  
  llm=lm(log(aeff)~log(eir))
  xx = exp(seq(0, 7, length.out=20))
  a = exp(coef(llm)[1]) 
  b = coef(llm)[2]
  lines(xx, a*xx^b)
  print(c(a,b))
})


################################################################################
#   Decomposing the Heterogeneity
################################################################################

Jd = sort(unique(jinja$day)) 
Kd = sort(unique(kanungu$day))
Td = sort(unique(tororo$day))

getSd = function(d, dt){with(dt,{
  Sd[min(which(day == d))]   
})}

JSd = sapply(Jd, getSd,dt =jinja)
KSd = sapply(Kd, getSd,dt =kanungu)
TSd = sapply(Td, getSd,dt =tororo)

getSR = function(dd,dt,wd=.6){with(dt,{
  wts = exp(-wd*(dd-day)^2)
  z=sum(wts*npos)/sum(wts*ntested)
  ifelse(is.na(z), 0, z)
})}

Jz = sapply(Jd, getSR,dt =jinja, wd=.001)
Kz = sapply(Kd, getSR,dt =kanungu, wd=.001)
Tz = sapply(Td, getSR,dt =tororo, wd=.001)
TTz = sum(tororo$npos,na.rm=T)/sum(tororo$ntested, na.rm=T) 
KKz = sum(kanungu$npos, na.rm=T)/sum(kanungu$ntested, na.rm=T) 
JJz = sum(jinja$npos, na.rm=T)/sum(jinja$ntested, na.rm=T) 

plot(Td, Tz, type = "l", ylim = range(Jz, Kz, Tz), col = "darkred")
lines(Kd, Kz, type = "l", col = "darkblue")
lines(Jd, Jz, type = "l", col = "darkgreen")

plot(Jd, JSd, type = "l", main = "Jinja", xlab = "Day", ylab = "Counts", lwd=3, col = "darkgreen")
lines(Jd, Jz*max(JSd)/max(Jz), col = grey(0.5))
axis(4, seq(0, 1, length.out=4)*max(JSd), c(0, "1%", "2%", "3%"))
  
plot(Jd, JSd, type = "l", main = "Jinja", xlab = "Day", ylab = "Counts", ylim = range(jinja$obs))
points(jinja$day, jinja$obs, pch = 15, col = grey(0.5), cex = 0.2)
lines(Jd, JSd, lwd=2, col = "darkgreen")

plot(Kd, KSd, type = "l", main = "Kanungu", xlab = "Day", ylab = "Counts", lwd=2, col = "darkblue")
lines(Kd, Kz*max(KSd)/max(Kz), col = grey(0.5))
axis(4, seq(0, 1, length.out=6)*max(KSd), c(0, "1%", "2%", "3%", "4%","5%"))

plot(Kd, KSd, type = "l", main = "Kanungu", xlab = "Day", ylab = "Counts", ylim = range(kanungu$obs))
points(kanungu$day, kanungu$obs, pch = 15, col = grey(0.5), cex = 0.2)
lines(Kd, KSd, lwd=2, col = "darkblue")

plot(Td, TSd, type = "l", main = "Tororo", xlab = "Day", ylab = "Counts", col = "darkred", lwd=2)
lines(Td, Tz*max(TSd)/max(Tz), col = grey(0.5), lwd=2)
axis(4, seq(0, 1, length.out=6)*max(TSd), c(0, "1%", "2%", "3%", "4%","5%"))

plot(Td, TSd, type = "l", main = "Tororo", xlab = "Day", ylab = "Counts", ylim = range(tororo$obs))
points(tororo$day, tororo$obs, pch = 15, col = grey(0.5), cex = 0.2)
lines(Td, TSd, lwd=2, col = "darkred")


################################################################################
#   Household Biting Weights
################################################################################

require(vioplot)
plot(c(jinja$w,kanungu$w, tororo$w),c(jinja$w,kanungu$w, tororo$w), type = "n", xlim = c(0.5, 3.5), xlab = "Site", ylab= "Biting Weights", xaxt = "n", main = "Household Biting Propensity Distributions")
axis(1, c(1,2,3), c("J", "K", "T"))

vioplot(unique(jinja$w), at=1,border="darkgreen", col = "white", add = TRUE)
vioplot(unique(kanungu$w),at=2,border="darkblue", col = "white", add = TRUE) 
vioplot(unique(tororo$w), at=3,border="darkred", col = "white", add = TRUE)

require(beeswarm)
beeswarm(unique(jinja$w), at = 1, col = 'darkgreen', add = TRUE, cex=0.8, pch = jpc )
beeswarm(unique(kanungu$w), at = 2, col = 'darkblue', add = TRUE, cex=0.8, pch = kpc)
beeswarm(unique(tororo$w), at = 3, col = 'darkred', add = TRUE, cex=0.8, pch = tpc)


################################################################################
#   Residual Error Ratios
################################################################################

smoothZeros = function(x, dt, wd=5){with(dt,{
  ix = which(abs(log10(x)-log10(Sd*w))< log10(wd))
  sum(E[ix]==0)/length(ix)
})}

plotResidErr = function(dta, mtl){with(dta,{
  plot(Sd*w, E, pch = 15, cex=0.5, log="xy", 
       xlab = expression(S[d] %.% omega[h]), 
       ylab = expression(M[d,h] / (S[d]%.%omega[h])), ylim = c(10^-3.2, 50),
       yaxt = "n", xaxt = "n", main = mtl, xlim = c(0.001,600))
  #text(0.05, .1, mtl, cex=1.5)
  text(.05, 10^-2, "zeros (jittered)", cex=1, pos=3)
  axis(1, c(0.01, .1, 1, 10, 100, 600), c("0.01", "0.1", "1", "10", "100", "600"))
  
  axis(2, c(.001,.01, .1, 1, 10, 50), c("0%", "100%", "0.1", "1", "10", "50"))
  segments(0.1, 10, 100, .01, col = "red")
  text(1, .001, "Proportion Zero (smoothed)", pos=1, col = "blue")
  
  segments(0.001, 10^-2, 650,10^-2, col = grey(0.5), lty = 2)
  segments(0.001, 10^-3, 650,10^-3, col = grey(0.5), lty = 2)
  ix = which(E == 0)
  xx = Sd[ix]*w[ix]
  points(xx, jitter(10^-2.28+0*xx, factor =40), pch = 15, cex = 0.5)
  xx = 10^seq(-3, 3, length.out = 100)
  SZ = sapply(xx, smoothZeros,dt=dta, wd=2)
  lines(xx, 10^(-3+SZ), col = "yellow", lwd=4)
  lines(xx, 10^(-3+SZ), col = "blue", lwd=2)
  ix = which(E>0)
})}

par(mfrow=c(3,1), mar =c(5,4,1,2))
plotResidErr(tororo, "Tororo")
plotResidErr(kanungu, "Kanungu")
plotResidErr(jinja, "Jinja")


################################################################################
#   Fitting, Using Likelihood
################################################################################

set.seed(22)
mu = 10; k=.2; N=10000
s1 = rpois(N,rgamma(N,shape=k, scale=mu/k))
s2 = rnbinom(N,mu=mu,size=k)
hist(s1, 40)
hist(s2, 40, add = TRUE, border = "red")

x=k/mu
s3 = rnbinom(N, size=mu*x, prob= x/(1+x))
s4 = rpois(N, rgamma(N, shape=mu*x, scale=1/x)) 
hist(s3, 40)
hist(s4, 40, add = TRUE, border = "blue")
hist(s2, 40, add = TRUE, border = "red")


################################################################################
#   The Simplest Model
################################################################################

fitE.xi = function(DTA){with(DTA,{
  dlik = function(xi){
    a=log(dnbinom(obs,size=Sd*w*xi,prob=xi/(1+xi)))
    -sum(a)
  }
  optimize(f=dlik, interval = c(0,1))$minimum
})}


J.xi = fitE.xi(jinja)
K.xi = fitE.xi(kanungu)
T.xi = fitE.xi(tororo)
all.xi = c(j=J.xi, k=K.xi, t=T.xi)
all.xi

fitE.k = function(DTA){with(DTA,{
  dlik = function(k){
    a=log(dnbinom(obs,mu=Sd*w,size=k))
    -sum(a)
  }
  optimize(f=dlik, interval = c(0,1))$minimum
})}

J.k = fitE.k(jinja)
K.k = fitE.k(kanungu)
T.k = fitE.k(tororo)
all.k = c(J=J.k, K=K.k, T=T.k)
all.k


################################################################################
#   Quantiles Analysis
################################################################################

# MLE fits by xi

fitE.gQ = function(DTA, qq){with(DTA,{
  bks = quantile(Sd*w, qq)
  dlik = function(x, ix){
    a=log(dnbinom(obs[ix],size=Sd[ix]*w[ix]*x,prob=x/(1+x)))
    -sum(a)
  }
  x = c(0,0)
  for(i in 2:length(bks)){
    ix = which(Sd*w > bks[i-1] & Sd*w < bks[i])
    ans = optimize(f=dlik, interval = c(0,20),ix=ix)
    x = rbind(x, c(ans$minimum,ans$objective)) 
  }
  mids = (bks[-1] + bks[-length(qq)])/2
  # col1 = midpoints of the quantiles
  # col2 = MLE estimate for that quantile
  # col3 = the likelihood
  cbind(mids, x[-1,]) 
})}

gg.mu = function(mu,P){
  exp(P[1])*exp(log(mu)*P[2])
}

fitit.gQ = function(N, data, plotit=TRUE, pointsit=FALSE){
  xx = fitE.gQ(data, c(0:N)/N)
  ix = which(xx[,1]<.1)
  xx = xx[-ix,]
  ix = which(xx[,2]<19)
  mu = xx[ix,1]
  x = xx[ix,2]
  llm=lm(log(x)~log(mu))
  #gg = exp(coef(llm)[1])*exp(log(mu)*coef(llm)[2])
  gg=gg.mu(mu, coef(llm))
  if(plotit){ 
    plot(mu, mu*x, log = "xy", xlim = c(0.1, 500), ylim = c(.1,10))
    lines(mu, mu*gg)
  } 
  if(pointsit){
    points(mu, mu*x, pch=3, col = "blue")
    lines(mu, mu*gg, col = "blue")
  }
  return(coef(llm))
}

par(mfrow=c(3,1))
J.g = fitit.gQ(61, jinja, 1, 0)
fitit.gQ(51, jinja, 0, 1)
text(10,5, "Jinja")
K.g = fitit.gQ(60, kanungu, 1,0)
fitit.gQ(30, kanungu, 0,1)
text(40,.3, "Kanungu")
T.g = fitit.gQ(61, tororo,1,0)
fitit.gQ(41, tororo,0,1)
text(40,.3, "Kanungu")

# the other way

fitE.fQ = function(DTA, qq){with(DTA,{
  bks = quantile(Sd*w, qq)
  dlik = function(k, ix){
    a=log(dnbinom(obs[ix],mu=Sd[ix]*w[ix],size=k))
    -sum(a)
  }
  x = c(0,0)
  for(i in 2:length(bks)){
    ix = which(Sd*w > bks[i-1] & Sd*w < bks[i])
    ans = optimize(f=dlik, interval = c(0,20),ix=ix)
    x = rbind(x, c(ans$minimum,ans$objective)) 
  }
  mids = (bks[-1] + bks[-length(qq)])/2
  # col1 = midpoints of the quantiles
  # col2 = MLE estimate for that quantile
  # col3 = the likelihood
  cbind(mids, x[-1,]) 
})}

ff.mu = function(mu, P){
  P[1]*exp(log(mu)*P[2]) 
}

fitit.fQ = function(N, data, plotit=TRUE, pointsit=FALSE){
  xx = fitE.fQ(data, c(0:N)/N)
  ix = which(xx[,1]<.1)
  xx = xx[-ix,]
  ix = which(xx[,2]<19)
  mu = xx[ix,1]
  k = xx[ix,2]
  llm=lm(k~log(mu))
  ff=ff.mu(mu, coef(llm))
  if(plotit){ 
    plot(mu, k, log="x", xlim = c(0.1, 500), ylim = c(0,4))
    lines(mu, ff)
  } 
  if(pointsit){
    points(mu, k, pch=3, col = "blue")
    lines(mu, ff, col = "blue")
  }
  return(coef(llm))
}

par(mfrow = c(3,1))
J.f = fitit.fQ(61, jinja, 1, 0)
fitit.fQ(51, jinja, 0, 1)
text(10, 3, "Jinja")
K.f = fitit.fQ(60, kanungu, 1,0)
fitit.fQ(30, kanungu, 0,1)
text(50, 3, "Kanungu")
T.f = fitit.fQ(61, tororo,1,0)
fitit.fQ(41, tororo,0,1)
text(1, 3, "Tororo")

# All of the fits plotted together

par(mfrow=c(1,1))
xx = 10^seq(-.8, 2.8, length.out=100)
ff.T = function(mu){ff.mu(mu,T.f)}
plot(xx,ff.T(xx), log ="x", type = "l", col = "darkred", ylim = c(0,3.5), xlab = expression(mu), ylab= expression(k(mu)), main = "Fitted relationships")
gg.T = function(mu){gg.mu(mu,T.g)}
lines(xx,xx*gg.T(xx), col = "darkred", lty=2)

xx = 10^seq(-.8, 2.3, length.out=100)
ff.K = function(mu){ff.mu(mu,K.f)}
lines(xx,ff.K(xx), col = "darkblue")
gg.K = function(mu){gg.mu(mu,K.g)}
lines(xx,xx*gg.K(xx), col = "darkblue",lty=2)

xx = 10^seq(-.8, 1.7, length.out=100)
ff.J = function(mu){ff.mu(mu,J.f)}
lines(xx,ff.J(xx), col = "darkgreen")
gg.J = function(mu){gg.mu(mu,J.g)}
lines(xx,xx*gg.J(xx), col = "darkgreen",lty=2)
  
# And we can visualize the noise. It looks like this:
  
plotEsim = function(DT, ttl, tX.f, clr, ppc, plotit=FALSE){with(DT,{
  N = length(Sd)
  cx = .2
  envVar = rgamma(N, shape=Sd*w*tX.f(Sd*w), scale=1/tX.f(Sd*w))/Sd/w
  if(plotit == TRUE){
    plot(Sd*w, envVar, log = "xy", xaxt = "n", col = clr, xlab =
           expression(S[d]%.%omega[h]), yaxt = "n", pch = 15, cex=cx, ylab="E", main = ttl, ylim = c(10^-4, 50), xlim = c(0.005,600))
    
    axis(1, c(.01, 1/10, 1, 10, 100), c(1/100, 1/10, 1, 10, 100))
    axis(2, c(1/1000, 1/100, 1/10, 1, 10), c(1/1000, 1/100, 1/10, 1, 10))
  }
  if(plotit == FALSE){
    points(Sd*w, envVar, col = clr, pch=ppc, cex=cx)
  }
  
  envVar = rgamma(N, shape=10^-2*tX.f(10^-2), scale=1/tX.f(10^-2))/10^-2
  vioplot(envVar, at = -2, border = clr, col = "white", add=TRUE)
})}
  
par(mfrow = c(3,1))
plotEsim(tororo, "Tororo", gg.T, "darkred", tpc, TRUE) 
plotEsim(kanungu, "Kanungu", gg.K, "darkblue", kpc,  TRUE)
plotEsim(jinja, "Jinja", gg.J, "darkgreen", jpc, TRUE)
par(mfrow=c(1,1))


################################################################################
#   Simulation: Simulated Counts
################################################################################

simData.gg = function(DT, gg, b=1, Z=1, Xd=NULL, XSd=NULL, clr=NULL, plotit=FALSE){
  N = length(DT$Sd)
  mu = DT$Sd*DT$w
  xi = gg(mu)
  simCounts=rnbinom(N,size=mu*xi*b*Z,prob=xi/(1+xi))
  if(plotit){ 
    plot(Xd, XSd, col = clr, type = "l", ylim = range(simCounts))
    points(DT$day, simCounts, pch =19, col = grey(0.5), cex=.3)
    lines(Xd, XSd, col=clr, lwd=2)
  }   
  simCounts
}

simData.ff = function(DT,ff, b=1, Z=1, Xd=NULL, XSd=NULL, clr=NULL, plotit=FALSE){
  N = length(DT$Sd)
  mu = DT$Sd*DT$w
  k = ff(mu)
  simCounts = rnbinom(N, mu=mu*b*Z, size=k)
  if(plotit){ 
    plot(Xd, XSd, col = clr, type = "l", ylim = range(simCounts, na.rm=TRUE))
    points(DT$day, simCounts, pch =19, col = grey(0.5), cex=.3)
    lines(Xd, XSd, col=clr, lwd=2)
  }   
  simCounts
}


################################################################################
#   Simulation: Total Anopheles, by Month
################################################################################

TM = powerMonth(tororo, tororo$obs, "darkred", "Tororo Counts", tpc, ccx)
TSimCounts0 = simData.ff(tororo, ff.T, 1, 1, Td, TSd, "darkred", FALSE)
TM0 = powerMonth(tororo, TSimCounts0, "orange", NULL, tpc, ccx,1)
TSimCounts1 = simData.gg(tororo,gg.T, 1, 1, Td, TSd, "darkred", FALSE)
TM1 =powerMonth(tororo, TSimCounts1, "cyan", NULL, tpc, ccx,1)

KM = powerMonth(kanungu, kanungu$obs, "darkblue", "Kanungu Counts", kpc, ccx)
KSimCounts0 = simData.ff(kanungu,ff.K, 1, 1, Kd, KSd, "darkblue", FALSE)
KM0 = powerMonth(kanungu, KSimCounts0, "orange", NULL, kpc, ccx, 1)
KSimCounts1 = simData.gg(kanungu,gg.K, 1, 1, Kd, KSd, "darkblue", FALSE)
KM1 = powerMonth(kanungu, KSimCounts1, "cyan", NULL, kpc, ccx, 1)

JM = powerHouse(jinja, jinja$obs, "darkgreen", mtl = "Jinja Counts", jpc, ccx)
JSimCounts0 = simData.ff(jinja, ff.J, 1,1,Jd, JSd, "darkgreen", FALSE)
JM0 = powerHouse(jinja, JSimCounts0, "orange", NULL, jpc, ccx, 1)
JSimCounts1 = simData.gg(jinja,gg.J, 1,1,Jd, JSd, "darkgreen", FALSE)
JM1 = powerHouse(jinja, JSimCounts1, "cyan", NULL, jpc, ccx, 1)


################################################################################
#   Simulation: Total Anopheles, by Household
################################################################################

TH = powerHouse(tororo, tororo$obs, "darkred", "Tororo Counts", tpc, ccx)
TH0 = powerHouse(tororo, TSimCounts0, "orange", NULL, tpc, ccx,1)
TH1 =powerHouse(tororo, TSimCounts1, "cyan", NULL, tpc, ccx,1)

KH = powerHouse(kanungu, kanungu$obs, "darkblue", "Kanungu Counts", kpc, ccx)
KH0 = powerHouse(kanungu, KSimCounts0, "orange", NULL, kpc, ccx, 1)
KH1 = powerHouse(kanungu, KSimCounts1, "cyan", NULL, kpc, ccx, 1)
  
JH = powerHouse(jinja, jinja$obs, "darkgreen", mtl = "Jinja Counts", jpc, ccx)
JH0 = powerHouse(jinja, JSimCounts0, "orange", NULL, jpc, ccx, 1)
JH1 = powerHouse(jinja, JSimCounts1, "cyan", NULL, jpc, ccx, 1)


################################################################################
#   Simulation: Infectious Anopheles, by Month
################################################################################

TZ = powerMonth(tororo, tororo$sEIR, "darkred", "Tororo Infectious", tpc, ccx)
TSimCounts0 = simData.ff(tororo, ff.T, 1,TTz,  Td, TSd, "darkred", FALSE)
TZ0 = powerMonth(tororo, TSimCounts0, "orange", NULL, tpc, ccx,1)
TSimCounts1 = simData.gg(tororo,gg.T, 1,TTz,  Td, TSd, "darkred", FALSE)
TZ1 =powerMonth(tororo, TSimCounts1, "cyan", NULL, tpc, ccx,1)
  
KZ = powerMonth(kanungu, kanungu$sEIR, "darkblue", "Kanungu Infectious", kpc, ccx)
KSimCounts0 = simData.ff(kanungu,ff.K, 1, KKz,  Kd, KSd, "darkblue", FALSE)
KZ0 = powerMonth(kanungu, KSimCounts0, "orange", NULL, kpc, ccx, 1)
KSimCounts1 = simData.gg(kanungu,gg.K, 1, KKz,  Kd, KSd, "darkblue", FALSE)
KZ1 = powerMonth(kanungu, KSimCounts1, "cyan", NULL, kpc, ccx, 1)


JZ = powerHouse(jinja, jinja$sEIR, "darkgreen", mtl = "Jinja Infectious", jpc, ccx)
JSimCounts0 = simData.ff(jinja, ff.J, 1,JJz,Jd, JSd, "darkgreen", FALSE)
JZ0 = powerHouse(jinja, JSimCounts0, "orange", NULL, jpc, ccx, 1)
JSimCounts1 = simData.gg(jinja,gg.J, 1,JJz,Jd, JSd, "darkgreen", FALSE)
JZ1 = powerHouse(jinja, JSimCounts1, "cyan", NULL, jpc, ccx, 1)


################################################################################
#   Simulation: Infectious Anopheles, by Household
################################################################################

THz = powerHouse(tororo, tororo$sEIR, "darkred", "Tororo Counts", tpc, ccx)
THz0 = powerHouse(tororo, TSimCounts0, "orange", NULL, tpc, ccx,1)
THz1 =powerHouse(tororo, TSimCounts1, "cyan", NULL, tpc, ccx,1)
  
KHz = powerHouse(kanungu, kanungu$sEIR, "darkblue", "Kanungu Counts", kpc, ccx)
KHz0 = powerHouse(kanungu, KSimCounts0, "orange", NULL, kpc, ccx, 1)
KHz1 = powerHouse(kanungu, KSimCounts1, "cyan", NULL, kpc, ccx, 1)
  
JHz = powerHouse(jinja, jinja$sEIR, "darkgreen", mtl = "Jinja Counts", jpc, ccx)
JHz0 = powerHouse(jinja, JSimCounts0, "orange", NULL, jpc, ccx, 1)
JHz1 = powerHouse(jinja, JSimCounts1, "cyan", NULL, jpc, ccx, 1)


################################################################################
#   Simulation: Exposure & Infection
################################################################################

JGaps = c(min(Jd):max(Jd))[-Jd]
KGaps = c(min(Kd):max(Kd))[-Kd]
TGaps = c(min(Kd):max(Td))[-Td]

interp=function(y1,yn,n){
  y1 + (1:n)*(yn-y1)/(n+1)
}

fillGaps = function(dd, Sd){
  ix0 = which(diff(dd)>1)
  gap = diff(dd)[ix0]
  
  ddnew = min(dd):max(dd)
  Sdnew = 0*ddnew
  Sdnew[dd]=Sd
  
  for(i in 1:length(ix0)){
    #if(i==172 & ix0[i] ==574) browser() 
    d0 = dd[ix0[i]]
    dn = dd[ix0[i]+1]
    y0 = Sd[ix0[i]]
    yn = Sd[ix0[i]+1]
    nn = gap[i]
    vals = interp(y0,yn,nn)
    
    Sdnew[d0+1:nn] = interp(y0,yn,nn)
  }
  list(dd=ddnew,Sd=Sdnew)
}

newJ = fillGaps(Jd,JSd)
JJSd = newJ$Sd
JJd= newJ$dd

newK = fillGaps(Kd,KSd)
KKSd = newK$Sd
KKd = newK$dd

newT = fillGaps(Td,TSd)
TTSd = newT$Sd[-1][1:1260]
TTd = newT$dd[1:1260]

foi = function(dd, Sd, f.w, f.e, SR, b=0.55){
  ld = length(dd)
  w = f.w()
  e = replicate(ld,f.e())
  hist(e)
  hist(Sd*e*w)
  bites = rpois(ld,Sd*e*w)
  rbinom(ld,bites,SR*b)
}

nAttacks = function(d,foi){
  nDays = dim(foi)[1]
  nHumans = dim(foi)[2]
  N = floor(nDays/d)
  attacks = colSums(foi[1:d,])
  for(i in 2:N){
    ix = 1:d + (i-1)*d
    attacks= rbind(attacks, colSums(foi[ix, ]))
  } 
  attacks
}

heratio = function(foi){
  foi.real = rowSums(foi)
  nz = function(i){
    sum(foi[i,]>0)
  }
  ar = sapply(1:length(foi.real),nz)
  list(foi=foi.real,attacks=ar)  
}

ibi2ar = function(i,ibi){
  sum(ibi[i,]==0) 
}

oneHouse.gg = function(Sd, Qw, gg, SR=0.0062, b=0.55){
  w=if(Qw==0){1}else{rgamma(1,Qw,Qw)}
  xi = gg(Sd*w)
  rnbinom(1260, size=Sd*w*xi*SR*b, prob=xi/(1+xi))
}

oneHouse14.gg = function(Sd, Qw, gg, SR=0.0062, b=0.55){
  colSums(matrix(oneHouse.gg(Sd,Qw,gg,SR,b), 14,90))
} 

simAttacks.gg = function(Sd,Qw,gg,SR=0.0062,b=0.55,N=10000){
  ibi = replicate(N, oneHouse14.gg(Sd,Qw,gg,SR,b))
  foi = rowSums(ibi)/N/14
  ar = 1-sapply(1:90, ibi2ar, ibi=ibi)/N
  foi.est = -log(1-ar)/14
  eir = colSums(matrix(Sd*SR,14,90))/14
  list(foi=foi,ar=ar, foi.est=foi.est, eir=eir, eff=eir/foi.est, aeff = foi/foi.est)
}

oneHouse.ff = function(Sd, Qw, ff, SR=0.0062, b=0.55, fac=1){
  w=if(Qw==0){1}else{rgamma(1,Qw,Qw)}
  k = ff(Sd*w)
  rnbinom(1260, mu=Sd*w*fac*b*SR,size=k)
}

oneHouse14.ff = function(Sd, Qw, ff, SR=0.0062, b=0.55, fac=1){
  colSums(matrix(oneHouse.ff(Sd,Qw,ff,SR,b,fac), 14,90))
} 

simAttacks.ff = function(Sd,Qw,ff,SR=0.0062,b=0.55,fac=1,N=10000){
  ibi = replicate(N, oneHouse14.ff(Sd,Qw,ff,SR,b,fac))
  foi = rowSums(ibi)/N/14
  ar = 1-sapply(1:90, ibi2ar, ibi=ibi)/N
  foi.est = -log(1-ar)/14
  eir = colSums(matrix(Sd*SR,14,90))/14
  list(foi=foi,ar=ar, foi.est=foi.est, eir=eir, eff=eir/foi.est, aeff = foi/foi.est)
}


Tw = 2.2
Kw = 0.68
Jw = 0.82

plotSimXI.gg = function(ggT, ggK, ggJ, plotit=TRUE){ 
  T0 = simAttacks.gg(TTSd[1:1260],Tw,ggT, SR=TTz)
  K0 = simAttacks.gg(KKSd[1:1260],Kw,ggK, SR=KKz)
  J0 = simAttacks.gg(JJSd[1:1260],Jw,ggJ, SR=JJz)
  if(plotit == TRUE)  plotSimXI(T0, K0, J0) 
}   

plotSimXI.ff = function(ffT, ffK, ffJ, plotit=TRUE){ 
  T0 = simAttacks.ff(TTSd[1:1260],Tw,ffT, SR=TTz)
  K0 = simAttacks.ff(KKSd[1:1260],Kw,ffK, SR=KKz)
  J0 = simAttacks.ff(JJSd[1:1260],Jw,ffJ, SR=JJz)
  if(plotit == TRUE)  plotSimXI(T0, K0, J0) 
}   

plotSimXI = function(T0, K0, J0){ 
  wks = 1:90 
  par(mfrow = c(2,2), mar = c(5,4,2,1)) 
  
  plot (wks, T0$foi, type = "l", col = "darkred", lwd=2,  xaxt = "n", xlab = "Time (Years)", ylab = "daily FOI (simulated)")
  lines(wks, T0$foi.est, col = "darkred", lwd=2, lty = 2)
  axis(1, c(1,366,731,1096)/14, c(0,1,2,3))
  mtext("a) Tororo",line=1,at=0)
  #axis(4, c(0,5,10), c(0, 0.5, 1))
  
  plot (wks, K0$foi, type = "l", col = "darkblue", lwd=2,  xaxt = "n", xlab = "Time (Years)", ylab = "daily FOI (simulated)")
  lines(wks, K0$foi.est, col = "darkblue", lwd=2, lty = 2)
  axis(1, c(1,366,731,1096)/14, c(0,1,2,3))
  mtext("b) Kanungu",line=1,at=0)
  
  
  plot (wks, J0$foi, type = "l", col = "darkgreen", lwd=2,  xaxt = "n", xlab = "Time (Years)", ylab = "daily FOI (simulated)")
  lines(wks, J0$foi.est, col = "darkgreen", lwd=2, lty = 2)
  #lines(wks, J0$foi*14, col = "darkgreen", lwd=2)
  #lines(wks, K0$foi*14, col = "darkblue", lwd=2)
  axis(1, c(1,366,731,1096)/14, c(0,1,2,3))
  mtext("c) Jinja",line=1,at=0)
  
  plot (wks, T0$aeff, type = "l", lwd=2, xlab = "Time (Years)", yaxt = "n", ylab = "Transmission Efficiency", xaxt = "n", col = "darkred", ylim = range(1,30), log="y")
  lines(wks, J0$aeff, lwd=2, col = "darkblue")
  lines(wks, K0$aeff, lwd=2, col = "darkgreen")
  mtext("d)",line=1,at=0)
  
  axis(2, c(2.2, 4.4, 9.5), c("1.7:1","2.7:1","7.4:1"))
  
  axis(1, c(1,366,731,1096)/14, c(0,1,2,3))
} 

plotSimXI.gg(gg.T, gg.K, gg.J) 
  
plotSimXI.ff(ff.T, ff.K, ff.J) 


################################################################################
#   Sensitivity Analysis
################################################################################

ggT=gg.T
#ggT = gg.J
#ggT = gg.K
T0 = simAttacks.gg(TTSd[1:1260],Tw,ggT, SR=TTz)
T0a = simAttacks.gg(TTSd[1:1260]*0+mean(TTSd[1:1260]),Tw,ggT, SR=TTz)
T0b = simAttacks.gg(TTSd[1:1260]*0+mean(TTSd[1:1260]),0,ggT, SR=TTz)
T0c = simAttacks.gg(TTSd[1:1260],0,ggT, SR=TTz)

ggK = gg.K
#ggK = gg.T
#ggK = gg.J
K0 = simAttacks.gg(KKSd[1:1260],Kw,ggK, SR=KKz)
K0a = simAttacks.gg(KKSd[1:1260]*0+mean(KKSd[1:1260]),Kw,ggK, SR=KKz)
K0b = simAttacks.gg(KKSd[1:1260]*0+mean(KKSd[1:1260]),0,ggK, SR=KKz)
K0c = simAttacks.gg(KKSd[1:1260],0,ggK, SR=KKz)

ggJ = gg.J
#ggJ = gg.K
#ggJ = gg.T
J0 = simAttacks.gg(JJSd[1:1260],Jw,ggJ, SR=JJz)
J0a = simAttacks.gg(JJSd[1:1260]*0+mean(JJSd[1:1260]),Jw,ggJ, SR=JJz)
J0b = simAttacks.gg(JJSd[1:1260]*0+mean(JJSd[1:1260]),0,ggJ, SR=JJz)
J0c = simAttacks.gg(JJSd[1:1260],Jw,ggJ, SR=JJz)

ratios = function(X0,X0a,X0b,X0c){
  base=mean(X0$aeff)
  a=mean(X0a$aeff)
  b=mean(X0b$aeff)
  c=mean(X0c$aeff)
  c(ES = mean(X0$aeff), a=a/base, b=base, c=c/base)
}

ratios(T0, T0a, T0b, T0c)
ratios(K0, K0a, K0b, K0c)
ratios(J0, J0a, J0b, J0c)