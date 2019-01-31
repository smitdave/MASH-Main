
par(mfrow = c(2,2))
  Tab = powerHouse(tororo, tororo$obs, "darkred", "a)", tpc, ccx) 
  text (0.2, 10000, "All Anopheles, by Household", cex = 1.2) 
  Kab = powerHouse(kanungu, kanungu$obs, "darkblue", NULL, kpc, ccx,1) 
  Jab = powerHouse(jinja, jinja$obs, "darkgreen", NULL, jpc, ccx, 1)
  ALLmn = c(Tab$mn, Kab$mn, Jab$mn)
  ALLvr = c(Tab$vr, Kab$vr, Jab$vr)
  ALLab = plotPowerFit(ALLmn, ALLvr, "black", NULL, add=TRUE)
  
  Tab = powerHouse(tororo, tororo$EIR, "darkred", "b)", tpc, ccx)
  text (0.1, 50, "Infectious Anopheles, by Household",
cex=1.2) 
  Kab = powerHouse(kanungu, kanungu$EIR, "darkblue", NULL, kpc, ccx, 1)
  Jab = powerHouse(jinja, jinja$EIR, "darkgreen", NULL, jpc, ccx, 1)
  ALLmn = c(Tab$mn, Kab$mn, Jab$mn)
  ALLvr = c(Tab$vr, Kab$vr, Jab$vr)
  ALLab = plotPowerFit(ALLmn, ALLvr, "black", NULL, add=TRUE)


require(vioplot)
plot(c(jinja$w,kanungu$w, tororo$w),c(jinja$w,kanungu$w,
tororo$w), main = "c)", type = "n", xlim = c(0.5, 3.5), ylim = c(0,10), xlab =
"Site", ylab= expression(omega), xaxt = "n")
axis(1, c(1,2,3), c("J", "K", "T"))
text(1.5, 9.5, "Household Biting Propensities", cex=1.2)
vioplot(unique(jinja$w), at=1,border="darkgreen", col = "white", add = TRUE)
vioplot(unique(kanungu$w),at=2,border="darkblue", col = "white", add = TRUE) 
vioplot(unique(tororo$w), at=3,border="darkred", col = "white", add = TRUE)

require(beeswarm)
beeswarm(unique(jinja$w), at = 1, col = 'darkgreen', add = TRUE, cex=0.6, pch =jpc )
beeswarm(unique(kanungu$w), at = 2, col = 'darkblue', add = TRUE, cex=0.60, pch=kpc)
beeswarm(unique(tororo$w), at = 3, col = 'darkred', add = TRUE, cex=0.60, pch=tpc)

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
})

