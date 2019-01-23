
  ccx =0.9
  tpc = 16
  kpc = 4
  jpc = 18

layout(matrix(c(1,4,1,4,2,4,2,5,3,5,3,5),6,2, byrow=TRUE),c(3,3), c(1,1,1,1,1,1))
par(mar = c(5,4,3,5))
plot(Td, TSd, type = "l",  main = "a)", xlab = "Day", ylab = expression(S[d]), col = "darkred", lwd=2)
lines(Td, TSd, col ="darkred")
lines(Td, Tz*max(TSd)/max(Tz), col = grey(0.5))
axis(4, c(0, 1, 3, 5, 7)*max(TSd)/7, c(0, "1%", "3%", "5%", "7%"))
text(1500, 300, "Tororo", cex=1.2)

plot(Kd, KSd, type = "l", main = "b)", xlab = "Day", ylab = expression(S[d]), lwd=2, col = "darkblue")
lines(Kd, Kz*max(KSd)/max(Kz), col = grey(0.5))
axis(4, c(0, 2, 4, 6)*max(KSd)/6, c(0, "2%", "4%", "6%"))
text(850, 22, "Kanungu", cex =1.2)

plot(Jd, JSd, type = "l", main = "c)", xlab = "Day", ylab = expression(S[d]), lwd=2, col = "darkgreen")
lines(Jd, Jz*max(JSd)/max(Jz), col = grey(0.5))
axis(4, c(0, 1, 2, 3, 4)*max(JSd)/4, c(0, "1%", "2%", "3%", "4%"))
text(850, 6, "Jinja", cex =1.2)


  Tab = powerMonth(tororo, tororo$obs, "darkred", "d)", tpc, ccx)
  text(0.1, 10000, "All Anopheles, by Month", cex=1.2) 
  Kab = powerMonth(kanungu, kanungu$obs, "darkblue", NULL, kpc, ccx, 1)
  Jab = powerMonth(jinja, jinja$obs, "darkgreen", NULL, jpc, ccx, 1)
  ALLmn = c(Tab$mn, Kab$mn, Jab$mn)
  ALLvr = c(Tab$vr, Kab$vr, Jab$vr)
  ALLab = plotPowerFit(ALLmn, ALLvr, "black", NULL, add=TRUE)

  Tab = powerMonth(tororo, tororo$EIR, "darkred", "e)", tpc, ccx)
  text(0.08, 40, "Infectious Anopheles, by Month", cex=1.2) 
  Kab = powerMonth(kanungu, kanungu$EIR, "darkblue", NULL, kpc, ccx, 1)
  Jab = powerMonth(jinja, jinja$EIR, "darkgreen", NULL, jpc, ccx, 1)
  ALLmn = c(Tab$mn, Kab$mn, Jab$mn)
  ALLvr = c(Tab$vr, Kab$vr, Jab$vr)
  ALLab = plotPowerFit(ALLmn, ALLvr, "black", NULL, add=TRUE)



