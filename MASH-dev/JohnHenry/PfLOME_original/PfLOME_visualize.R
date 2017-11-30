
bumps1 = function(lP, tmin=25, blockLength=9){
  allFacs = rep(1, lP)
  ttFacs = rep(0, lP)
  if(tmin<lP){
    nBlocks = ceiling((lP-tmin)/blockLength)
    facs = rnorm(nBlocks,0,0.2)
    ttFacs = rep(facs[1], blockLength)
    for(i in 2:nBlocks){
      ttFacs = c(ttFacs, rep(facs[i], blockLength))
    }
    ttFacs = c(rep(0,tmin), ttFacs)[1:lP]
  } 
  allFacs - ttFacs
}

PAR = list(dam=1, sire=2, red = TRUE)
thisPf = makeTimeCourse_PfLOME_tent(0,20,0) 


plotPfTent = function(Pf){with(Pf,{
  par(mfrow = c(3,1))
  tt = t0 + 1:tEnd
  plot(tt, Pt-6, type = "l", ylim = c(-6,7), lwd=2,  ylab = expression(log[10](P[t], G[t])) , xlab = "Time")
  Psamples = Pt + rnorm(length(Pt), 0, 0.7)
  segments(t0, 0, t0+tEnd, 0, lty = 2)
  Pix = which(Psamples>6)
  lines(tt, Gt-6, type = "l", ylim = c(0,13), col = "red", lwd = 1.5)
#  lines(tt, G1.t[1:tEnd]-6, type = "l", ylim = c(0,13), col = "blue", lwd = 1.5)
  plot(tt[Pix], Psamples[Pix]-6, pch = 4, type= "l", lwd=2, xlim = range(tt), ylab = "Sampled Densities", xlab = "Time")
  Gsamples = Gt + rnorm(length(Gt), 0, 0.7)
  Gix = which(Gsamples>6)
  lines(tt[Gix]+0.5, Gsamples[Gix]-6, pch = 3, type = "l", col =
"red", lwd=1.5)
  plot(tt, pr.infect(Gt), type = "l",ylab = "Probability of Infection", xlab = "Time") 
})}  

plotPfTent(thisPf)

pr.infect=function(G, G50=5.5, Gs=1){
  1/(1+exp(-Gs*(G-G50)))
}

# Pt = makePt.tent(Pf.gr(), Pf.MZ0(), Pf.PeakD(), Pf.MaxPD(), Pf.Duration())
# plot(1:length(Pt), Pt, type = "l", ylim = c(0,13.5), xlim = c(0,450))
# for(i in 1:20){
#   Pt = makePt.tent(Pf.gr(), Pf.MZ0(), Pf.PeakD(), Pf.MaxPD(), Pf.Duration())
#   lines(1:length(Pt), Pt, col = i+2)
# }
