

now = 0
newInfections = rep(0,5)
oldInfections = rep(0,6)

updateOld6 = function(S){
  rmultinom(1, S[1], P6t[,1]) + 
  rmultinom(1, S[2], P6t[,2]) + 
  rmultinom(1, S[3], P6t[,3]) + 
  rmultinom(1, S[4], P6t[,4]) + 
  rmultinom(1, S[5], P6t[,5]) + 
  rmultinom(1, S[6], P6t[,6])
}

qq = 4/5
pp = 1/10
Pt = t(matrix(
  c(
    c(qq,   pp,           0,          0, 0),
    c(1-qq, (1-pp)*qq,    pp,         0, 0), 
    c(0,    (1-pp)*(1-qq), (1-pp)*qq, pp, 0),
    c(0,    0,  (1-pp)*(1-qq), (1-pp)*qq,0),
    c(0,    0,  0, (1-pp)*(1-qq), 1)
  )
,5,5))

P6t = Pt[1:4, 1:3]
P6t = rbind(P6t, 0)
P6t = rbind(P6t, 0)
P6t = rbind(P6t, 0)
P6t = cbind(P6t, c(0,P6t[-7,3]))
P6t = cbind(P6t, c(0,P6t[-7,4]))
P6t = cbind(P6t, c(0,P6t[-7,5]))
P6t = cbind(P6t, c(0,0,0,0,0,0,1))

S = matrix(c(1,0,0,0,0,0,0), 7, 1)
Sh = S


plotsh = function(Sh){
 mxx = function(x){which.max(x*10^(6:0))}
 Pt = 7-apply(Sh, 2, mxx)
 Pt = c(Pt[which(Pt>0)],0)
 t = 7*(1:length(Pt))
 plot(t, Pt, type = "l")
 segments(0,2.5, max(t)*7, 2.5) 
 Pt
}

linesh = function(Sh){
  mxx = function(x){which.max(x*10^(6:0))}
  Pt = 7-apply(Sh, 2, mxx)
  Pt = c(Pt[which(Pt>0)],0)
  t = 7*(1:length(Pt))
  lines(t, Pt, type = "l")
  segments(0,2.5, max(t)*7, 2.5) 
  Pt
}

meanInfection = function(S, plotit=TRUE){
  Sh=S
  while(sum(S[-7]>.001)){
    S=P6t%*%S
    Sh=cbind(Sh,S)
  }
  t=1:dim(Sh)[2]
  plot(7*t, 1-Sh[7,], type = "l", lty=2, xaxt="n")
  patent = colSums(Sh[1:3,])
  lines(7*t, patent)
  ix = min(which(patent<0.5))
  segments(0, 0.5, 7*t[ix], 0.5, lty =2)
  segments(7*t[ix], 0, 7*t[ix], 0.5, lty=2)
  axis(1, 7*t[ix], 7*t[ix])

  vv = 0.2
  ix = min(which(patent<vv))
  segments(0, vv, 7*t[ix], vv, lty =2)
  segments(7*t[ix], 0, 7*t[ix], vv, lty=2)
  axis(1, 7*t[ix], 7*t[ix])
  
  vv = 0.1
  ix = min(which(patent<vv))
  segments(0, vv, 7*t[ix], vv, lty =2)
  segments(7*t[ix], 0, 7*t[ix], vv, lty=2)
  axis(1, 7*t[ix], 7*t[ix])
  
  vv = 0.05
  ix = min(which(patent<vv))
  segments(0, vv, 7*t[ix], vv, lty =2)
  segments(7*t[ix], 0, 7*t[ix], vv, lty=2)
  axis(1, 7*t[ix], 7*t[ix])
  
}

oneInfection = function(S, plotit="plot",i=0){
  Sh=S
  while(sum(S[-7])>0){
    S = updateOld6(S)
    Sh = cbind(Sh, S)
  }

  mxx = function(x){which.max(x*10^(6:0))}
  Pt = 7-apply(Sh, 2, mxx)
  if(plotit=="plot") plotsh(Pt)
  if(plotit=="lines") linesh(Pt,i)
  Pt 
} 

plotsh = function(Pt){
  t = 7*(1:length(Pt))
  plot(t, Pt, type = "l", xlim = c(0,700))
  segments(0,2.5, max(t)*7, 2.5, lty=3) 
}

linesh = function(Pt,i=0){
  t = 7*(1:length(Pt))
  lines(t, Pt, type = "l", xlim = c(0, 700), col = i)
}

par(mfrow = c(2,1))

S = c(2,0,0,0,0,0,0)
meanInfection(S)

Pt=oneInfection(S, "plot")
for(i in 1:10)
  oneInfection(S, "lines", i)