library('rJava')
library('xlsx')
MT_PT_NP <- read_excel("~/GitHub/MASH-Main/MASH-dev/JohnHenry/PDG/MT_PT_NP.xlsx")
M = as.matrix(MT_PT_NP)

MT_GT_NP <- read_excel("~/Malaria Data Files/MT_GT_NP.xlsx")
G = as.matrix(MT_GT_NP)

MW = matrix(NaN,ncol=ncol(M),nrow=ceiling(nrow(M)/7))
for(i in 1:ncol(M)){
  for(j in 1:floor(nrow(M)/7)){
    MW[j,i] = mean(M[((j-1)*7+1):(7*j),i],na.rm=T)
  }  
}
write.csv(MW, "MT_PT_Weekly.csv")

GW = matrix(NaN,ncol=ncol(G),nrow=ceiling(nrow(G)/7))
for(i in 1:ncol(G)){
  for(j in 1:floor(nrow(G)/7)){
    GW[j,i] = mean(G[((j-1)*7+1):(7*j),i],na.rm=T)
  }  
}
write.csv(GW, "MT_GT_Weekly.csv")

par(mar=c(1,1,1,1))

plot(log10(MW[,1]),type="l",xlim=c(0,52),ylim=c(0,5.5))
par(mfrow=c(5,5))
for(i in 1:25){
  plot(log10(MW[,i]),type="l",xlim=c(0,52),ylim=c(0,5.5))
  lines(log10(GW[,i]),col="red")
}

Mdt = 0*MW
for(i in 1:ncol(MW)){
  x = 1:nrow(MW)
  y = MW[,i]
  yp = y[which(!(is.na(y) | y == 0))]
  xp = x[which(!(is.na(y) | y == 0))]
  fit = lm(log10(yp)~xp)
  yfit = fit$coefficients[[1]]+fit$coefficients[[2]]*xp
  ydt = log10(yp)-yfit
  yfin = NaN*y
  yfin[which(!(is.na(y) | y == 0))] = ydt
  Mdt[,i] = yfin
}
par(mfrow = c(5,5))
for(i in 26:50){
  qqnorm(Mdt[,i])
}
acf_MT = function(i){
  y = Mdt[,i]
  yp = y[which(!is.na(y))]
  acf(yp)
}
for(i in 26:50){
  acf_MT(i)
}

par(mar=c(5.1,4.1,4.1,2.1))


############ largest contiguous portion

Mdt = 0*M
for(i in 1:ncol(M)){
  y = na.contiguous(M[,i])
  x = which(M[,i]==y)
}


f = function(x,a){
  px = x[which(x>=0)]
  nx = x[which(x<0)]
  py = 2-exp(-a*px)
  ny = exp(a*nx)
  y = 0*x
  y[which(x>=0)] = py
  y[which(x<0)] = ny
  y = y/2
  return(y)
}