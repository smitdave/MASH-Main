M = as.matrix(MT_PT_NP)

rm = rowMeans(M,na.rm=T)
plot(log10(rm),type="l",xlim=c(0,500),xlab="days",ylab="log10 Parasites per microL")
abline(h=log10(50))

plot(log10(M[,1]),type="l",xlim=c(0,200))
for(i in 20:20){
  #abline(v=7*(i-1))
  lines(log10(M[,i]),type="l")
}

hist(log10(M[100,]))

v = rep(0,1000)
dv = v
for(i in 1:1000){
  v[i] = sum(is.na(M[i,])/333)
  if(i>1){
    dv[i] = abs(v[i]-v[i-1])
  }
}
plot(v,type="l",ylim=c(0,1))
abline(h=1)

mu = sum(1-v)
abline(v=mu)

plot(dv,type="l")
abline(v=mu)
