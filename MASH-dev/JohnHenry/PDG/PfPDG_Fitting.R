library(readxl)
MT_PT_NP <- read_excel("~/GitHub/MASH-Main/MASH-dev/JohnHenry/PDG/MT_PT_NP.xlsx")
M = as.matrix(MT_PT_NP)

rm = rowMeans(M,na.rm=T)
plot(log10(rm),type="l",xlim=c(0,500),xlab="days",ylab="log10 Parasites per microL")
abline(h=log10(50))

plot(log10(M[,1]),type="l",xlim=c(0,200))
for(i in 20:20){
  #abline(v=7*(i-1))
  lines(log10(M[,i]),type="l")
}

#hist(log10(M[100,]))
hist(log10(colMeans(M[1:14,],na.rm=T)))
hist(log10(colMeans(M[15:28,],na.rm=T)))
hist(log10(colMeans(M[29:42,],na.rm=T)))
hist(log10(colMeans(M[43:56,],na.rm=T)))
hist(log10(colMeans(M[57:70,],na.rm=T)))
hist(log10(colMeans(M[71:nrow(M),],na.rm=T)))

m1 = log10(mean(colMeans(M[1:14,],na.rm=T),na.rm=T))
m2 = log10(mean(colMeans(M[15:28,],na.rm=T),na.rm=T))
m3 = log10(mean(colMeans(M[29:42,],na.rm=T),na.rm=T))
m4 = log10(mean(colMeans(M[43:56,],na.rm=T),na.rm=T))
m5 = log10(mean(colMeans(M[57:70,],na.rm=T),na.rm=T))
m6 = log10(mean(colMeans(M[71:84,],na.rm=T),na.rm=T))
m7 = log10(mean(colMeans(M[85:98,],na.rm=T),na.rm=T))
m8 = log10(mean(colMeans(M[99:112,],na.rm=T),na.rm=T))
m9 = log10(mean(colMeans(M[113:126,],na.rm=T),na.rm=T))
m10 = log10(mean(colMeans(M[127:140,],na.rm=T),na.rm=T))
m11 = log10(mean(colMeans(M[141:154,],na.rm=T),na.rm=T))
m12 = log10(mean(colMeans(M[155:168,],na.rm=T),na.rm=T))
m13 = log10(mean(colMeans(M[169:182,],na.rm=T),na.rm=T))
m14 = log10(mean(colMeans(M[196:210,],na.rm=T),na.rm=T))
m15 = log10(mean(colMeans(M[210:nrow(M),],na.rm=T),na.rm=T))


s1 = log10(var(colMeans(M[1:14,],na.rm=T),na.rm=T))
s2 = log10(var(colMeans(M[15:28,],na.rm=T),na.rm=T))
s3 = log10(var(colMeans(M[29:42,],na.rm=T),na.rm=T))
s4 = log10(var(colMeans(M[43:56,],na.rm=T),na.rm=T))
s5 = log10(var(colMeans(M[57:70,],na.rm=T),na.rm=T))
s6 = log10(var(colMeans(M[71:84,],na.rm=T),na.rm=T))
s7 = log10(var(colMeans(M[85:98,],na.rm=T),na.rm=T))
s8 = log10(var(colMeans(M[99:112,],na.rm=T),na.rm=T))
s9 = log10(var(colMeans(M[113:126,],na.rm=T),na.rm=T))
s10 = log10(var(colMeans(M[127:140,],na.rm=T),na.rm=T))
s11 = log10(var(colMeans(M[141:154,],na.rm=T),na.rm=T))
s12 = log10(var(colMeans(M[155:168,],na.rm=T),na.rm=T))
s13 = log10(var(colMeans(M[169:182,],na.rm=T),na.rm=T))
s14 = log10(var(colMeans(M[196:210,],na.rm=T),na.rm=T))
s15 = log10(var(colMeans(M[210:nrow(M),],na.rm=T),na.rm=T))


mu = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15)
f = splinefun(mu,method="natural")
plot(1:15,mu,ylim=c(0,5),xlab="Weeks Since Infection",ylab="log10 Parasites / microliter")
lines(seq(1,15,.01),f(seq(1,15,.01)),lty=2)
abline(h=log10(20))

var = c(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15)
plot(1:15,sqrt(var))

lm(var~mu)
lin = function(x){
  1.549*x+1.888
}

plot(mu,var)
x = seq(1,5,.01)
lines(x,lin(x))

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

