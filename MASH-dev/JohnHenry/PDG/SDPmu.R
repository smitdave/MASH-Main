an = rep(.05,1000)
for(i in 1:999){
  an[i+1] = fn(an[i],A,B,s,p)#8.5*an[i]*(1-(.95*an[i]+an[i]^.5)/(1+.95*an[i]))
}
plot(an,type="l")

hist(log(an/(1-an)),breaks=1000,freq=F)

hist(tan(an),breaks=1000,freq=F)
hist(an,breaks=1000,freq=F)

plot(an[1:499],an[2:500],xlim=c(0,1),ylim=c(0,1))
lines(0:10,0:10,lty=2)
plot(an[1:499],an[2:500],type="l")
lines(0:10,0:10,lty=2)
points(an[1:499],an[2:500])

x = seq(0,1,.01)
plot(x,8.5*x*(1-(.95*x+x^.5)/(1+.95*x)),type="l",xlim=c(0,1),ylim=c(0,1.1),xlab="x[n]",ylab="x[n+1]",main="Asymptotic Iterated Mapping")
lines(x,x)
abline(h=1,lty=2)
abline(v=1,lty=2)
abline(h=0,lty=2)

xy = seq(0,100,1)
plot(xy,20*xy*(1-(.65*xy+.05*xy^.45)/(1+.65*xy)),type="l",xlab="x[n]",ylab="x[n+1]",main="Asymptotic Iterated Mapping")
lines(xy,xy)

### calculate lyapunov exponent
A = 8.5
B = 1.05
s = 1
p = 1.5
fp = function(x,A,B,s,p){
  A*(1-B*s*x^(p-1))/(1+B*x) + A*x*(((1+B*x)*(1-p)*B*s*x^(p-2) - (1-B*s*x^(p-1))*B)/(1+B*x)^2)
}

n = length(an)
L = sum(log(abs(fp(an,A,B,s,p))))/(n+1)
## lyapunov exponent approx .1714409 - Positive value implies chaotic trajectory
k = 1:1000
Lk = 0*k
for(i in 1:1000){
  Lk[i] = sum(log(abs(fp(an[1:i],A,B,s,p))))/i
}
plot(k,Lk,type="l",xlab="k",ylab="Lk",main="Lyapunov Exponent Evaluated With k Iterates, Lk")
abline(h=L,lty=2)

### bifurcation diagram with bifurcation parameter A
fn = function(x,A,B,s,p){
  A*x*(1-B*s*x^(p-1))/(1+B*x)
}
Bi = seq(.7,1.5,.001)
x0 = .05
for(i in 1:length(Bi)){
  xj = rep(0,500)
  xj[1] = x0
  for(j in 1:499){
    xj[j+1] = fn(xj[j],A,Bi[i],s,p)
  }
  if(i==1){
    plot(rep(Bi[i],length(unique(xj[400:500]))),unique(xj[400:500]),xlim=c(.7,1.5),ylim=c(0,1.3),type="p")
  }
  if(i>1){
    points(rep(Bi[i],length(unique(xj[400:500]))),unique(xj[400:500]),cex=.1)
  }
}
abline(v=1.05)