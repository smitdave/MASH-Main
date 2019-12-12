library(plotly)

an = rep(.5,1000)
gn = function(x,A,C,g){
  A*x*(1-C*x^g)/(1+x)
}
A = 9
C = 1
g = .5
for(i in 1:999){
  an[i+1] = gn(an[i],A,C,g)#fn(an[i],A,B,s,p)
}
plot(an,ylim=c(0,1))

hist(log(an/(1-an)),breaks=10,freq=F)

plot(an[1:499],an[2:500],xlim=c(0,1),ylim=c(0,1))
lines(0:10,0:10,lty=2)
plot(an[1:499],an[2:500],type="l")
lines(0:10,0:10,lty=2)
points(an[1:499],an[2:500])

lag0 = list(an[1:998])
lag1 = list(an[2:999])
lag2 = list(an[3:1000])
df = data.frame(lag0 = lag0[[1]], lag1 = lag1[[1]], lag2 = lag2[[1]])
plot_ly(df,x=~lag0,y=~lag1,z=~lag2, type='scatter3d',mode='points')

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
fp = function(x,A,B,s,p){
  A*(1-B*s*x^(p-1))/(1+B*x) + A*x*(((1+B*x)*(1-p)*B*s*x^(p-2) - (1-B*s*x^(p-1))*B)/(1+B*x)^2)
}

gp = function(x,A,C,g){
  A*(1-C*x^g)/(1+x) + A*x*((1+x)*(-g)*C*x^(g-1) - (1-C*x^g))/(1+x)^2
}

n = length(an)
#L = sum(log(abs(fp(an,A,B,s,p))))/(n+1)
L = sum(log(abs(gp(an,A,C,g))))/(n+1)
## lyapunov exponent approx .1714409 - Positive value implies chaotic trajectory
k = 1:600
Lk = 0*k
for(i in 1:600){
  #Lk[i] = sum(log(abs(fp(an[1:i],A,B,s,p))))/i
  Lk[i] = sum(log(abs(gp(an[1:i],A,C,g))))/i
}
plot(k,Lk,type="l",ylim=c(-.1,1),xlab="k",ylab="Lk",main="Lyapunov Exponent Evaluated With k Iterates, Lk")
abline(h=L,lty=2)

### bifurcation diagram with bifurcation parameter B
A = 10
B = .5
s = 1
p = 1.5
fn = function(x,A,B,s,p){
  A*x*(1-B*s*x^(p-1))/(1+B*x)
}
Bi = seq(.001,1,.001)
x0 = .05
for(i in 1:length(Bi)){
  xj = rep(0,400)
  xj[1] = x0
  for(j in 1:399){
    xj[j+1] = fn(xj[j],A,Bi[i],s,p)
  }
  if(i==1){
    plot(rep(Bi[i],length(unique(xj[300:400]))),unique(xj[300:400]),xlim=c(0,1),ylim=c(0,4),type="p",main="Bifurcation Diagram for parameter B",xlab="B",ylab="an")
  }
  if(i>1){
    points(rep(Bi[i],length(unique(xj[300:400]))),unique(xj[300:400]),cex=.1)
  }
}


min = 0
C = 1
g = .5
gn = function(x,A,C,g){
  A*x*(1-C*x^g)/(1+x)
}
Ai = seq(min,9,.001)
x0 = .05
for(i in 1:length(Ai)){
  xj = rep(0,300)
  xj[1] = x0
  for(j in 1:299){
    xj[j+1] = gn(xj[j],Ai[i],C,g)
  }
  if(i==1){
    plot(rep(Ai[i],length(unique(xj[200:300]))),unique(xj[200:300]),xlim=c(min,9),ylim=c(0,1),type="p",main="Bifurcation Diagram for parameter A",xlab="A",ylab="an")
  }
  if(i>1){
    points(rep(Ai[i],length(unique(xj[200:300]))),unique(xj[200:300]),cex=.1)
  }
}
abline(h=c(0,1),v=1,lty=2)
