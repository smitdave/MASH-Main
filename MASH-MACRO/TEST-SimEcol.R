###############################################################################
#       __  ______   _____ __  __      __  ______   __________  ____
#      /  |/  /   | / ___// / / /     /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| | \__ \/ /_/ /_____/ /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ |___/ / __  /_____/ /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_/____/_/ /_/     /_/  /_/_/  |_\____/_/ |_|\____/
#
#   MASH-MACRO
#   Testing Ground
#   August 31, 2017
#
###############################################################################

rm(list=ls());gc()

Tmax=10

lambda <- function(x){
  100*(sin(x*pi)+1)
}

Lambda <- function(t){
  integrate(f=lambda,lower=0,upper=t)$value
}

Lambda <- Vectorize(FUN = Lambda)

v=seq(0,Tmax,length=1000)
s=0
X=0

t=min(v[which(Lambda(v)>=s)])

while(X[length(X)]<Tmax){
  u=runif(1)
  s=s-log(u)
  t=min(v[which(Lambda(v)>=s)])
  if(t>Tmax){break()}
  X=c(X,t)
  print(paste0("sampled t: ",t))
}

hist(X,breaks=seq(0,max(X)+1,by=.1),col="steelblue")
   u=seq(0,max(X),by=.02)
   lines(u,lambda(u)/10,lwd=2,col="red")

integrate(f = lambda,lower = 0,upper = 1,subdivisions = 1e3)$value

cutX = cut(x = X,breaks=0:10,right = FALSE)
