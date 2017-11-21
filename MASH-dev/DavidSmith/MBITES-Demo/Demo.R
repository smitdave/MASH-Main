
getPoints = function(seed, nCenters,  rng, nPaC, nPaCvr, spr, centers=NULL){
  
  set.seed(seed)
  xCenters = runif(nCenters, -rng, rng)
  yCenters = runif(nCenters, -rng, rng)

  x = 0; y=0

  n = pmax(5, rnbinom(nCenters,mu=nPaC,size=nPaCvr))
  spread = rgamma(nCenters,1,1)*spr

  for(i in 1:nCenters){
    x = c(x,xCenters[i]+rnorm(n[i],0,spread[i]))
    y = c(y,yCenters[i]+rnorm(n[i],0,spread[i]))
  }
  x = x[-1]
  y = y[-1]

  plot(x,y, pch = 15, col = "red") 
  cbind(x,y) #return(list(xy=cbind(x,y), centers = cbind(xCenters, yCenters)))  
}

xy.f = getPoints(21,nCenters=5,rng=10,nPaC=12,nPaCvr=2,spr=1)
xy.l = getPoints(21,nCenters=25,rng=10,nPaC=8,nPaCvr=2,spr=.4)
N.l = length(xy.l[,1])
w.l = rgamma(length(xy.l[,1]), 1,1)

xy.f1 = getPoints(22,nCenters=25,rng=10,nPaC=10,nPaCvr=2,spr=.6)

xy.f = rbind(xy.f, xy.f1)
N.f = length(xy.f[,1])
w.f = rgamma(length(xy.f[,1]), 1,1)

plot(xy.f, pch = 15, col = "red", xlim = range(xy.f, xy.l), ylim = range(xy.f, xy.l))

points(xy.l, pch =15, col = "blue")
