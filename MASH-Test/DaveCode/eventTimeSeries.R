
hh = function(t,hb,hs,trend=0,wt=1){
  wt*(hb*(1+trend*t)*(1 + hs*sin(2*pi*t/365))^2) 
} 

make.weight = function(age, aa=1.7, bb=4){
  aa*age/(bb*365+age) 
} 

make.bites = function(N, mxYR, hb, hs, trend=0, wt=1, dt=5){
  t = seq(0,mxYR*365, by = dt)
  wt = make.weight(t) 
  xp = hh(t, hb, hs, wt=wt)
  xpC = cumsum(xp)
  bite.dt = runif(N,0,max(xpC))
  get.ix = function(i){
    min(which(bite.dt[i] < xpC)) 
  } 
  ixx = sapply(1:N, get.ix) 
  t[ixx] 
} 

#wt = seq(0, 5*365, by = 5)

age = seq(0:3650)
wt = make.weight(age) 
xp = sapply(age, hh, hb=2, hs = 1, wt=wt)
#plot(age, xp, type = "l") 

bites= sort(make.bites(40, 5, 1, 5, wt=wt, trend = .05))
#plot(bites, bites*0, type = "p")  

