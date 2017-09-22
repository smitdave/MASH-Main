
dMin = 1:(60*24)
d2Min = c(1:(60*48))

cosHaz = function(t, o=-2, b=.5, p=3){
  #NOTE :: offset o is in hours
  pmax(0,(b+cos(2*pi*(t-o*60)/24/60)))^p 
}

cumCosHaz = function(o=-2,b=0.5,p=2){
  #NOTE :: offset o is in hours
  #cs = c(0,cumsum(cosHaz(d2Min[-1],k,o,b,p)))
  cs = cumsum(cosHaz(d2Min,o,b,p))
  cs1 = 2*cs/max(cs)
  cs1
}

rCosHazTim = function(t, o=-2, b=1.1, p=2, cs2=NULL){
  t0 = ceiling((t-floor(t))*60*24)+1
  if(is.null(cs2)) cs2=cumCosHaz(o,b,p)
  css2 = cs2[t0:c(t0+1440)]-cs2[t0]
  tm = min(which(css2>runif(1,0,1))) 
  # This occationally returns a number that is slightly larger than 1
  # which should never occur. 
  # print(c(check = diff(range(css2)))) 
  min(tm/60/24,1)
}

rCosHaz = function(t,P=1,o=-2,b=1,p=2,cs2=NULL){
  t+rCosHazTim(t,o,b,p,cs2)+rgeom(1,P)
}

#Check Timing


checkIt = function(t, P=.9, o=12, b=1, p=2, N=10000){
  t=t/24 
  par(mfrow = c(2,2))
  plot(dMin/60, cosHaz(dMin,o,b,p), type = "l", ylab = "Activity Levels", xaxt = "n", xlab = "Time of Day")
  axis(1, 4*c(0:6), 4*c(0:6))
  
  t0 = ceiling((t-floor(t))*60*24)+1
  ixx = t0:(t0+1440)
  
  cch = P*(cumCosHaz(o,b,p)[ixx] - cumCosHaz(o,b,p)[t0]) 
  plot(d2Min[ixx]/60, cch, type = "l", ylab = "CDF of Activity (One Day)", xlab = "Time of Day", xaxt = "n", ylim = c(0,1))  
  axis(1, 4*c(0:11), 4*c(0:11))
  
  tt = replicate(10000,rCosHazTim(1,o,b,p))
  hist(tt, xlab = "Time of Day", main = "Timing")
  
  tt=replicate(10000,rCosHaz(t,P,o,b,p))
  hist(tt, main = paste("Time to Event, P=",P), xlab = "Time (Days)") 
  tt
}

tt = checkIt (24, o=-4)

