
MOSY.sde = function(lambda, kappa, U,F,Y,Z, p=11/12, L=12, a=.3)
{
  T = length(kappa) 
  now = list(t=1,U=U,F=F,Y=Y,Z=Z)
  hist = as.vector(unlist(now)) 
  for(i in 1:T){ 
    #browser() 
    #print(now) 
    now = MOSY.sstep(now, p,L,a,lambda,kappa)
    hist = rbind(hist,as.vector(unlist(now))) 
  } 
  hist
}

MOSY.sstep = function(vars, p=11/12, L=12, a=.3, lambda, kappa){
  with(vars,{
  U = rbinom(1,U,p)
  F = rbinom(1,F,p)
  Z = rbinom(1,Z+Y[L],p) 
  Y[-1] = rbinom(L-1,Y[-L],p)
  FirstFeed = rbinom(1, U, a) #Number of nonparous mosquitoes that feed
  FFInfec = rbinom(1, FirstFeed, kappa[t]) #Number of those mosquitoes who feed on an infectious person
  SFInfec = rbinom(1, F, a*kappa[t]) #Number of parous mosquitoes that feed on an infectious person
  Y[1] = SFInfec + FFInfec #The total number of new infected mosquitoes
  F = F - SFInfec + (FirstFeed - FFInfec) #Parous susc. mosquitoes (Old - Infected + New)
  U = U - FirstFeed + rpois(1,lambda[t]) #Nonparous mosquitoes (Old - Infected + New)
  #browser() 
  list(t=t+1,U=U,F=F,Y=Y,Z=Z)
})}



get.last = function(mt){
  lst = dim(mt)[1]
  L = dim(mt)[2]-4
  M = mt[lst,-1]  
  U = M[1]
  F = M[2]
  Y = M[2 + 1:L] 
  Z = M[3 + L]
  c(U=U,F=F,Y=Y,Z=Z)     
}


sample = function(mt, p=.01, a=.3){
  lst = dim(mt)[1]
  L = dim(mt)[2]-4
  U = mt[,2]
  F = mt[,3]  
  Y = mt[,3 + 1:L]
  Z = mt[,4 + L]
  M = U+F+rowSums(Y)+Z
  urate = U/M
  frate = F/M
  yrate = Y/rep(M,L)
  zrate = Z/M
  #ZZ = rnbinom(N, mu=p*Z[t], size = 1/4.2) 
  #MM = rnbinom(N, mu=p*(U[t]+Y[t]), size = 1/4.2) 
  #list(Z=ZZ,M=MM+ZZ)
  TrueM = M
  MM = matrix(0,5,lst)
  UU = matrix(0,5,lst)
  FF = matrix(0,5,lst)
  YY = array(0,dim=c(5,lst,L))
  ZZ = matrix(0,5,lst)
  OO = matrix(0,5,lst)
  PaR = matrix(0,5,lst)
  for (temptime in 1:lst){
	if (M[temptime]>0){
	  probs = c(urate[temptime],frate[temptime],yrate[temptime,],zrate[temptime])
	  for (pull in 1:5){
	    MM[pull,temptime] = rnbinom(1, mu=a*p*M[temptime], size = 2)
	    #probs[is.na(probs)]=0#Why would this happen still?
        draw = rmultinom(1,MM[pull,temptime],probs)
	    UU[pull,temptime] = draw[1]
	    FF[pull,temptime] = draw[2]
	    YY[pull,temptime,] = draw[2+1:L]
	    ZZ[pull,temptime] = draw[3+L]
		OO[pull,temptime] = sum(draw[3+1:L])
		PaR[pull,temptime] = sum(draw[2:(3+L)])
	  }
	}
  }
  list(Parity=PaR,Z=ZZ,O=OO,M=MM,TrueM=TrueM) 
}


oneSample = function(lambda, kappa, L=12, p=0.01,a=.3){
  ans = MOSY.sde(lambda, kappa, U=0, F=0, Y=rep(0,L),Z=0)
  lst = get.last(ans)
  Uo = lst[1]; Fo = lst[2]; Yo = lst[2+1:L]; Zo=lst[L+3]
  ans = MOSY.sde(lambda, kappa, U=Uo, F=Fo, Y=Yo,Z=Zo)
#  plotit(ans, lambda, kappa) 
  dd = sample(ans,p) 
  list(Par=dd$Parity,Z=dd$Z,O=dd$O,M=dd$M,TrueM=dd$TrueM)
}
