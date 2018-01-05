
IBlock = function(i, ID){
  which(ID == unique(ID)[i])
}

GetIBlock = function(i, ID, X){
  X[iBlock(i,ID)]
}

SumIBlock = function(i, ID, X){
  sum(GetIBlock(i,ID, X)) 
}

IJBlock = function(i, j, ID, M){
  MM = M[IBlock(i,ID), ]
  MM[,IBlock(j,ID)]
}

GetIJBlock = function(i, j, ID, X){
  X[IJBlock(i,j,ID)]
}

SumIJBlock = function(i, j, ID, M){
  sum(GetIJBlock(i,j,ID,M))
}

CenterIJBlock = function(ii, x, H, J){
  i = which(J == unique(J)[ii])
  weighted.mean(x[i], H[i])
}



getIJBlock = function(ii, jj, pij, H, J){
  i = which(J == unique(J)[ii])
  j = which(J == unique(J)[jj])
  
  weighted.mean(pij[i,j], H[i]) 
}

sigmoid = function(x, k=1, X50=4){
  1-1/(1+exp(-k*(x-X50)))
}

kernSm = function(X,Y,H,PAR){with(PAR,{
  makeD = function(i){
    dst = sqrt((X[i]-X)^2 + (Y[i]-Y)^2)
    #sum(H/(1+dst^2)) 
    max(H*sigmoid(log10(dst),kd,d50)) 
  }
  ii = c(1:length(X))
  sapply(ii,makeD)
})} 

#kernSm = function(X,Y,H,alpha){
#  makeD = function(i){
#    dst = sqrt((X[i]-X)^2 + (Y[i]-Y)^2) 
#    sum(H*dst^alpha) 
#  }
#  ii = c(1:length(X))
#  sapply(ii,makeD)
#}



getPij = function(i,H,X,Y,dst,D,PAR){with(PAR,{
  #dst = sqrt((X[i]-X)^2 + (Y[i]-Y)^2)
  routineP = routine(i, H, X, Y, dst, D, PAR)
  travelP  = travel(i, H, X, Y, dst, D, PAR)
  #browser() 
  probs = p1*routineP + (1-p0-p1)*travelP
  probs[i] = p0 
  probs 
})}


connect = function(x, M){with(c(x,M),{

  D = rowSum(dd^-abs(alpha)%*%H) 
  P = H*D^beta*dd^-abs(gamma)
  pij = P/(psi + sum(P))
  
  k = length(unique(J))
  Q = matrix(0,k,k)
  for(i in 1:k) for(j in 1:k) Q[i,j] = getBlock(i,j,pij,H,J)
  -sum((Q-O)^2)
})}

getProbs = function(ix, M, PAR, wtD, showit = FALSE){with(M,{
  dst = sqrt((X[ix]-X)^2 + (Y[ix]-Y)^2)  
  pT = travel(ix,H,X,Y,dst,wtD,PAR) 
  pR = routine(ix,H,X,Y,dst,wtD,PAR)
  probs = getPij(ix,H,X,Y,dst,wtD,PAR) 
 list(Pij=probs, Rij=pR, Tij=pT)    
})}






