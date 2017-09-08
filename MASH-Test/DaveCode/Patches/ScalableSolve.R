
#Step 1: pick a mesh of centers

pickCenters = function(landscape, N){with(landscape,{
  #Returns a mesh of (N+1)^2 Items
  ix = 2*c(1:N)
  NN = 2*N
  xmesh = min(X) + c(0:NN)[ix]/NN*(max(X)-min(X))
  ymesh = min(Y) + c(0:NN)[ix]/NN*(max(Y)-min(Y))
  rx = 3*diff(xmesh)[1]/4
  ry = 3*diff(ymesh)[1]/4
  list(X=xmesh,Y=ymesh, rx=rx, ry=ry) 
})}

getIx = function(i,j, landscape, centers){
  cX=centers$X[i]; rx = centers$rx
  cY=centers$Y[i]; ry = centers$ry
  
  ixX = which(landscape$X > cX-rx & landscape$X < cX+rx)
  ixY = which(landscape$Y > cY-ry & landscape$X < cY+ry)
  intersect(ixX,ixY)
}

RMMacro0.Rc2x.SolveByTile = function(landscape,Rc,psi,alpha,par,N){
  centers = pickCenters(landscape,N)
  lX = length(landscape$X)
  storeIt = matrix(0, N^2, lX)
  cc = 0
  for(i in 1:N) for (j in 1:N){
    cc=cc+1
    print(c(cc, " of ", N^2 ))
    ixx = getIx(i,j,landscape,centers)
    if(length(ixx)>0){
      print(c("length of ixx = ", length(ixx))) 
      H1 = landscape$H[ixx]
      psi1a = psi[ixx,]
      psi1 = psi1a[,ixx]
      Rc1 = Rc[ixx]
      x = RMMacroHum.eq(Rc1, H1, psi1, alpha, par)$x
      browser() 
      storeIt[cc,ixx]=x
    } 
  }
  storeIt
}