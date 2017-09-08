
# P is a set of patches
# X,Y is a vector of coordinates for the patch centers 
# H is the vector of human population densities for P
# Hi is the vector of human population densities for Pi
# P0 is the initial set of patches (e.g. A3)
# Pi is a new set of patches we get from splitting one patch
# Pe is the final set of patches (e.g. a grid)
# ix is membership of Pe in P0
# D is the distance matrix for P
# Di is the new distance matrix for Pi 

 
#####
# The algorighm: 
# 1. NEED: i, H, X, Y for P0 
# 2. NEED: i, H, X, Y for Pe 
# 3. Compute H1, H2, and centers X1, Y1, X2, Y2 from Pe
# 3. Compute D1 from D :: newD(...) 
# 4.  
#####

newXYH = function(i, X, Y, Pe, ix){
} 

split2 = function(i, Patches, cx1, cx2, cy1, cy2, w1, w2, H1, H2){with(Patches,{with(par,{
    X = X[-i]  
    Y = Y[-i]
    Airow = A[i,][-i]
    Aicol = A[,i][-i]
    Aii = A[i,i]
    A = A[-i,-i]
    Hi = H[i]
    H = H[-i]
   
    # The new Distances
    d1 = sqrt((cx1-X)^2 + (cy1-Y)^2) 
    d2 = sqrt((cx2-X)^2 + (cy2-Y)^2) 
    d12 = sqrt((cx1-cx2)^2 + (cy1-cy2)^2)

    # Remake the distance matrix 
    D = D[-i,-i]
    D = rbind(D, d1, d2)
    Dii = matrix(c(0,d12,d12,0),2,2) 
    D = cbind(D,rbind(cbind(d1,d2),Dii))

    # INTERNAL
    fD = ker(c(0,d12), H1, H2, par) 
    A11 = p0+(Aii-p0)*fD[1]/(fD[1] + fD[2])#*H1/(H1+H2)
    A12 = (Aii-p0)*fD[2]/(fD[1] + fD[2])#*H2/(H1+H2)
    fD = ker(c(d12,0), H2, H1, par)
    A21 = (Aii-p0)*fD[1]/(fD[1] + fD[2])#*H1/(H1+H2)
    A22 = p0 + (Aii-p0)*fD[2]/(fD[1] + fD[2])#*H1/(H1+H2)
    Aint = matrix(c(A11,A21,A12,A22), 2, 2)

    # Time spent here by those living there
    A1f = ker(d1, H, H1, par)
    A2f = ker(d2, H, H2, par)
    A1 = Aicol*A1f/(A1f+A2f)
    A2 = Aicol*A2f/(A1f+A2f) 
    A = cbind(A,A1,A2)
    
    # Time spent there by those living here 
    A1t = ker(d1, H1, H, par)
    A2t = ker(d2, H2, H, par)
    A1 = Airow*A1t/(A1t+A2t)
    A2 = Airow*A2t/(A1t+A2t)
    A = rbind(A,cbind(rbind(A1,A2),Aint))
    
    H = c(H, H1, H2)
    ans = splitPfPR(PfPR[i], PfPR[-i], A, H, Hi, H1, H2, w1, w2, F, par)
    
    Patches$A = A 
    Patches$D = D  
    Patches$X = c(X, cx1, cx2) 
    Patches$Y = c(Y, cy1, cy2)
    Patches$H = H 
    Patches$N = N+1
    Patches$PfPR = ans$PfPR
    Patches$Rc   = ans$Rc
    Patches
})})} 

join2 = function(i,j,Patches){with(Patches,{
  cx = (X[i]*H[i] + X[j]*H[j])/(H[i] + H[j]) 
  cy = (Y[i]*H[i] + Y[j]*H[j])/(H[i] + H[j]) 
  X = X[-c(i,j)] 
  Y = Y[-c(i,j)] 
  d = sqrt((cx-X)^2 + (cy-Y)^2) 
  D = D[-c(i,j),-c(i,j)]  
  D = cbind(rbind(D,d), c(d,0))
  # Add the rows
  Aii = (A[i,i]+A[i,j]+A[j,i]+A[j,j])/2
  Arow = A[i,]+A[j,]
  A = rbind(A[-c(i,j),], Arow)
  Acol = A[,i]+A[,j]
  A = cbind(A[,-c(i,j)],Acol)
  A[N-1,N-1] = Aii
  
  H = c(H[-c(i,j)],H[i]+H[j])
  Patches$N = N-1
  Patches$A = A
  Patches$D = D 
  Patches$X = c(X, cx) 
  Patches$Y = c(Y, cy)
  Patches$H = H
  Patches
})}

f.exp = function(d,H1,H2,par){with(par,{
  # time spent by H1 in H2
  wt=exp(-a*(1+d/r)^b)*H1/(H1+H2)
  wt/sum(wt)
})}

kerDi = function(i,D,H,ker,par){
  ker(D[i,],H[i],H,par)
}



getA = function(D,H,ker,par){
  A = t(sapply(1:length(H),kerDi,D=D,H=H,ker=ker,par=par))
  A = A*(1-par$p0) + diag(par$p0, length(H))
}

parms = function(a=2,
    b=2,
    r=0.2,
    p0=0.9){
  list(a=a,b=b,r=r,p0=p0)
}

x = runif(10,0,1)
y = runif(10,0,1)
D = as.matrix(dist(cbind(x,y), upper=T, diag=T))
H = rnbinom(10, size=10, mu=5)*20 + 20
testPatches = list(X=x,Y=y,H=H,D=D,N=10)
testPatches$ker = f.exp
testPatches$par = parms() 
testPatches$A = with(testPatches, getA(D,H,ker, par))  

cx1 = x[10]+0.01
cx2 = x[10]-0.01
cy1 = y[10]+0.01
cy2 = y[10]-0.01
H1 = round(H[10]*.7) 
H2 = H[10]-H1

newPatches = split2(10, testPatches, cx1, cx2, cy1, cy2, H1, H2)
backPatches = join2(10,11,newPatches)

AA = backPatches$A - testPatches$A

x2Rc.2Patch = function(x1, x2, H1, H2, 
	A11, A12, A21, A22, 
	delta1=0, delta2=0, cS = .1, r=1/200)
{
  
  if(x1 < delta1/(delta1+r) | x2 < delta2/(delta2+r)) 
  {
    print("x2Rc ERROR: delta too large")
    print(c(x1=c(x1, delta1/(delta1+r), x2=c(x2, delta2/(delta2+r)))))
  } 
  
  H1r = H1/(A11*H1 + A12*H2) 
  H2r = H2/(A21*H2 + A22*H2)
  print(c(H1=H1,H2=H2, H1r=H1r, H2r=H2r))
  x1r = (A11*x1*H1 + A12*x2*H2)/(A11*H1 + A12*H2) 
  x2r = (A21*x1*H1 + A22*x2*H2)/(A21*H1 + A22*H2) 
 
  LHS = matrix(c(
  t11 = H1r*A11*x1r/(1+cS*x1r),
  t21 = H2r*A21*x1r/(1+cS*x1r),
  t12 = H1r*A12*x2r/(1+cS*x2r),
  t22 = H2r*A22*x2r/(1+cS*x2r)), 2,2)  
  
  RHS = matrix(c(x1/(1-x1)-delta1*r, x2/(1-x2)-delta2*r),2,1) 
  solve(LHS, RHS) 
}

wPfPR = function(wa, wb, Ha, Hb, X, par){with(par,{
  tot = Ha+Hb+1 
  while(tot > X){
   wa1 = wa*rgamma(1,a,a); wb1 = wb*rgamma(1,a,a)
   x1 = wa1/(wa1+wb1)
   tot = p*Ha + (1-p)*Hb
  } 
  x1 
})}

splitPfPR = function(PfPRi, PfPR, A, H, Hi, Ha, Hb, wa, wb,  F, par){
  xa = wPfPR(wa,wb,Ha,Hb,PfPRi*Hi,par)
  xb = (PfPRi*Hi - xa*Ha)/Hb
  PfPR = c(PfPR,x1,x2)
  Rc = PfPR2Rc(PfPR,H,A,par)
  list(PfPR=PfPR, Rc=Rc)
}

h2Rc.2Patch = function(h1, h2, H1, H2, 
                       A11, A12, A21, A22, 
                       delta1=0, delta2=0, cS = .1, r=1/200)
{
  #NOTE: Assumes delta1, delta2 are "baked in" to the estimates of h1, h2
  #      => delta*r < h 
  x1= h1/(h1+r); x2 = h2/(h2+r)
  if(x1 < delta1/(delta1+r) | x2 < delta2/(delta2+r)) 
  {
    print("h2Rc ERROR: delta too large")
    print(c(x1=c(x1, delta1/(delta1+r), x2=c(x2, delta2/(delta2+r)))))
  } else{
    x2Rc.2Patch(x1,x2,H1,H2,A11,A12,A21,A22,delta1,delta2,cS)
  } 
}

PfPR2Rc = function(x,H,A,par){with(par,{
  Hi = t(A)%*%H
  Xi = t(A)%*%(x*H)
  kappa = Xi/Hi
  foi = diag(as.vector(H*kappa/(1+c*S*kappa)/Hi), length(H))
  solve(A%*%foi,x/(1-x))
})}

R = x2Rc.2Patch(x1=.05, x2=.005, H1=200, H2=100, A11=.85, A12=.04, A21=.05, A22=.83, delta1=.00001, delta2=.00002)  
print(c(R=R))
R = h2Rc.2Patch(h1=.0002, h2=.00005, H1=200, H2=100, A11=.85, A12=.04, A21=.05, A22=.83, delta1=.0001, delta2=0.00002)  
print(c(R=R))

