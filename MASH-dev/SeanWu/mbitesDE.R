################################################################################
#
#  MBITES-DE
#  PDE approximation of semi-markov model of mosquito life-cycle
#  MBITES Team
#  October 2017
#
################################################################################

# test John Henry's code with the power of C++
# (need lots of runs for conference)
rm(list=ls());gc()

Rcpp::sourceCpp('~/Desktop/git/MASH-Main/MASH-dev/SeanWu/mbitesDE.cpp')

# run sim
dt = .1
tfin = 180
da = .25
afin = 40

# output U has dimensions [time,age,states]
U = upwindSolveCPP(dt = dt,tfin = tfin,dx = da,xfin = afin,e = 8)

t = 0:(tfin/dt)*dt
x = 0:(afin/da)*da

par(mfrow=c(3,2))

##see total population size as a function of time
N = rep(0,length(t))
for(i in 1:length(t)){
  N[i] = sum(U[i,,])
}

plot(x,U[length(t),,2]/N[length(t)],type="l")


##plot total population as a function of time
plot(t,N,type="l")

##see U as a function of time in each compartment over all ages
Ut = matrix(0,nrow=length(t),ncol=9)
for(k in 1:9){
  for(i in 1:length(t)){
    Ut[i,k] = sum(U[i,,k])
  }
}

##plot proportion attempting to blood feed
plot(t,(Ut[,1]+Ut[,2])/N,type="l",ylim=c(0,1),main="proportion searching for blood meal")
##plot proportion blood feeding
lines(t,(Ut[,3]+Ut[,4])/N,lty=2)
lines(t,1-(Ut[,1]+Ut[,2]+Ut[,3]+Ut[,4])/N,lty=3)

##plot propotion blood feeding or attempting to blood feed
plot(t,(Ut[,1]+Ut[,2]+Ut[,3]+Ut[,4])/N,type="l",ylim=c(0,1),main="proportion searching or feeding")
abline(h=1,lty=2)
abline(h=0,lty=2)

##see U across all 9 compartments; age structure of U over time
Ua = matrix(0,nrow=length(t),ncol=length(x))
for(i in 1:length(t)){
  for(j in 1:length(x)){
    Ua[i,j] = sum(U[i,j,])
  }
}

plot(x,Ua[length(t),]/N[length(t)],type="l")

par(mfrow=c(1,1))

################################################################################
# Surface Plots
################################################################################

library(viridis)

# plotting functions

colGG <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

persp.withcol <- function(x,y,z,color,...,xlg=TRUE,ylg=TRUE){
  colnames(z) <- y
  rownames(z) <- x
  
  nrz <- nrow(z)
  ncz <- ncol(z) 
  
  nb.col = length(color)
  
  zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
  facetcol <- cut(zfacet, nb.col)
  par(xlog=xlg,ylog=ylg)
  persp(
    as.numeric(rownames(z)),
    as.numeric(colnames(z)),
    as.matrix(z),
    col=color[facetcol],
    ...
  )
}

# output U has dimensions [time,age,states]

totalPop = matrix(data = 0,nrow = dim(U)[1],ncol = dim(U)[2])

for(i in 1:dim(U)[3]){
  totalPop = totalPop + U[,,i]
}

feedPop = U[,,1] + U[,,2] + U[,,3] + U[,,4]
feedPop = feedPop / totalPop
feedPop[is.nan(feedPop)] = 0

persp.withcol(x = t,y = x,z = feedPop,color = viridis(60),theta=120,phi=30,border=NA,xlab="Time",ylab="Age",zlab="Proportion Feeding",ticktype="detailed")


library(viridis)
library(rgl)

y <- 2000 * feedPop # Exaggerate the relief
x <- 10 * (1:nrow(y)) # 10 meter spacing (S to N)
z <- 10 * (1:ncol(y)) # 10 meter spacing (E to W)
ylim <- range(y)
ylen <- ylim[2] - ylim[1] + 1
colorlut <- viridis(ylen) # height color lookup table
col <- colorlut[ y-ylim[1]+1 ] # assign colors to heights
rgl.open()
par3d(windowRect = 50 + c( 0, 0, 800, 800 ) )
rgl.bg(color = "grey80",fogtype = "none",alpha=0.5)
rgl.viewpoint(theta = -90, phi = 20, zoom = 1)
rgl.surface(x, z, y, color=col, back="lines")
movie3d(spin3d(axis = c(0, 1, 0),rpm=-4.5), duration = 36, fps=20, dir = "~/Desktop/git/MASH-Main/MASH-dev/SeanWu/mbitesDE_gif/")
rgl.close()





################################################################################
# DEPRECATED
################################################################################

# upwindCPP = function(dt,tfin,dx,xfin,par_ptr,tau=21,e=5) {
#   
#   ttot = tfin/dt
#   xtot = xfin/dx
#   
#   m = matrix(nrow=xtot+1,ncol=xtot+1)
#   diag= rep(1,xtot+1)
#   diag1 = rep(-1,xtot)
#   diag(m) <- diag
#   m[row(m)==(col(m)+1)] = diag1
#   m[is.na(m)]=0
#   m = m*dt/dx
#   
#   x = 0:xtot*dx
#   t = 0:ttot*dt
#   ##array to contain all 9 matrices (time x age for each state variable)
#   v = matrix(0,nrow=xtot+1,ncol=9)
#   v[1,1] = 1000
#   w = v
#   A = array(0,dim=c(ttot+1,xtot+1,9))
#   A[1,1,1] = 1000
#   for(i in 1:ttot){
#     for(k in 1:9){
#   
#       w[,k] = v[,k] - m%*%v[,k]
#   
#       ##boundary condition - emerge from eggs
#       if(k==1){
#         v[1,k] = ifelse(ceiling(dt*i+dt/2)<=tau,0,e*sum(v[,8]+v[,9]))
#       }
#       
#       for(j in 2:(xtot+1)){
# 
#         # v[j,k] = w[j,k] + dt*rhs(A,i,j)[k]
#         v[j,k] = w[j,k] + dt*mbitesDE_cpp(Fo = A[i,j,1],Fe = A[i,j,2],
#                                           Bo = A[i,j,3],Be = A[i,j,4],
#                                           R = A[i,j,5],
#                                           Lo = A[i,j,6],Le = A[i,j,7],
#                                           Oo = A[i,j,8],Oe = A[i,j,9],
#                                           t = i,dt = dt,p = par_ptr)[k]
#         
#       }
#       A[i+1,,] = v
#     }
#   }
#   return(A)
# }

# make a pointer to paramters struct (exposed as SEXP to Rcpp::XPtr<parameters>)
# par = make_parameters()
