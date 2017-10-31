# test John Henry's code with the power of C++
# (need lots of runs for conference)
rm(list=ls());gc()

Rcpp::sourceCpp('~/Desktop/git/MASH-Main/MASH-dev/SeanWu/mbitesDE.cpp')

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

# run sim
dt = .1
tfin = 100
da = .25
afin = 40
# U = upwindCPP(dt = dt,tfin = tfin,dx = da,xfin = afin,par_ptr = par)
U = upwindSolveCPP(dt = dt,tfin = tfin,dx = da,xfin = afin)

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

par(mfrow=c(1,2))
