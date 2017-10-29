#install.packages('animation')
library(animation)
library(jsonlite)
library(plotly)

#jsonOut=prettify(toJSON(mosquito_par))
#write(jsonOut,paste0(DIR,"mosquitoParameters.json"))

par=read_json(paste0("mosquitoParameters.json"),simplifyVector=TRUE)

activity = function(t,ave,max=ave) {
  (max-ave)*cos(2*pi*t/24)+ave
}

t = 1:480/10
plot(t,activity(t),type="l")

##expected number of eggs in each clutch that makes it to adulthood
e = 5

##waiting times
gammaF = function(t){activity(t,1,1.5*1)}
gammaB = function(t){activity(t,.75,1.5*.75)}
gammaR = function(t){activity(t,1.5,1.5*1.5)}
gammaL = function(t){activity(t,.75,1.5*.75)}
gammaO = function(t){activity(t,1,1.5*1)}

##probability of success at event
PF = .99
PB = .99
PR = .99
PL = .99
PO = .99

##probability of surviving event
sF = .95
sB = .98
sR = .98
sL = .8
sO = .98

##success * survive / time
rF = function(t){PF/gammaF(t)*sF}
rB = function(t){PB/gammaB(t)*sB}
rR = function(t){PR/gammaR(t)*sR}
rL = function(t){PL/gammaL(t)*sL}
rO = function(t){PO/gammaO(t)*sO}

##1/time (rate *something* happens)
dF = function(t){1/gammaF(t)}
dB = function(t){1/gammaB(t)}
dR = function(t){1/gammaR(t)}
dL = function(t){1/gammaL(t)}
dO = function(t){1/gammaO(t)}

##Fail * survive / time
aF = function(t){(1-PF)*sF/gammaF(t)}
aB = function(t){(1-PB)*sB/gammaB(t)}
aR = function(t){(1-PR)*sR/gammaR(t)}
aL = function(t){(1-PL)*sL/gammaL(t)}
aO = function(t){(1-PO)*sO/gammaO(t)}

##Transition probabilities given success
M = matrix(c(4,2,1,0,1,2,1,1,0,1,2,1,1,0,2,1,1,1,0,1,6,4,2,1,0),ncol=5,nrow=5)
M = M/rowSums(M)

##time from eggs being laid to adult-mosquito-hood
tau = 21

f = function(array,t,a){
  
  Fo = array[t,a,1]
  Fe = array[t,a,2]
  Bo = array[t,a,3]
  Be = array[t,a,4]
  R = array[t,a,5]
  Lo = array[t,a,6]
  Le = array[t,a,7]
  Oo = array[t,a,8]
  Oe = array[t,a,9]
  
  c(
    rO((t-1)*dt)*M[5,1]*(Oe+Oo) + rB((t-1)*dt)*M[2,1]*(Be+Bo) + rR((t-1)*dt)*M[3,1]*R + (rF((t-1)*dt)*M[1,1]+aF((t-1)*dt))*Fe - dF((t-1)*dt)*Fo,
    (rF((t-1)*dt)*M[1,1]+aF((t-1)*dt))*Fo - dF((t-1)*dt)*Fe,
    rO((t-1)*dt)*M[3,2]*(Oe+Oo) + rF((t-1)*dt)*M[1,2]*(Fe+Fo) + rR((t-1)*dt)*M[3,2]*R + rB((t-1)*dt)*(M[2,2]+aB((t-1)*dt))*Bo - dB((t-1)*dt)*Bo,
    (rB((t-1)*dt)*M[2,2]+aB((t-1)*dt))*Bo - dB((t-1)*dt)*Be,
    rB((t-1)*dt)*M[2,3]*(Be+Bo) - dR((t-1)*dt)*R,
    rR((t-1)*dt)*M[3,4]*R + rO((t-1)*dt)*M[5,4]*(Oe+Oo) + (rL((t-1)*dt)*M[4,4]+aL((t-1)*dt))*Le - dL((t-1)*dt)*Lo,
    (rL((t-1)*dt)*M[4,4]+aL((t-1)*dt))*Lo - dL((t-1)*dt)*Le,
    rL((t-1)*dt)*M[4,5]*(Le+Lo)+ rR((t-1)*dt)*M[3,5]*R + (rO((t-1)*dt)*M[5,5]+aO((t-1)*dt))*Oe - dO((t-1)*dt)*Oo,
    (rO((t-1)*dt)*M[5,5]+aO((t-1)*dt))*Oo - dO((t-1)*dt)*Oe
  )
  
}

#########################################

##1st order (dx) upwind scheme, 1st order (dt) FE

upwind1 = function(dt,tfin,dx,xfin,rhs,k) {
  
  ttot = tfin/dt
  xtot = xfin/dx
  
  m = matrix(nrow=xtot+1,ncol=xtot+1)
  diag= rep(1,xtot+1)
  diag1 = rep(-1,xtot)
  diag(m) <- diag
  m[row(m)==(col(m)+1)] = diag1
  m[is.na(m)]=0
  m = m*dt/dx
  
  x = 0:xtot*dx
  t = 0:ttot*dt
  ##array to contain all 9 matrices (time x age for each state variable)
  v = matrix(0,nrow=xtot+1,ncol=9)
  v[1,1] = 1000
  w = v
  A = array(0,dim=c(ttot+1,xtot+1,9))
  A[1,1,1] = 1000
  for(i in 1:ttot){
    for(k in 1:9){
      #      z = w[length(x),k]
      w[,k] = v[,k] - m%*%v[,k]
      #      w[length(x),k] = w[length(x),k]+z
      ##boundary condition - emerge from eggs
      if(k==1){
        v[1,k] = ifelse(ceiling(dt*i+dt/2)<=tau,0,e*sum(v[,8]+v[,9]))
      }
      
      for(j in 2:(xtot+1)){
        v[j,k] = w[j,k] + dt*rhs(A,i,j)[k]
      }
      A[i+1,,] = v
    }
  }
  return(A)
}

##run simulation
dt = .1
tfin = 100
da = .25
afin = 40
U = upwind1(dt,tfin,da,afin,f,9)
t = 0:(tfin/dt)*dt
x = 0:(afin/da)*da

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
plot(t,(Ut[,1]+Ut[,2])/N,type="l",ylim=c(0,1))
##plot proportion blood feeding
lines(t,(Ut[,3]+Ut[,4])/N,lty=2)
lines(t,1-(Ut[,1]+Ut[,2]+Ut[,3]+Ut[,4])/N,lty=3)

##plot propotion blood feeding or attempting to blood feed
plot(t,(Ut[,1]+Ut[,2]+Ut[,3]+Ut[,4])/N,type="l",ylim=c(0,1))
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


##video showing proportions in 
saveHTML({
  for(i in 1:length(t)){
    #    par(mfrow=c(3,2))
    #    plot(x,(U[i,,1]+U[i,,2])/N[i],type="l",ylim=c(0,.2),main="Searching for Blood")
    #    plot(x,(U[i,,3]+U[i,,4])/N[i],type="l",ylim=c(0,.05),main="Bloodfeeding")
    #    plot(x,U[i,,5]/N[i],type="l",ylim=c(0,.1),main="Resting")
    #    plot(x,(U[i,,6]+U[i,,7])/N[i],type="l",ylim=c(0,.008),main="Searching for Water")
    #    plot(x,(U[i,,8]+U[i,,9])/N[i],type="l",ylim=c(0,.025),main="Ovipositing")
    plot(x,Ua[i,]/N[i],type="l",ylim=c(0,.5),main="Age Structure Over Time")
  }
},interval = 0.1, ani.width = 1000, ani.height = 400)