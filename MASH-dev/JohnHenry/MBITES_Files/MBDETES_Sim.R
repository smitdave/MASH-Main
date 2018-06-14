activity = function(t,ave,max=ave) {
  (max-ave)*cos(2*pi*t/24)+ave
}

t = 1:480/10
plot(t,activity(t),type="l")

##expected number of eggs in each clutch that makes it to adulthood
e = 30

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
PL = .95
PO = .95

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
  
  ## determines total number of 
  n = length(array[1,1,])
  
  Fe = array[t,a,1]
  B = array[t,a,2]
  R = array[t,a,3]
  L = array[t,a,4]
  O = array[t,a,5]
  
  c(
    
  )
  
}