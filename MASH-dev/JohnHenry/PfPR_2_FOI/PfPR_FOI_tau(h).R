
######## This code plots the log10 FOI as a function of the log10 Odds Ratio of Infection
######## for the model with chemoprotection and constant treatment rate

w = 365/3
r = 365/200
tau0 = 0
h = seq(.01,10000,01)
f1 = function(h){
  h/(h*(1+tau0/w)+r+tau)
}

plot(log10(f1(h)/(1-f1(h))),log10(h),type="l",ylim=c(-2,4),xlab="log10 Odds Ratio of Infection",ylab="log10 FOI",main="RM Model")

taup = r*c(1/10,1/5,1/2,1,2,5,10)

for(j in 1:length(taup)){
  tau0 = taup[j]
  lines(log10(f1(h)/(1-f1(h))),log10(h),lty=2)
}


######## This code plots the maximum possible value of prevalence as a function of the treatment rate
######## for the model with chemoprotection and a treatement rate proportional to the FOI

w = 365/3
r = 365/200
tau0 = seq(.01,100,.01)
f = function(tau0){
  h = sqrt(r*w/(tau0*(1+tau0)))
  xmax = h*w/((h*(1+tau0)+r)*(tau0*h+w))
  return(xmax)
}

plot(log10(tau0),f(tau0),type="l",xlab="Log10 Base Treatment Rate tau_0",ylab="Maximum Prevalence x_max", main="Maximum Possible Prevalence under RM Model with Chemoprotection")
plot(log10(tau0),log10(f(tau0)/(1-f(tau0))),type="l",xlab="Log10 Base Treatment Rate tau_0",ylab="Maximum Prevalence x_max", main="Maximum Possible Odds Ratio under RM Model with Chemoprotection")
######## This code plots the log10 FOI as a function of the log10 Odds Ratio of Infection
######## for varying levels of treatment for the model with chemoprotection and a 
######## treatement rate proportional to the FOI

w = 365/3
r = 365/200
h = seq(.01,1000,.01)
tau0 = 0
g = function(h){
  h*w/((h*(1+tau0)+r)*(tau0*h+w))
}

plot(log10(g(h)/(1-g(h))),log10(h),xlab="log10 Odds Ratio of Infection",ylab="log10 FOI", type="l",xlim=c(-2,2), main="RM Model with Chemoprotection, Treatment Proportional to FOI")

taup = r*c(1/10,1/5,1/2,1,2,5,10)
for(j in 1:length(taup)){
  tau0 = taup[j]
  lines(log10(g(h)/(1-g(h))),log10(h),lty=2)
}

######## 

ff = function(h,r,tau){
  h/(r+(1+tau)*h)
}

h = seq(.01,100,.01)
plot(log10(ff(h,r,0)/(1-ff(h,r,0))),log10(h),type="l",xlab="log10 Odds Ratio of Infection",ylab="log10 FOI",main="Model with Treatment Rate Proportional to FOI")

for(j in 1:length(taup)){
  tau0 = taup[j]
  lines(log10(ff(h,r,tau0)/(1-ff(h,r,tau0))),log10(h),lty=2)
}

