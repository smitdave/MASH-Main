## Using PDG simulations to determine average parasite densities across transmission intensity
## conditioned on infection. Create a human, run for 10 years with possible reexposure, then
## pull parasite density; repeat many times

h = seq(.2,5,.05)
N = 20000
PD = matrix(rep(0,length(h)*N),nrow=length(h))
dt = 10/365
tau = 0*h

for(i in 1:nrow(PD)){
  hh = h[i]
  human = PDGHuman$new()
  for(j in 1:ncol(PD)){
    human$update_Human(dt)
    Pfnew = rpois(1,h[i]*dt)
    ## if (+) and density of greater than 0 parasites per cmm, record parasite density
    ## (can run MCMC on mean detectability)
    PD[i,j] = ifelse(human$get_Pt()>0,human$get_Pt(),NaN)
    ## if febrile, treat - this mimicks PRISM
    if(Pfnew>0){human$infect_Human(Pfnew)}
    if(rbinom(1,1,human$get_pFever())==1){
      human$clear_All_Pathogens()
      tau[i] = tau[i]+1
      }
  }
}

## average parasitemia 'filtered' through microscopy,
## assuming threshhold of WHO
PDM = PD
PDM[which(PDM<log10(88))] = NaN

log10(rowMeans(10^PD,na.rm=T))

plot(h,log10(rowMeans(10^PD,na.rm=T)),ylim=c(3.5,4.4),xlab="Force of Infection (h)",ylab="Parasite Density (count per cmm)", main="Parasite Density Given FOI, Without Adaptive Immunity")
points(h,log10(rowMeans(10^PDM,na.rm=T)),col="blue")

#lm(rowMeans(10^PD,na.rm=T)~h)
#lines(h,15065+801.7*h)
#lines(h,15342.6+631.5*h)


pars = c(15000,1)
data = rowMeans(10^PD,na.rm=T)
dataM = rowMeans(10^PDM,na.rm=T)
nlm = nls(data~A*(1-B*exp(-L*h)),start=list(A=1500,L=1,B=1000))
nlmM = nls(dataM~A*(1-B*exp(-L*h)),start=list(A=1500,L=1,B=1000))

plot(h,log10(rowMeans(10^PD,na.rm=T)),ylim=c(3,4.4),xlab="Force of Infection (h)",ylab="Parasite Density (count per cmm)", main="Parasite Density Given FOI, Without Adaptive Immunity")
A = 19900
B = .8354
L = .2819
lines(h,log10(A*(1-B*exp(-L*h))))

abline(h=log10(AM*(1-BM*exp(-LM))),col="red")
abline(h=log10(AM*(1-BM*exp(-LM*1.5))),col="blue")
abline(h=log10(AM*(1-BM*exp(-LM*.5))))

points(h,log10(rowMeans(10^PDM,na.rm=T)),col="blue",ylim=c(3,4.4))
AM = 19960
BM = .6391
LM = .4046
lines(h,log10(AM*(1-BM*exp(-LM*h))),col="blue")

points(c(.5,1,1.5),c(log10(mean(Age2PD1(1,3),na.rm=T)),log10(mean(Age2PD2(1,3),na.rm=T)),log10(mean(Age2PD3(1,3),na.rm=T))),pch=19,col="red")
#points(c(.5,1,1.5),c(log10(mean(Age2PD1(9,10),na.rm=T)),log10(mean(Age2PD2(9,10),na.rm=T)),log10(mean(Age2PD3(9,10),na.rm=T))),pch=19,col="green")

#gg = log10(c(mean(Age2PD1(1,3),na.rm=T),mean(Age2PD2(1,3),na.rm=T),mean(Age2PD3(1,3),na.rm=T)))
#gg10 = c(log10(mean(Age2PD1(9,10),na.rm=T)),log10(mean(Age2PD2(9,10),na.rm=T)),log10(mean(Age2PD3(9,10),na.rm=T)))

#lines(h,log10(AM*(1-BM*exp(-LM*h)))-mean(gg-gg10),col="green")

qqnorm((A*(1-B*exp(-L*h))-data)/sqrt(var(A*(1-B*exp(-L*h))-data)))
lines(seq(-3,3,.1),seq(-3,3,.1))


#aa = 13153
#bb = 2830
#cc = -11647
#dd = 5632
#LL = 1.744
#xx = seq(0,20,.01)
#lines(xx,(aa*xx+bb)*(1-exp(-LL*xx))+cc*xx+dd)

#write.csv2(PD,file="PDG_Sim_PD_h_nAI.csv",row.names=F)

PR = rep(0,length(h))
for(i in 1:length(h)){
  PR[i] = sum(PD[i,]>0,na.rm=T)/length(PD[i,])
}
plot(h,PR)
plot(h,PR/(1-PR))
lm(PR/(1-PR)~h+0)
tt = seq(0,10,.1)
lines(tt,tt*.0475)

