
dt = 10/365
Ntimesteps = 365
Npeople = 1000
PD = matrix(0,nrow=Npeople,ncol=Ntimesteps)
pFever = PD

h = 1.45
for(i in 1:Npeople){
  human = PDGHuman$new(age=0)
  for(t in seq(1:Ntimesteps)){
    Pfnew = rpois(1,h*dt)
    if(Pfnew>0){human$infect_Human(Pfnew)}
    human$update_Human(dt=dt)
    if(rbinom(1,1,human$get_pFever())==1){
      human$clear_All_Pathogens()
    }
  }
  PD[i,] = human$get_history()$Pt
  pFever[i,] = human$get_history()$pFever
}

MPD = PD
## set threshold of detectability
MPD[which(PD<=log10(88))]=NaN
t = seq(dt,10,dt)
plot(t,log10(rowMeans(t(10^MPD),na.rm=T)),type="l",col="red",ylim=c(3.5,4.4))
lines(t,log10(rowMeans(t(10^PD),na.rm=T)),type="l")
abline(v=1.7)

Age2PDG = function(alow,ahigh){
  t = seq(dt,10,dt)
  tlow = which(t>alow)
  thigh = which(t<ahigh)
  log10(mean(rowMeans(t(10^MPD),na.rm=T)[intersect(tlow,thigh)]))
}

aa = seq(1/4,10-1/4,1/2)
MPDSmooth = rep(0,length(aa))
for(i in 1:length(aa)){
  MPDSmooth[i] = Age2PDG((i-1)/2,i/2)
}
plot(aa,MPDSmooth,type="l",ylim=c(3,4.5),xlab="Age (in Years)",ylab="Log10 Mean Parasite Density")
lines(x,PDmu3,col="blue")
abline(v=1.75)

plot(aa,(10^PDmu3)/(10^MPDSmooth),type="l",ylim=c(0,1))
plot(aa[4:length(aa)],(10^PDmu3[4:length(aa)])/(10^MPDSmooth[4:length(aa)]),type="l",ylim=c(0,1))
