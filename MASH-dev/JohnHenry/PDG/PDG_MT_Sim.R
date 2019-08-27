######################################################################
########### This code creates an artificial malaria    ###############
########### therapy dataset; 334 individual infections ###############
###########          updated fortnightly               ###############
######################################################################

rowVars = function (x,na.rm = TRUE) 
{
  sqr = function(x) x * x
  n = rowSums(!is.na(x))
  n[n <= 1] = NA
  return(rowSums(sqr(x - rowMeans(x,na.rm = na.rm)), na.rm = na.rm)/(n - 1))
}

source('PDG.R')

## run for max of 30 fortnights for 334 patients

## asexual densities
MPDG = matrix(rep(0,30*334),nrow=30)
## gametocytes
GPDG = matrix(rep(0,30*334),nrow=30)
## probability of fever
FPDG = matrix(rep(0,30*334),nrow=30)
## TE
TEPDG = matrix(rep(0,30*334),nrow=30)


for(i in 1:334){
  ## create human
  human = PDGHuman$new()
  ## single inocculation
  human$infect_Human(1)
  ## how much to age human per time step; irrelevant for this
  dt = 1/26
  Nfortnights = 30
  ## updating step
  for(t in seq(1:Nfortnights)){
    human$update_Human(dt=dt)
  }
  
  MPDG[,i] = human$get_history()$Pt
  GPDG[,i] = human$get_history()$Gt
  FPDG[,i] = human$get_history()$pFever
  TEPDG[,i] = human$get_history()$TE
}

## average
MPDG[which(MPDG<1)]=NaN
GPDG[which(GPDG<1)]=NaN
plot(log10(rowMeans(10^MPDG,na.rm=T)),ylim=c(0,4.5),xlab="fortnights",ylab="log10 Parasite Densities",main="Average Across 334 Patients")
points(log10(rowMeans(10^GPDG,na.rm=T)),col="red")

## example
plot(MPDG[,2],type="l",xlab="fortnights",ylab="log10 Parasite Densities",main="Example Trajectory")
points(GPDG[,2],col="red")

plot(FPDG[,2],xlab="fortnights",ylab="Probability of Fever",main="Example Fever",type="l")
plot(TEPDG[,2],xlab="fortnights",ylab="Transmission Probability",main="Example Tranmission Efficiency",type="l")

plot(log10(rowMeans(10^MPDG,na.rm=T)),log10(rowVars(10^MPDG,na.rm=T)),xlim=c(1,4.5))
PDG_mvpl = lm(log10(rowVars(10^MPDG,na.rm=T))~log10(rowMeans(10^MPDG,na.rm=T)))
pdgxx = seq(1,5,.01)
lines(pdgxx,PDG_mvpl$coefficients[1]+pdgxx*PDG_mvpl$coefficients[2])

FPDG[which(FPDG==0)]=NaN
plot(rowMeans(FPDG,na.rm=T),xlab="fortnights",ylab="Probability of Fever",main="Average Probability of Fever Across 334 Patients")

TEPDG[which(TEPDG==0)]=NaN
plot(c(0,rowMeans(TEPDG,na.rm=T)),xlab="fortnights",ylab="Probability of Transmission",main="Average Probability of Transmission Across 334 Patients")

