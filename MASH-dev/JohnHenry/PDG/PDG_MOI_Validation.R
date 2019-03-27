########################## Testing PDG Superinfection
########################## To test that simulated superinfections are statistically consistent
########################## with the assumption of independent infection dynamics, we first add
########################## each independent trajectory pairwise to create data consistent with
########################## this assumption. Then, simulate MOI=2 infections. Compare histograms
########################## of output and related summary statistics


########################## Part 1: pairwise additive data.

MT_PT_NP = read_excel("~/GitHub/MASH-Main/MASH-Main/MASH-dev/JohnHenry/PDG/MT_PT_NP.xlsx")
M = as.matrix(MT_PT_NP)
PtData = matrix(0,nrow=52,ncol=334)

for(j in 1:334){
  for(i in 1:52){
    PtData[i,j] = mean(M[((i-1)*14+1):(i*14),j],na.rm=T)
  }
}

hist(log10(PtData[1,]))

PtPaired = matrix(0,nrow=52,ncol=334*335/2)
for(i in 1:334){
  for(j in 1:i)
  PtPaired[,i+j-1] = PtData[,i]+PtData[,j]
}

hist(log10(PtPaired[9,]),freq=F,ylim=c(0,1.5),xlim=c(0,6),breaks=10)


######################### Part 2: simulate synthetic data of MOI=2

P = 100
Years = 2
Fortnights = Years*26
Pt = matrix(0,nrow=Fortnights,ncol=P)

for(j in 1:P){
  
  human = PDGHuman$new()
  human$infect_Human(2)
  
  Pt[,j] = human$get_history()$Pt
  
}

