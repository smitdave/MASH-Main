PRISM <- read.csv('PRISM_Data.csv',header=TRUE)
library('fitdistrplus')
library('vioplot')

## Parasite Density Detected
PD = PRISM$parsdens
Age = PRISM$age

plot(Age,log10(PD))

PDAge = list()
for(i in 1:9){
  PDAge[[i]] = log10(PD[which(Age>=i & Age<(i+1))])
  PDAge[[i]] = PDAge[[i]][which(is.finite(PDAge[[i]]))]
}

vioplot(PDAge[[1]],PDAge[[2]],PDAge[[3]],PDAge[[4]],PDAge[[5]],PDAge[[6]],PDAge[[7]],PDAge[[8]],PDAge[[9]],xlab="Age Category",ylab="log10 Parasite Density")

Weight = PRISM$weight
PDAge_Weight = list()
for(i in 1:9){
  PDAge_Weight[[i]] = log10(PD[which(Age>=i & Age<(i+1) & PD>0)])/Weight[which(Age>=i & Age<(i+1) & PD>0)]
}

vioplot(PDAge_Weight[[1]],PDAge_Weight[[2]],PDAge_Weight[[3]],PDAge_Weight[[4]],PDAge_Weight[[5]],PDAge_Weight[[6]],PDAge_Weight[[7]],PDAge_Weight[[8]],PDAge_Weight[[9]])


fPDAge = function(alow,ahigh){
  temp = PD[which(Age>=alow & Age<ahigh)]
  tempPat = temp[which(temp>0)]
  return(tempPat)
}
