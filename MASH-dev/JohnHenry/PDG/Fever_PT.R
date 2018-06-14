FeverV = as.matrix(Fever)
FeverV[which(FeverV=="."),1]=NaN
FeverV = as.numeric(FeverV)

FeverM = matrix(0,nrow=1400,ncol=334)
for(i in 1:334){
  FeverM[1:(end[i]-begin[i]+1),i] = FeverV[begin[i]:end[i]]
}

Fevermu = rowMeans(FeverM>0,na.rm=T)
plot(Fevermu,type="l",xlim=c(0,365),ylim=c(0,1),col="red")
lines(log10(PTmu)/max(log10(PTmu)))

max(which(Fevermu>.5))
