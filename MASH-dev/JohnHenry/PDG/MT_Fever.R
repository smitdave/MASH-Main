fever <- read_excel("~/GitHub/MASH-Main/MASH-dev/JohnHenry/PDG/Fever.xlsx")
fever = as.matrix(fever)
dots = which(fever=='.')
fever[dots] = NaN
fever=as.numeric(fever)

dur = c(56,324,401,72,22,31,50,231,736,40,590,50,47,220,38,531,172,12,442,431,527,693,419,
        154,63,432,526,13,139,93,123,445,130,471,14,34,421,425,401,315,186,375,135,127,324,
        289,32,481,61,28,211,174,190,31,61,440,476,430,48,254,53,98,176,78,115,24,342,376,
        370,32,234,312,13,458,52,99,114,176,160,280,40,257,588,498,638,330,638,497,506,405,
        38,72,159,536,234,47,405,159,144,60,469,239,442,68,71,303,192,400,261,140,135,180,
        181,332,50,251,84,240,175,50,199,80,124,127,12,320,18,120,67,261,522,543,119,409,
        414,732,396,366,483,503,584,498,60,187,425,433,80,527,209,50,1327,43,43,42,18,106,
        50,45,10,81,36,39,120,66,41,7,6,43,45,58,109,54,113,125,199,9,12,32,71,65,95,108,
        50,43,77,41,209,80,47,231,88,178,26,33,84,49,146,56,70,85,80,176,48,178,18,120,46,
        131,67,74,49,84,57,45,243,102,41,195,173,56,30,143,41,167,145,105,46,108,64,117,45,
        343,137,51,111,71,102,240,160,201,96,160,211,130,160,50,227,118,158,170,57,131,
        177,115,46,62,103,101,52,215,39,106,35,141,99,89,188,108,107,107,160,67,83,107,86,
        75,36,92,162,139,126,152,138,11,93,85,47,152,131,152,35,106,108,17,31,115,103,37,93,
        89,45,80,57,83,110,33,15,77,80,57,66,45,47,114,93,35,22,96,44,33,104,32,10,113,6,10,
        10,34,71,45,80,8,94,105)

durdur = cumsum(dur)

Fever = matrix(NaN,ncol=334,nrow=1327)
Fever[1:dur[1],1] = fever[1:durdur[1]]
for(i in 2:334){
  Fever[1:dur[i],i] = fever[(durdur[i-1]+1):durdur[i]]
}

BinaryFever = (Fever>37)
PropFever = rowMeans(BinaryFever,na.rm=T)
plot(PropFever[1:365],type="l",ylim=c(0,1),xlab="Days Since Patency", ylab="Proportion with Fever",main="Daily Prevalence of Fever Conditioned on Infection")

plot(log10(rm[1:365]),type="l",ylim=c(0,5))
lines(PropFever[1:365])
ccf(PropFever[1:200],log10(rm[1:200]))
cor(PropFever[1:200],log10(rm[1:200]))
plot(log10(rm[1:200]),PropFever[1:200])
plot(log10(rm[1:200]),PropFever[1:200]/(1-PropFever[1:200]),xlab="Log10 Mean Asexual Densities",ylab="Odds Ratio of Fever",main="Odds Ratio of Fever for Given Mean Parasite Density")

plot(log10(rm[1:200]),log10(PropFever[1:200]/(1-PropFever[1:200])),xlab="Log10 Mean Asexual Density", ylab="log10 Odds Ratio of Fever", main="Log Odds Ratio of Fever for Given Mean Parasite Density")

logodd = log10(PropFever[1:200]/(1-PropFever[1:200]))
logrm = log10(rm[1:200])

logodd[which(is.infinite(logodd))]=NaN
oddfit = lm(logodd~logrm)
lines(seq(2,4.5,.01),-2.8475+.8762*seq(2,4.5,.01))
