######################### Now play with PRISM data, specifically


#PRISM_Temp = read_dta("PRISM cohort study expanded database through June 30th 2016 FINAL.dta")
#write.csv(PRISM_Temp,"PRISM_Database.csv")
PRISMDB = read.csv("PRISM_Database.csv")

plot(PRISMDB$age[which(PRISMDB$age<10)],log10(PRISMDB$parasitedensity[which(PRISMDB$age<10)]))

## separate by site; site 1 is Jinja (L), site 2 is Kihihi (M), site 3 is Nagongera (H)

PRISM1PD = PRISMDB$parasitedensity[PRISMDB$siteid==1]
PRISM1Age = PRISMDB$age[PRISMDB$siteid==1]
PRISM2PD = PRISMDB$parasitedensity[PRISMDB$siteid==2]
PRISM2Age = PRISMDB$age[PRISMDB$siteid==2]
PRISM3PD = PRISMDB$parasitedensity[PRISMDB$siteid==3]
PRISM3Age = PRISMDB$age[PRISMDB$siteid==3]

plot(PRISM1Age,log10(PRISM1PD))
plot(PRISM2Age,log10(PRISM2PD))
plot(PRISM3Age,log10(PRISM3PD))

plot(PRISM1Age[which(PRISM1Age<10)],log10(PRISM1PD[which(PRISM1Age<10)]))
plot(PRISM2Age[which(PRISM2Age<10)],log10(PRISM2PD[which(PRISM2Age<10)]))
plot(PRISM3Age[which(PRISM3Age<10)],log10(PRISM3PD[which(PRISM3Age<10)]))

PRISM = list(PD1=PRISM1PD, PD2=PRISM2PD, PD3=PRISM3PD, Age1 = PRISM1Age, Age2 = PRISM2Age, Age3 = PRISM3Age)


#############################
## site 1
#############################

## Children

Age2PD1 = function(alow,ahigh){
  temp = which(PRISM$Age1>alow & PRISM$Age1<ahigh)
  return(PRISM$PD1[temp])
}

PDmu1 = rep(0,20)
for(i in 1:20){
  alow = (i-1)/2
  ahigh = i/2
  PDmu1[i] = log10(mean(Age2PD1(alow,ahigh),na.rm=T))
}

PDmu11 = rep(0,10)
for(i in 1:10){
  alow = (i-1)
  ahigh = i
  PDmu11[i] = log10(mean(Age2PD1(alow,ahigh),na.rm=T))
}

x = seq(0,99)*10/100
plot(x,PDmu1,type="l",main="Walukuba, Jinja District")


## Adults


APDmu1 = rep(0,30)
for(i in 1:30){
  alow = (i-1)+20
  ahigh = i+20
  APDmu1[i] = mean(Age2PD1(alow,ahigh),na.rm=T)
}
plot(1:30+20,log10(APDmu1),ylim=c(0,3.5),xlab="Age",ylab="Expected log10 Parasite Densities (count per cmm)",main="Walukuba, Jinja District")

APDmumu1 = log10(mean(Age2PD1(20,100),na.rm=T))
abline(h=APDmumu1)

##################################
## site 2
##################################

Age2PD2 = function(alow,ahigh){
  temp = which(PRISM$Age2>alow & PRISM$Age2<ahigh)
  return(PRISM$PD2[temp])
}

PDmu2 = rep(0,20)
for(i in 1:20){
  alow = (i-1)/2
  ahigh = i/2
  PDmu2[i] = log10(mean(Age2PD2(alow,ahigh),na.rm=T))
}

PDmu22 = rep(0,10)
for(i in 1:10){
  alow = i-1
  ahigh = i
  PDmu22[i] = log10(mean(Age2PD2(alow,ahigh),na.rm=T))
}


x = seq(0,99)*10/100
plot(x,PDmu2,type="l", main="Kihihi, Kanungu District")

## Adults


APDmu2 = rep(0,30)
for(i in 1:30){
  alow = (i-1)+20
  ahigh = i+20
  APDmu2[i] = mean(Age2PD2(alow,ahigh),na.rm=T)
}
plot(1:30+20,log10(APDmu2),ylim=c(0,3.5),xlab="Age",ylab="Expected log10 Parasite Densities (count per cmm)",main="Kihihi, Kanungu District")

APDmumu2 = log10(mean(Age2PD2(20,100),na.rm=T))
abline(h=APDmumu2)

#############################
## site 3
#############################

Age2PD3 = function(alow,ahigh){
  temp = which(PRISM$Age3>alow & PRISM$Age3<ahigh)
  return(PRISM$PD3[temp])
}

PDmu3 = rep(0,20)
for(i in 1:20){
  alow = (i-1)/2
  ahigh = i/2
  PDmu3[i] = log10(mean(Age2PD3(alow,ahigh),na.rm=T))
}

PDmu33 = rep(0,10)
for(i in 1:10){
  alow = i-1
  ahigh = i
  PDmu33[i] = log10(mean(Age2PD3(alow,ahigh),na.rm=T))
}

x = seq(0,99)*10/100
plot(x,PDmu3,type="l",main="Nagongera, Tororo District")


## Adults

APDmu3 = rep(0,30)
for(i in 1:30){
  alow = (i-1)+20
  ahigh = i+20
  APDmu3[i] = mean(Age2PD3(alow,ahigh),na.rm=T)
}
plot(1:30+20,log10(APDmu3),ylim=c(0,3.5),xlab="Age",ylab="Expected log10 Parasite Densities (count per cmm)",main="Nagongera, Tororo District")

APDmumu3 = log10(mean(Age2PD3(20,100),na.rm=T))
abline(h=APDmumu3)

######## Compare across sites

x = seq(1/4,10-1/4,1/2)
plot(x,PDmu2,ylim=c(3,4.5),main="Rolling Average of Parasitemia Across Study Sites",type="l",col="red",xlab="Age (Years)",ylab="Average log10 Parasite Density")
lines(x,PDmu3,col="blue")
#lines(x,PDmu1,lty=3)
lines(x,PDmu1)


xx = seq(.5,9.5,1)
plot(xx,PDmu22,ylim=c(3,4.5),main="Average of Parasitemia Across Study Sites by Age",type="l",col="red",xlab="Age (Years)",ylab="Average log10 Parasite Density")
lines(xx,PDmu33,col="blue")
lines(xx,PDmu11)

abline(h=log10(mean(Age2PD2(0,3),na.rm=T)),lty=2,col="red")
abline(h=log10(mean(Age2PD3(0,3),na.rm=T)),lty=2,col="blue")
abline(h=log10(mean(Age2PD1(0,3),na.rm=T)),lty=2)

vioplot(log10(PRISM$PD1[which(PRISM$PD1>0)]),log10(PRISM$PD2[which(PRISM$PD2>0)]),log10(PRISM$PD3[which(PRISM$PD3>0)]))


##### Fever prevalences to estimate antidisease immunity 
#ConfFev = which(PRISMDB$fever==1)
#

####################################
###  Fever Data
####################################

