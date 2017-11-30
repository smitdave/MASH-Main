source ("PfLOME.R")
source ("Rx.R")
source ("PfLOME_Objects.R")
source ("tent_PfLOME.R")
source ("eventTimeSeries.R")
source ("ImmuneCounters.R")
source ("Setup.PfLOME.R")

NOISY = FALSE

age = 1:3650
bites = unique(sort(make.bites(70, 10, 1, 5, wt=wt, trend = .05)))
moi = 1+rnbinom(length(bites), mu=3, size = .3)
treat = -100 #unique(sort(sample(bites, 20) + 8))

doOneHuman = function(bites, moi, age, treat){
  HUMANS <<- list()
  HUMANS[[1]] <<- pathObject_HUMAN()
  HUMANS[[1]]$Pf <<- pathObject_PfLOME(1)

  HUMANS[[1]]$Pf$Rx$StartTreatment <<-treat
  HUMANS[[1]]$Pf$Rx$Drug <<- sample(c(1,2), length(treat), TRUE, c(3,1))
  add2Pedigree_PfLOME(1,ixh)

  for(t in age){
    if(t %in% bites){
      k = which(bites %in% t)
      #print(c(t=t, k=k, bitek=bites[k], moi=moi[k]))
      for(jj in 1:moi[k]){
        #print(c(k=k, moi=moi[k]))
        addClone_PfLOME(t,1,tentPAR(t,pfid))
      }
      #browser()
    }
    updateHuman_PfLOME(t,1)
#    if(t%%365 == 0) image2D(z=HUMANS[[1]]$Pf$History$ptypes) # plots 2d hist of type specific immune counters - can see "average" phenotype recognized
  }

  HUMANS[[1]]
}

############################ create human ################################
#human = doOneHuman(bites,moi,age,treat)

#plot(1:length(age)/365,human$Pf$History$Ptt,type="l",ylim=c(0,12))

#means = matrix(rep(0,3650*20),nrow=20)
#for(i in 1:20){
#  means[i,] = human[[i]]$Pf$History$Ptt
#}

#with immunity
#ptmeanimm = colMeans(means,na.rm=T)
#plot(1:3650/365,ptmeanimm,type="l")

#without immunity
#ptmean = colMeans(means,na.rm=T)
#plot(1:3650/365,ptmean,type="l")


#comparisons
#par(mfrow=c(1,2))
#plot(1:3650/365,ptmeanimm,type="l")
#plot(1:3650/365,ptmean,type="l")

plotHistory = function(human, bites, moi, age){with(human$Pf$History,{
  plot(age/365, Ptt, type = "l", ylim = c(-4,13), lwd =2)
  points(bites/365, bites*0, pch = 3, cex=sqrt(moi), col = "red")
  lines(c(0,age)/365, Gt, pch = 19, col = gray(.5), lwd=2, lty=2)

  lines(c(0,age)/365, PD, pch = 19, type="h", col = "purple")
  for(i in 1:length(wx)){
    lines(c(0,age)/365, BSx[,i]-2, col = "darkgreen")
  }
  segments(0,6, 10,6, col = grey(0.5))
  segments(0,11, 10,11, col = grey(0.5))
  segments(0,-2, 10,-2, col = grey(0.5))
  text(0,0, "PD", col = "purple", pos = 1)
  text(0,0, "Infections", col = "red", pos = 3)
  text(0,1, "Log10 Parasites", pos = 4)
  text(0.1,-2, "Immunity", col = "darkgreen", pos = 3)

#  lines(age/365, MOI/max(MOI)-2.5, pch = 19, col = "purple")
  lines(age/365, MOI1/max(MOI1)-3, pch = 19, col = "blue")
  segments(0,-3, 10,-3, col = grey(0.5))
  text(0.1,-3, paste("MOI, max=", max(MOI)), col = "blue", pos = 3)
#  lines(age/365, (MOI-MOI1)/max(MOI-MOI1)-3.5, pch = 19, col = "purple")
})}

#lines(c(0,age/365), human$Pf$History$Gt-3.5, pch = 19, col = "purple")

#####################################plot function####################
#plotHistory(human, bites, moi, age)

##years = age/365
#plot(years, human$History$RBC,"l",ylab='RBC (10^13)',ylim=c(0,4))
#abline(h=2.5/3)
#plot(human$History$Anemia)

#library('plot3D')
#image2D(human$Pf$TypeImmunity)
#image2D(human$Pf$History$ptypes)

#plot(age/365,log10(10^(human$Pf$History$HRP2)/(5*10^6)),"l",ylab="HRP2 (ng/uL)",xlab="years",ylim=c(-5,3))
#abline(h = log10(.00011))
#detectable = rep(0,length(age))
#pos = which(log10(10^(human$Pf$History$HRP2)/(5*10^6)) > log10(.00011))
#detectable[pos] = 1

#par(mfrow=c(2,1))
#plot(age/365,detectable)
#plot(age/365,human$Pf$History$Ptt,type="l")
