library(shiny)
library(shinythemes)
library(matrixStats)
library(plotly)
library(ggplot2)
library(reshape2)
source("PfLOME.R")
source ("tent_PfLOME.R")
source("ImmuneCounters.R")
source("Rx.R")
source("showHistory.R")


ui = fluidPage(
  
  titlePanel("Primaquine Efficacy"),
  
  ######################### PfTent Tab ###########################
  tabsetPanel(
    
    tabPanel("PfTent",
             
             #             radioButtons(inputId = "shape",
             #                          label = "Log Parasite Density Shape",
             #                          choices = c("Tent" = "tent",
             #                                      "Red Tent" = "rtent")),
             pageWithSidebar(
               
               headerPanel("Pf Profile"),
               sidebarPanel(
                 
                 selectInput("whichPar","Choose a Parameter",
                             c("Initial iRBC","Time of Peak","Peak","End of Infection","Pf Profile")),
                 
                 ##Initial infection
                 uiOutput("Mz0"),
                 uiOutput("muMz0"),
                 uiOutput("sigmaMz0"),
                 
                 ##peak of infection
                 uiOutput("peakt"),
                 uiOutput("muPeakt"),
                 uiOutput("sigmaPeakt"),
                 
                 ##length of infection
                 uiOutput("tfin"),
                 uiOutput("muTfin"),
                 
                 ##maximum log10 parasite burden
                 uiOutput("maxPD"),
                 uiOutput("muMaxPD"),
                 uiOutput("sigmaMaxPD")
               ),
               
               mainPanel(
                 
                 plotOutput("parPlot")
                 
               )
             )
    ),
    
    ############################# Gametocyte Tab ####################################
    
    tabPanel("Gametocytes",
             pageWithSidebar(
               
               headerPanel("Gametocytes"),
               
               sidebarPanel(
                 ##Initial infection
                 uiOutput("ddays"),
                 uiOutput("gbk"),
                 uiOutput("delay")
               ),
               
               mainPanel(
                 plotOutput("GtPlot")
               )
             )
    ),
    
    
    ######################## Fever Tab ################################
    
    tabPanel("Fever",
             
             pageWithSidebar(
               
               headerPanel("Fever Calibration"),
               
               sidebarPanel(
                 ##Initial infection
                 uiOutput("fever"),
                 uiOutput("fevThresh"),
                 uiOutput("maxFever"),
                 uiOutput("halfFever"),
                 uiOutput("rateFever")
               ),
               
               mainPanel(
                 plotOutput("feverPlot")
               )
             )
    ),
    
    ################## Medications Tab ##################
    
    tabPanel("Rx",
             
             pageWithSidebar(
               
               headerPanel("PD Effect"),
               
               sidebarPanel(
#                 uiOutput("whichDrug"),
#                 uiOutput("PD"),
                 uiOutput("timeTreated")
               ),
               
               mainPanel(
                 plotOutput("RxPlot")
               )
               
             )
             
    ),

#################### Gametocyte to Transmissibility #####################

#      tabPanel("Transmission",
         
#              pageWithSidebar(
           
#                headerPanel("Gametocyte Density to Transmissibility"),
           
#                sidebarPanel(
#                  uiOutput("halfSat"),
#                  uiOutput("slope"),
#                  uiOutput("atMax")
#                ),
           
#              mainPanel(
#                plotOutput("RxPlot")
#              )
           
#          )
         
#      ),
    
    ##################### simulation tab ##############################3
    
    tabPanel("Simulation",
             pageWithSidebar(
               
               ## simulate with given parameters - options: no treatment, ACT, P+ACT
               ## OR just P+ACT vs not, show tau_p/tau over time & tau_mu
               headerPanel("Simulation"),
               
               sidebarPanel(
                 uiOutput("Coverage"),
                 actionButton("runPQ", "Run Simulation")
               ),
               
               mainPanel(
                 plotOutput("plotSim")
               )
             )
    )
  )
)

server = function(input,output){
  
  ################################ PfTent Server ############################
  
  
  output$Mz0 = renderUI({
    if(input$whichPar == "Initial iRBC"){
      selectInput("Mz0","Distribution",c("Normal" = "norm","Exponential" = "exp"))
    }
  })
  
  ##update expected initial Mz population
  output$muMz0 = renderUI({
    if(input$whichPar == "Initial iRBC"){
      if(input$Mz0=="norm" | input$Mz0=="exp") {
        numericInput("muMz0","Enter the mean numer of initial iRBC",value=4.2)
      }
    }
  })
  
  ##update sd of initial Mz population
  output$sigmaMz0 = renderUI({
    if(input$whichPar == "Initial iRBC"){
      if(input$Mz0=="norm") {
        numericInput("sigmaMz0","Enter the SD of the number of initial iRBC",value=.1)
      }
    }
  })
  
  output$peakt = renderUI({
    if(input$whichPar == "Time of Peak"){
      selectInput("peakt","Distribution",c("Log Normal" = "lnorm","Normal" = "norm"))
    }
  })
  
  ##update expected time of peak Mz burden
  output$muPeakt = renderUI({
    if(input$whichPar == "Time of Peak"){
      if(input$peakt=="lnorm"|input$peakt=="norm") {
        numericInput("muPeakt","Enter the Mean log Time Past 18 Days of the Peak Number of iRBC",value=1.1)
      }
    }
  })
  
  ##update sd of time of peak Mz burden
  output$sigmaPeakt = renderUI({
    if(input$whichPar == "Time of Peak"){
      if(input$peakt=="lnorm"|input$peakt=="norm") {
        numericInput("sigmaPeakt","Enter the SD of the Time of the Peak Number of iRBC",value=.5)
      }
    }
  })
  
  output$tfin = renderUI({
    if(input$whichPar == "End of Infection"){
      selectInput("tfin","Distribution",c("Geometric" = "geom"))
    }
  })
  
  ##update expected length of infection
  output$muTfin = renderUI({
    if(input$whichPar == "End of Infection"){
      if(input$tfin=="geom") {
        numericInput("muTfin","Enter the Mean Length of Infection (in days)",value=200)
      }
    }
  })
  
  output$maxPD = renderUI({
    if(input$whichPar == "Peak"){
      selectInput("maxPD","Distribution",c("Normal" = "norm"))
    }
  })
  
  ##update expected maximum Mz burden
  output$muMaxPD = renderUI({
    if(input$whichPar == "Peak"){
      if(input$maxPD=="norm"){
        numericInput("muMaxPD","Enter Mean log10 Peak Number of iRBC",value=10.5)
      }
    }
  })
  
  ##update sd of maximum Mz burden
  output$sigmaMaxPD = renderUI({
    if(input$whichPar == "Peak"){
      if(input$maxPD=="norm"){
        numericInput("sigmaMaxPD","Enter the SD of the log10 Peak Number of iRBC",value=.5)
      }
    }
  })
  
  ##update distribution of initial Mz burden
  output$parPlot = renderPlot({
    
    if(input$whichPar == "Initial iRBC"){
      m = input$muMz0
      if(input$Mz0=="norm"){
        s = input$sigmaMz0
        x = seq(-3*s,3*s,.1*s)+m
        plot(x,dnorm(x,m,s),type="l",xlab="log10 iRBC", ylab="Probability", main="Distribution of Initial iRBC")
      }
      if(input$Mz0=="exp"){
        x = seq(0,5*m,.1)
        plot(x,dexp(x,1/m),type="l",xlab="Initial log10 iRBC",ylab="probability")
      }
    }
    
    if(input$whichPar == "Time of Peak"){
      s = input$sigmaPeakt
      m = input$muPeakt
      if(input$peakt=="lnorm"){
        x = seq(.1,5*m,.1)
        plot(x,dlnorm(x,m,s),type="l",xlab = "Time (in Days)",ylab="Probability",main="Distribution of Time of Peak Number of iRBC")
      }
      if(input$peakt=="norm"){
        x = seq(-3*s,3*s,.1*s)+m
        plot(x,dnorm(x,m,s),type="l",xlab = "Time (in Days)",ylab="Probability")
      }
    }
    
    if(input$whichPar == "Peak"){
      s = input$sigmaMaxPD
      m = input$muMaxPD
      if(input$maxPD=="norm"){
        x = seq(-3*s,3*s,.1*s)+m
        plot(x,dnorm(x,m,s),type="l",xlab="Peak log10 Number of iRBC", ylab = "Probability", main="Distribution of Peak log10 iRBC")
      }
    }
    
    if(input$whichPar == "End of Infection"){
      m = input$muTfin
      x = seq(0,1.5*m,1)
      plot(x,dgeom(x,1/m),type="l",xlab="Length of Infection (in Days)",ylab="Probability",main="Distribution of Length of Infection")
    }
    
    if(input$whichPar == "Pf Profile"){
      title = "Log Parasite Density Shape"
      Mz0   = switch(input$Mz0,
                     norm = rnorm,
                     exp = rexp,
                     rnorm)
      peakt = switch(input$peakt,
                     lnorm = rlnorm,
                     norm = rnorm,
                     rlnorm)
      tfin  = switch(input$tfin,
                     geom = rgeom,
                     rgeom)
      maxPD = switch(input$maxPD,
                     norm = rnorm,
                     rnorm)
      
      ##plotting infection profile - include mean, 20th, 50th, and 80th percentiles
      plotTent = function(n,Mz0,muMz0,sigmaMz0,peakt,muPeakt,sigmaPeakt,tfin,muTfin,maxPD,muMaxPD,sigmaMaxPD){
        x = (-3):300
        M = matrix(0,nrow=n,ncol=(4+length(x)))
        for(i in 1:n){
          if(input$Mz0=="norm"){
            M[i,1:4] = c(Mz0(1,muMz0,sigmaMz0),peakt(1,muPeakt,sigmaPeakt),tfin(1,1/muTfin),maxPD(1,muMaxPD,sigmaMaxPD))
          }
          if(input$Mz0=="exp"){
            M[i,1:4] = c(Mz0(1,muMz0),peakt(1,muPeakt,sigmaPeakt),tfin(1,1/muTfin),maxPD(1,muMaxPD,sigmaMaxPD))
          }
          
          #         Full dPdt_tent function
          #          M[i,2] = M[i,2]+18
          #          M[i,8] = M[i,1]
          #          params = list(age=0,t0=0,mxPD=M[i,4],peakD=M[i,2],gr=M[i,4]/M[i,2],dr=M[i,4]/M[i,3],tEnd=M[i,3],NOISY=F)
          #          for(j in 9:length(x)){
          #            M[i,j] = dPdt_tent(j-9,M[i,j-1],params)
          #            if(is.na(M[i,j])){
          #              M[i,j]=0
          #              }
          #          }
          
          #       Simple tent function
          M[i,5:(length(x)+4)] = tent(x,M[i,1],M[i,4],M[i,2],M[i,3])
          
        }
        write.csv(M, "MzData.txt")
        mutent = colMeans(M[1:n,5:(length(x)+4)])
        quanttent = colQuantiles(M[1:n,5:(length(x)+4)],probs=seq(0, 1, 0.1))
        #          plot(x,mutent,type="l",xlim=c(-3,300),ylim=c(0,12),ylab="log10 Parasite Densities",xlab="Time (in Days)",main="Infection Profile - log10 iRBC")
        plot(x,quanttent[,3], lty=2,type="l",xlim=c(-3,300),col="blue",ylim=c(0,12),ylab="log10 Parasite Densities",xlab="Time (in Days)",main="Infection Profile - log10 iRBC")
        lines(x,quanttent[,6])
        lines(x,quanttent[,9], lty=2, col="red")
        legend(200, 12, legend=c("20th Percentile", "Median", "80th Percentile"),
               col=c("blue","black","red"), lty=c(2,1,2), cex=1.2)
      }
      
      plotTent(1000,Mz0,input$muMz0,input$sigmaMz0,peakt,input$muPeakt,input$sigmaPeakt,tfin,input$muTfin,maxPD,input$muMaxPD,input$sigmaMaxPD)
      #        if(input$display == "parmean") {
      #          x = (-3):(1*(300))/1
      #          plot(x,tent(x,input$muMz0,input$muMaxPD,input$muPeakt,input$muTfin),type="l",xlab="Time (in Days)", ylab="log10 iRBC", main="Infection Profile - log10")
      #        }
    }
  })
  
  
  ########################### GAMETOCYTE Server ################################
  
  ##gametocyte lifespan
  output$ddays = renderUI({
    sliderInput("ddays","Enter the Average Gametocyte Lifespan (in Days)",value=6,min=1,max=15,step=.5)
  })
  
  ##gametocyte birth rate
  output$gbk = renderUI({
    sliderInput("gbk","Enter the Gametocyte Birth Rate",value=.01,min=.001,max=.05,step=.001)
  })
  
  ##time for gametocyte maturation
  output$delay = renderUI({
    sliderInput("delay","Enter the Lag (in Days) Between iRBC and Gametocytogenesis",value=10,min=0,max=20)
  })
  
  
  ##plot gametocytes
  output$GtPlot = renderPlot({
    M = read.csv("MzData.txt")
    M = as.matrix(M[,2:ncol(M)])
    G = M[,5:ncol(M)]*0
    Del = matrix(rep(0,nrow(M)*input$delay),nrow=nrow(M))
    G = cbind(Del,G)
    gdk = log(2)/input$ddays
    for(i in (input$delay+8):ncol(G)){
      G[,i-3] = pmax(log10(10^(G[,i-4]-gdk)+10^(M[,i-(input$delay)]+log10(input$gbk))),0)
    }
    write.csv(G,"GtData.txt")
    muGam = colMeans(G)
    quantGam = colQuantiles(G,probs=seq(0, 1, 0.1))
    x = (-3):(300+input$delay)
    
    ##update Gt profile plot
    plot(x,quantGam[,3],col="blue",lty=2,type="l",xlim=c(-3,300+input$delay),ylim=c(0,10),ylab="log10 Gametocyte iRBC",xlab="Time (in Days)",main="Infection Profile - log10 Gametocyte Burden")
    lines(x,quantGam[,6])
    lines(x,quantGam[,9],lty=2, col="red")
    legend(200, 10, legend=c("20th Percentile", "Median", "80th Percentile"),
           col=c("blue", "black","red"), lty=c(2,1,2), cex=1.2)
  })
  
  
  ############################# Fever Server ######################################
  
  output$fever = renderUI({
    selectInput("fever","Implement Simple Fever or Graded Fever?",c("Simple","Graded"))
  })
  
  output$fevThresh = renderUI({
    if(input$fever == "Simple"){
      sliderInput("fevThresh","Enter the cutoff % Infected RBC Associated with Fever",value=1,min=.01,max=5,step=.01)
    }
  })
  
  output$maxFever = renderUI({
    if(input$fever == "Graded"){
      sliderInput("maxFever","Enter the Maximum Fever Possible (deg C)",value=41,min=37,max=42,step=.1)
    }
  })
  
  output$halfFever = renderUI({
    if(input$fever == "Graded"){
      sliderInput("halfFever","Enter the % Infected RBC Associated with Moderate Fever",value=2,min=.1,max=5,step=.1)
    }
  })
  
  output$rateFever = renderUI({
    if(input$fever == "Graded"){
      sliderInput("rateFever","Steepness Parameter",value=3,min=.1,max=4,step=.1)
    }
  })
  
  
  
  output$feverPlot = renderPlot({
    M = read.csv("Mzdata.txt")
    M = as.matrix(M[,2:ncol(M)])
    MQuants = colQuantiles(M[,5:ncol(M)],probs=seq(0, 1, 0.1))
    Mmed = MQuants[,6]
    Fev = rep(0,length(Mmed))
    thresh = log10(input$fevThresh/100*10^6*5*10^3)
    if(input$fever == "Simple"){
      for(i in 1:length(Mmed)){
        Fev[i] = ifelse(Mmed[i]>=thresh,1,0)
      }
      plot(-3:(length(Fev)-4),Fev,type="l",xlab="time (days)")
      par(new=T)
      plot(-3:(length(Fev)-4),Mmed,axes=F,type="l",lty=2, xlab="time (days)")
      axis(side=4)
    }
    half = log10(input$halfFever/100*10^6*5*10^3)
    rate = input$rateFever
    if(input$fever == "Graded"){
      Fev = 37+(input$maxFever-37)*sigmoidX(pmax(Mmed,0),half,rate,Inf)
      plot(-3:(length(Fev)-4),Fev,ylim=c(37,42),type="l", xlab = "time (days)")
      par(new=T)
      plot(-3:(length(Fev)-4),Mmed,axes=F,type="l",lty=2)
      axis(side=4)
    }
    
  })
  
  
  ############################# Rx Server ##################################
  
#  output$whichDrug = renderUI({
#    selectInput("whichDrug","Which Drug Would You Like to Callibrate?",c("ACT","Primaquine+ACT"))
#  })
  
#  output$PD = renderUI({
#    sliderInput("PD","What is the pharmakodynamic killing effect?",value=4,min=.1,max=5,step=.1)
#  })
  
  output$timeTreated = renderUI({
    sliderInput("timeTreated","Time Treatment is Initiated",value=0,min=0,max=120)
  })
  
  
  output$RxPlot = renderPlot({
    
#    Therapy = ifelse(input$whichDrug == "ACT", 1,2)
    Rx = list(StartTreatment=(as.integer(input$timeTreated)), Drug=1)
    M = read.csv('MzData.txt')
    M = as.matrix(M[,2:ncol(M)])
    M = M[,5:ncol(M)]
    
    quantM = colQuantiles(M,probs=seq(0, 1, 0.1))
    Mmed = quantM[,6]
    peakD=which(Mmed==max(Mmed))-4
    mxPD = max(Mmed)
    m0 = Mmed[4]
    tEnd = max(which(Mmed>.01))-4
    params = list(age=0,t0=0,mxPD=mxPD,peakD=peakD,tEnd=tEnd,gr=(mxPD-m0)/peakD,dr=mxPD/(tEnd-peakD),NOISY=F)
    ##Gt params
    gdk = log(2)/6
    gbk = .01
    delay = 10
    Mpd = 0*Mmed
    G=0*Mmed
    PD = G
    Gp = G
    Gpp = G
    #calculate PD effects on M and G
    for(t in -3:295){
      PD[t+4] = getPD(t,Rx)
      Mpd[t+5] = dPdt_tent(t,Mpd[t+4],params,PD[t+4])
      if(t==0) {
        Mpd[t+5] = m0
      }
      if(is.na(Mpd[t+5])){
        Mpd[t+5] = 0
      }
      if(t > (delay)){
        G[t] = pmax(log10(10^(G[t-1]-gdk)+10^(Mpd[t-(delay)+4]+log10(gbk))),0)
        Gp[t] = ifelse(PD[t+4]==4, pmax(log10(10^(Gp[t-1]-gdk)+10^(Mpd[t-(delay)+4]+log10(gbk)))-PD[t+4],0),
                       pmax(log10(10^(Gp[t-1]-gdk)+10^(Mpd[t-(delay)+4]+log10(gbk))),0))
        Gpp[t] = pmax(log10(10^(Gpp[t-1]-gdk)+10^(Mpd[t-(delay)+4]+log10(gbk)))-PD[t+4],0)
        }
    }
    plot(-3:(length(Mpd)-4),Mpd,type="l",ylim=c(0,10), xlab="time (days)", ylab="log10 iRBC")
    lines(1:length(G),G,lty=2)
    lines(1:length(Gp),Gp,lty=2,col="blue")
    lines(1:length(Gpp),Gpp,lty=2,col="red")
    legend(140, 10, legend=c("Asexual Parasites", "Gametocytes with ACT", "Gametocytes with ACT+P", "Gametocytes with ACT+P+RM"),
          lty=c(1,2,2,2), col=c("black","black","blue","red"), cex=1.2)
    abline(v = input$timeTreated)
  })
  
  ######################### Gametocyte to Transmissibility Server ##########
  
  output$halfSat = renderUI({
    sliderInput("halfSat","Halfway to Max Transmissibility", value = 10, min = 1, max = 100)
  })
  
  output$slope = renderUI({
    sliderInput("slope", "Transition Slope", value = 10, min = 1, max = 100)
  })
  
  output$atMax = renderUI({
    sliderInput("atMax", "Point of Maximum Transmission", value = 10, min = 1, max = 100)
  })
  
  
  ######################### Simulation Server #############################
  
  output$Coverage = renderUI({
    sliderInput("Coverage","Treatment Coverage",value=1,min=0,max=1,step=.05)
  })
  
  observeEvent(input$runPQ, {
    Mmat = matrix(rep(0,100*304),nrow=100)
    Gmat = matrix(rep(0,100*304),nrow=100)
    Gpmat = matrix(rep(0,100*304),nrow=100)
    Gppmat = matrix(rep(0,100*304),nrow=100)
    p = input$Coverage
    diff = rep(0,100)
    diff2 = diff
    for(j in 1:100){
    treat = -100
    Rx <<- list(StartTreatment = treat, Drug = 1)
    Mpd = rep(0,304)
    G = Mpd
    Gp = rep(0, 304)
    Gpp = Gp
    PD = Gp
    m0 = rnorm(1,4.2,.1)
    peakD = rlnorm(1,1.1,.5)+18
    tEnd = rgeom(1,1/200)
    mxPD = rnorm(1,10.5,.5)
    delay = 10
    gdk = log(2)/6
    gbk = .01
    params = list(age=0,t0=0,mxPD=mxPD,peakD=peakD,tEnd=tEnd,gr=(mxPD-m0)/peakD,dr=mxPD/(tEnd-peakD),NOISY=F)
    for(t in -3:299){
      PD[t+4] = getPD(t,Rx)
      if(t == 0){Mpd[t+4] = Mpd[t+4]+m0}
      Mpd[t+5] = dPdt_tent(t,Mpd[t+4],params,PD[t+4])
      if(is.na(Mpd[t+5])){Mpd[t+5]=0}
      thresh = log10(1/100*10^6*5*10^3)
      if(Mpd[t+5] > thresh & Rx$StartTreatment == -100 & j < p*100){
        Rx$StartTreatment <<- t+8
      }
      if(t > delay){
        G[t] =  pmax(log10(10^(G[t-1]-gdk)+10^(Mpd[t-(delay)+4]+log10(gbk))),0)
        Gp[t] = ifelse(PD[t+4]==4, pmax(log10(10^(Gp[t-1]-gdk)+10^(Mpd[t-(delay)+4]+log10(gbk)))-PD[t+4],0),
                       pmax(log10(10^(Gp[t-1]-gdk)+10^(Mpd[t-(delay)+4]+log10(gbk))),0))
        Gpp[t] = pmax(log10(10^(Gpp[t-1]-gdk)+10^(Mpd[t-(delay)+4]+log10(gbk)))-PD[t+4],0)
      }
    }
    Mmat[j,] = Mpd
    Gmat[j,] = G
    Gpmat[j,] = Gp
    Gppmat[j,] = Gpp
    
    Mquant = colQuantiles(Mmat,probs=seq(0,1,.1))
    Mmed = Mquant[,6]
    Gquant = colQuantiles(Gmat,probs=seq(0,1,.1))
    Gmed = Gquant[,6]
    Gpquant = colQuantiles(Gpmat,probs=seq(0,1,.1))
    Gpmed = Gpquant[,6]
    Gppquant = colQuantiles(Gppmat,probs=seq(0,1,.1))
    Gppmed = Gppquant[,6]
    
    diff[j] = sum(abs(10^G-10^Gp))/sum(10^G)
    diff2[j] = sum(abs(10^G-10^Gpp))/sum(10^G)
    }
    
    output$plotSim = renderPlot({
      plot(-3:300,Mmed,type="l",xlab="time (days)",ylab="log10 iRBC")
      lines(-3:300,Gmed,lty=2)
      lines(-3:300,Gpmed,lty=2,col = "blue")
      lines(-3:300,Gppmed,lty=2, col = "red")
      
      Gplus = G[which(G>.1)]
      Gpplus = Gp[which(Gp>.1)]
      aveDiff = sum(diff)/100
      varDiff = var(diff)
      
      Gppplus = Gpp[which(Gp>.1)]
      aveDiff2 = sum(diff2)/100
      varDiff2 = var(diff2)
      
      
      legend(160,6, legend=c("P",floor(aveDiff*1000)/10,round(sqrt(varDiff*100),2),"P+RM",floor(aveDiff2*1000)/10,round(sqrt(varDiff2*100),2)))
    })
    })
  
}

##define tent function (will later import directly from tent_pflome)
tent = function(t,x0,peak,tpeak,tend){
  y = t
  tpeak = tpeak+18
  for(x in 1:length(t)){
    if(t[x]<0) y[x] = 0
    if(0<=t[x] & t[x]<=tpeak) y[x] = x0+(peak-x0)/(tpeak)*t[x]
    if(t[x]>=tpeak & t[x]<=tend) y[x] = peak-peak/(tend-tpeak)*(t[x]-tpeak)
    if(t[x]>tend) y[x]=0
  }
  return(y)
}

shinyApp(ui=ui,server=server)

