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

Imm = list()
AA = matrix(rep(0,100),ncol=10)

ui = fluidPage(

  titlePanel("PfLOME"),

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

    #################### Gametocyte Tab #########################

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

    #################### Immune Counter Tab ######################

    tabPanel("Immune Counters",
             pageWithSidebar(

               headerPanel("Immune Counter Calibration"),

               sidebarPanel(
                 uiOutput("types"),
                 uiOutput("counters"),
                 uiOutput("dropdown"),
                 uiOutput("wx"),
                 uiOutput("wn"),
                 uiOutput("P50"),
                 uiOutput("Ps"),
                 uiOutput("atMax"),
                 uiOutput("b"),
                 uiOutput("sigma")
               ),

               mainPanel(
                 plotOutput("immunePlot",width="100%"),
                 wellPanel(
                    uiOutput("showQuants"),
                    uiOutput("tNewInf")
                 )
               )
             )),
    
    ################### Parasite Genetics Tab ##############################
    
    tabPanel("Pf Phenotypes",
             
             pageWithSidebar(
               
               headerPanel("Phenotypic Space"),
               
               sidebarPanel(
                 
                 uiOutput("nvar"),
                 uiOutput("whichGene"),
                 uiOutput("nantigen"),
                 uiOutput("gweight")
                 
               ),
               
               mainPanel(
                 
                 plotOutput("genespace")
                 
               )
             )
             
             ),
    
    
    ############################### Fever Tab ################################
    
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
    
    ###################### Anemia Tab #####################################
    
    tabPanel("Anemia",
             
             pageWithSidebar(
               
               headerPanel("RBC and Anemia"),
               
               sidebarPanel(
                 uiOutput("baseRBC"),
                 uiOutput("anemiaThresh"),
                 uiOutput("delayRBC"),
                 uiOutput("a"),
                 uiOutput("c"),
                 uiOutput("d"),
                 uiOutput("e")
               ),
               
               mainPanel(
                 #                   plotOutput("PfPlot"),
                 plotOutput("RBCPlot"),
                 uiOutput("whichQuant")
               )
             )
             
             ),
    
    
    ####################### Biomarkers Tab ##################################
    
    tabPanel("Biomarkers",
             
             pageWithSidebar(
               
               headerPanel("Biomarkers"),
               
               sidebarPanel(
                 ##Initial infection
                 uiOutput("whichMarker"),
                 uiOutput("HRP2gr"),
                 uiOutput("HRP2dr"),
                 uiOutput("pLDHmodel"),
                 uiOutput("pLDHdata"),
                 uiOutput("PDdata"),
                 uiOutput("pLDHgr"),
                 uiOutput("pLDHdr")
               ),
               
               mainPanel(
                 plotOutput("biomarkerPlot")
               )
             )
             
             ),
    
    ############### Diagnostic Tests Tab ################
    
    tabPanel("Diagnostic Tests",
             
             pageWithSidebar(
               
               headerPanel("Diagnostic Tests"),
               
               sidebarPanel(
                 uiOutput("whichTest"),
                 uiOutput("whichMarkerTest"),
                 uiOutput("type1"),
                 uiOutput("type2"),
                 uiOutput("lm1Thresh"),
                 uiOutput("dnaThresh"),
                 uiOutput("RDTThresh"),
                 uiOutput("lm1Slope")
               ),
               
               mainPanel(
                 plotOutput("DTPlot"),
                 wellPanel(
                   uiOutput("testInf")
                 )
               )
               
             )
             
             ),
    
    ################## Medications Tab ##################
    
    tabPanel("Rx",
             
             pageWithSidebar(
               
               headerPanel("Drug Registry"),
               
               sidebarPanel(
                 uiOutput("whichDrug"),
                 uiOutput("regimen"),
                 uiOutput("timeTreated")
               ),
               
               mainPanel(
                 plotOutput("RxPlot")
               )
               
             )
             
             ),

    #################### Run Simulation ################

    tabPanel("Run LOME",
             
             headerPanel("Run PfLOME"),
             
             plotOutput("plotPfLOME",height="600px"),
             br(),
             br(),
             br(),
             br(),
             
             wellPanel(
               fluidRow(
                 column(6, align="center", offset = 3,
                        uiOutput("treatment"),
                        actionButton("runPfLOME", "Run PfLOME"),
                        tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
                  )
                )
              )
          )
    
  )
)


  ################################### Server ################################


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
        plot(x,dnorm(x,m,s),type="l",xlab="log10 Number of iRBC", ylab="Probability", main="Distribution of Initial iRBC")
      }
      if(input$Mz0=="exp"){
        x = seq(0,5*m,.1)
        plot(x,dexp(x,1/m),type="l")
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
          plot(x,dnorm(x,m,s),type="l")
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
        
         plotTent(500,Mz0,input$muMz0,input$sigmaMz0,peakt,input$muPeakt,input$sigmaPeakt,tfin,input$muTfin,maxPD,input$muMaxPD,input$sigmaMaxPD)
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
      plot(x,quantGam[,3],col="blue",lty=2,type="l",xlim=c(-3,300+input$delay),ylim=c(0,10),ylab="log10 Gametocyte Densities",xlab="Time (in Days)",main="Infection Profile - log10 Gametocyte Burden")
      lines(x,quantGam[,6])
      lines(x,quantGam[,9],lty=2, col="red")
      legend(200, 10, legend=c("20th Percentile", "Median", "80th Percentile"),
             col=c("blue", "black","red"), lty=c(2,1,2), cex=1.2)
    })

    ########################### Immune Counters Server #########################

    ##general or type-specific immunity
    output$types = renderUI({
      selectInput("types","General or Type-Specific Immune Counters:",c("General","Type-Specific"))
    })

    output$showQuants = renderUI({
      selectInput("showQuants","Show Median or Median and 20/80 percentiles:",c("Median","Median+20/80"))
    })

    ##number of counters of the above type
    output$counters = renderUI({
      numericInput("counters","Numer of Immune Counters",value=1,min=1,max=10,step=1)
    })

    ##time that a new infection occurs at given immune level (demonstrate effect of immunity)
    output$tNewInf = renderUI({
      sliderInput("tNewInf","time of new infection (days since previous infection)",value=0,min=0,max=200,step=1)
    })

    ##select which counter to configure
    output$dropdown = renderUI({
      selectInput("dropdown","Which counter would you like to configure?",1:as.integer(input$counters),selected = as.integer(input$counters))
    })

    ##counter waxing rate
    output$wx = renderUI({
      sliderInput("wx","Average number of days for Immune acquisition",1,200,value=80,step=1)
    })

    ##counter waning rate
    output$wn = renderUI({
      sliderInput("wn","Average number of days for Immune loss",1,200,value=180,step=1)
    })

    ##half saturation
    output$P50 = renderUI({
      sliderInput("P50", "Log10 Parasite Half Saturation",0,12,6)
    })

    output$Ps = renderUI({
      sliderInput("Ps", "Ps",0,12,1)
    })

    output$atMax = renderUI({
      sliderInput("atMax","atMax",0,12,11)
    })

    output$b = renderUI({
      sliderInput("b","b",0,5,2,step = .1)
    })

    output$sigma = renderUI({
      sliderInput("sigma","sigma",0,5,1,step=.1)
    })

    output$immunePlot = renderPlot({
      gImPAR = function(wx=1/80, wn=1/180, P50=6, Ps=1, atMax=11, b=2, sigma=1){
        list(wx=wx,wn=wn,P50=P50,Ps=Ps,atMax=atMax,b=b,sigma=sigma)
      }

      PAR = gImPAR(wx=1/input$wx,wn=1/input$wn,P50=input$P50,Ps=input$Ps,atMax=input$atMax,b=input$b,sigma=input$sigma)
      Imm[as.integer(input$dropdown)] <<- list(PAR)
      ImmPars = NULL
      for(i in 1:as.integer(input$counters)){
        ImmPars = rbind(ImmPars,Imm[[i]])
      }
      write.csv(ImmPars,"ImmPars.txt")

      M = read.csv("MzData.txt")
      M = as.matrix(M[,2:ncol(M)])
      meanM = colMeans(M[,5:ncol(M)])
      quantM = colQuantiles(M[,5:ncol(M)],probs=seq(0, 1, 0.1))
      muI = rep(0,length(meanM))
      I20 = muI
      Imed = muI
      I80 = muI

      for(i in 2:length(meanM)) {
        muI[i] = dynamicXdt(muI[i-1],meanM[i-1],PAR)
        I20[i] = dynamicXdt(I20[i-1],quantM[i-1,3],PAR)
        Imed[i] = dynamicXdt(Imed[i-1],quantM[i-1,6],PAR)
        I80[i] = dynamicXdt(I80[i-1],quantM[i-1,9],PAR)
      }
      
#      Imedtot = Imed thinking of including total immunity = 1-(prod(1-imm[[i]]))
#      I20tot = I20
#      I80tot = I80

      Mmod = M
      Mmod[,1] =  Mmod[,1]*Mz0Mod(1,Imed[input$tNewInf+1],.6,3)
      Mmod[,2] =  Mmod[,2]*peakDMod(1,Imed[input$tNewInf+1],.6,3)
      Mmod[,3] =  Mmod[,3]*tEndMod(1,Imed[input$tNewInf+1],.6,3)
      Mmod[,4] =  Mmod[,4]*mxPDMod(1,Imed[input$tNewInf+1],.6,3)

      for(i in 1:nrow(M)){
        Mmod[i,5:ncol(M)] = tent(-3:(ncol(M)-8),Mmod[i,1],Mmod[i,4],(Mmod[i,2]+18),Mmod[i,3])
      }

      MmodQuants = colQuantiles(Mmod[,5:ncol(M)],probs=seq(0, 1, 0.1))
      Mmodmed = MmodQuants[,6]
      Mmod20 = MmodQuants[,3]
      Mmod80 = MmodQuants[,9]


      pad = rep(0,input$tNewInf)
      Mmodmed = c(pad,Mmodmed)
      Mmod20 = c(pad,Mmod20)
      Mmod80 = c(pad,Mmod80)
      par(mfrow=c(2,1),mar=c(4,4,1,1))
      plot(-3:(length(muI)-4),Imed,type="l",xlab="Time (in Days)",ylab="% of Maximum Immunity")
      plot(-3:(length(muI)-4),Mmodmed[1:length(Imed)],type="l",ylim=c(0,11),xlab="Time (in Days)",ylab="Profile of Secondary Infection")
      if(input$showQuants=="Median+20/80"){
        lines(-3:(length(muI)-4),Mmod20[1:length(Imed)],lty=2)
        lines(-3:(length(muI)-4),Mmod80[1:length(Imed)],lty=2)
      }
      #    plot(-3:(length(muI)-4),quantM[,6],type="l",xlab="Time(in Days)",ylab="log10 iRBC")
      #    lines(1:length(I20),I20)
      #    lines(1:length(Imed),Imed)
      #    lines(1:length(I80),I80)
    },height=400)
    
    
    ################### Parasite Genetics Server ############################
    
    output$nvar = renderUI({
      sliderInput("nvar","Enter the number of VAR genes in the Pf population:",value=3,min=1,max=10,step=1)
    })
    
    
    output$whichGene = renderUI({
      selectInput("whichGene","Select which gene to configure:",1:input$nvar,selected=1)
    })
    
    
    output$nantigen = renderUI({
      sliderInput("nantigen","Choose the number of phenotypes associated with all genetic variants at this locus",value=4,min=1,max=10)
    })
    
    output$gweight = renderUI({
      textInput("gweight","Input the Relative Weight of Each Phenotype (As a Decimal)",value=".25,.25,.25,.25")
    })
    
    output$genespace = renderPlot({
      A = matrix(rep(1,100),ncol=10)
      geneweight = A*0
      whichGene = as.integer(input$whichGene)
      nantigen = rep(1,input$nantigen)
      for(i in 1:input$nantigen){
        A[i,whichGene] = 1
        geneweight[i,whichGene] = as.numeric(unlist(strsplit(input$gweight,",")))[i]
        #      nantigen[whichGene] = input$nantigen
      }
      geneweight[is.na(geneweight)]=0
      A = A*geneweight
      AA[,whichGene] <<- A[,whichGene]*input$nantigen
      B=melt(AA[,1:input$nvar],id.var="Rank")
      fill=rep(c("dodgerblue","seagreen2"),5)
      ggplot(B,aes(x=B$Var2,y=B$value,fill=B$Var1))+geom_bar(stat="identity",aes(color="seagreen2"),show.legend = F)
    })
    
    
    ########################## Fever Server ######################################
    
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
        plot(-3:(length(Fev)-4),Fev,type="l")
        par(new=T)
        plot(-3:(length(Fev)-4),Mmed,axes=F,type="l",lty=2)
        axis(side=4)
      }
      half = log10(input$halfFever/100*10^6*5*10^3)
      rate = input$rateFever
      if(input$fever == "Graded"){
        Fev = 37+(input$maxFever-37)*sigmoidX(pmax(Mmed,0),half,rate,Inf)
        plot(-3:(length(Fev)-4),Fev,ylim=c(37,42),type="l")
        par(new=T)
        plot(-3:(length(Fev)-4),Mmed,axes=F,type="l",lty=2)
        axis(side=4)
      }
      
    })
    
    
    ############################# Anemia Server #########################
    
    
    output$baseRBC = renderUI({
      sliderInput("baseRBC","Normal RBC Baseline (millions of cells/uL)",value=5,min=3.5,max=6.5,step=.1)
    })
    
    output$anemiaThresh = renderUI({
      sliderInput("anemiaThresh","Enter the Cutoff log10 RBC Count for Anemia",value=1,min=.01,max=50,step=.01)
    })
    
    output$delayRBC = renderUI({
      sliderInput("delayRBC","Enter the Length of Maturation of RBCs (in Days)",value=7,min=0,max=10,step=1)
    })
    
    output$a = renderUI({
      sliderInput("a","a",value=.006,min=.001,max=.01,step=.001)
    })
    
    output$c = renderUI({
      sliderInput("c","c",value=1.7,min=.1,max=4,step=.1)
    })
    
    output$d = renderUI({
      sliderInput("d","Maximum daily % of RBC Cell Lysis",value=1,min=.1,max=50,step=.1)
    })
    
    output$e = renderUI({
      sliderInput("e","% Parasitemia for Half Maximum Rate of RBC Lysis",value=1,min=.1,max=2,step=.1)
    })
    
    output$whichQuant = renderUI({
      selectInput("whichQuant","Which Quantile Would You Like to See?",c("20","50","80"),selected="50")
    })
    
    output$PfPlot = renderPlot({
      M = read.csv("Mzdata.txt")
      M = as.matrix(M[,2:ncol(M)])
      MQuants = colQuantiles(M[,5:ncol(M)],probs=seq(0, 1, 0.1))
      Mmed = MQuants[,6]
      M20 = MQuants[,3]
      M80 = MQuants[,9]
      if(input$whichQuant=="50"){
        plot(-3:(length(Mmed)-4),Mmed,type="l",ylab="iRBC Count")
      }
      if(input$whichQuant=="20"){
        plot(-3:(length(M20)-4),M20,type="l",ylab="iRBC Count")
      }
      if(input$whichQuant=="80"){
        plot(-3:(length(M80)-4),M80,type="l",ylab="iRBC Count")
      }
    })
    
    output$RBCPlot = renderPlot({
      M = read.csv("Mzdata.txt")
      M = as.matrix(M[,2:ncol(M)])
      MQuants = colQuantiles(M[,5:ncol(M)],probs=seq(0, 1, 0.1))
      Mmed = MQuants[,6]
      M20 = MQuants[,3]
      M80 = MQuants[,9]
      r0 = input$baseRBC/2
      b = input$a*r0*exp(input$c*r0)
      e = 13+log10(input$e/100)
      d = input$d/100
      rmed = rep(r0,length(Mmed))
      rmed = c(rep(r0,input$delayRBC),rmed)
      r20 = rmed
      r80 = rmed
      for(i in 2:length(Mmed)){
        rmed[i+input$delayRBC] = rmed[i-1+input$delayRBC]-input$a*rmed[i-1+input$delayRBC]+b*exp(-input$c*rmed[i-1])-d*rmed[i-1+input$delayRBC]*10^(Mmed[i-1])/(10^(e) + 10^(Mmed[i-1]))
        r20[i+input$delayRBC] = r20[i-1+input$delayRBC]-input$a*r20[i-1+input$delayRBC]+b*exp(-input$c*r20[i-1])-d*r20[i-1+input$delayRBC]*10^(M20[i-1])/(10^(e) + 10^(M20[i-1]))
        r80[i+input$delayRBC] = r80[i-1+input$delayRBC]-input$a*r80[i-1+input$delayRBC]+b*exp(-input$c*r80[i-1])-d*r80[i-1+input$delayRBC]*10^(M80[i-1])/(10^(e) + 10^(M80[i-1]))
      }
      if(input$whichQuant=="50"){
        plot((input$delayRBC-3):(length(rmed)-4),rmed[(input$delayRBC+1):length(rmed)]*10^13/(5*10^6),type="l",ylab="RBC Count / mm^3")
      }
      if(input$whichQuant=="20"){
        plot((input$delayRBC-3):(length(r20)-4),r20[(input$delayRBC+1):length(r20)]*10^13/(5*10^6),type="l",ylab="RBC Count / mm^3")
      }
      if(input$whichQuant=="80"){
        plot((input$delayRBC-3):(length(r80)-4),r80[(input$delayRBC+1):length(r80)]*10^13/(5*10^6),type="l",ylab="RBC Count / mm^3")
      }
      abline(h=2.5/3*10^13/(5*10^6))
    })
    
    
    ########################### Biomarkers Server #######################
    
    
    output$whichMarker = renderUI({
      selectInput("whichMarker","Which Marker Would You Like to Configure?",c("HRP2","pLDH"),selected = "HRP2")
    })
    
    output$HRP2gr = renderUI({
      if(input$whichMarker == "HRP2"){
        sliderInput("HRP2gr","Enter the Per-Parasite Rate of HRP2 Production (pg/day)",value=1.8,min=.01,max=5,step=.01)
      }
    })
    
    output$pLDHmodel = renderUI({
      if(input$whichMarker == "pLDH"){
        selectInput("pLDHmodel","Which Model for pLDH Would You Like?",c("Dynamic","Linear"),selected="Dynamic")
      }
    })
    
    output$pLDHgr = renderUI({
      if(input$whichMarker == "pLDH" && input$pLDHmodel=="Dynamic"){
        sliderInput("pLDHgr", "Enter the Per-Parasite Rate of pLDH Production (pg/day)",value=.13,min=.01,max=.5,step=.01)
      }
    })
    
    output$HRP2dr = renderUI({
      if(input$whichMarker == "HRP2"){
        sliderInput("HRP2dr","Enter the Halflife of HRP2 (in days)",value=3.7,min=1,max=10,step=.1)
      }
    })
    
    output$pLDHdr = renderUI({
      if(input$whichMarker == "pLDH" && input$pLDHmodel=="Dynamic"){
        sliderInput("pLDHdr","Enter the Halflife of pLDH (in days)",value=2,min=1,max=10,step=.1)
      }
    })
    
    output$pLDHdata = renderUI({
      if(input$whichMarker == "pLDH" && input$pLDHmodel=="Linear"){
        textInput("pLDHdata","Enter a list of [pLDH] (ng/mL), separated by commas",value="8700,870,435,87,44,8.7,.9,0")
      }
    })
    
    output$PDdata = renderUI({
      if(input$whichMarker == "pLDH" && input$pLDHmodel=="Linear"){
        textInput("PDdata","Enter a list of Parasites/uL, separated by commas",value="5000,500,250,50,25,5,.5,0")
      }
    })
    
    
    
    output$biomarkerPlot = renderPlot({
      M = read.csv("Mzdata.txt")
      M = as.matrix(M[,2:ncol(M)])
      MQuants = colQuantiles(M[,5:ncol(M)],probs=seq(0, 1, 0.1))
      Mmed = MQuants[,6]
      M20 = MQuants[,3]
      M80 = MQuants[,9]
      if(input$whichMarker == "HRP2"){
        HRP2med = 0*Mmed
        HRP220 = 0*Mmed
        HRP280 = 0*Mmed
        gr = input$HRP2gr/1000
        dr = log(2)/input$HRP2dr
        for(i in 2:length(Mmed)){
          HRP2med[i] = HRP2med[i-1]+gr*10^Mmed[i-1]-dr*HRP2med[i-1]
          HRP220[i] = HRP220[i-1]+gr*10^M20[i-1]-dr*HRP220[i-1]
          HRP280[i] = HRP280[i-1]+gr*10^M80[i-1]-dr*HRP280[i-1]
        }
        plot(-3:(length(Mmed)-4),log10(HRP2med),type="l",ylim=c(-2,9),ylab="log10([HRP2]), ng/uL")
        lines(-3:(length(Mmed)-4),log10(HRP220),lty=2)
        lines(-3:(length(Mmed)-4),log10(HRP280),lty=2)
        write.csv(HRP2med,'HRP2.txt')
      }
      if(input$whichMarker == "pLDH"){
        if(input$pLDHmodel=="Linear"){
          Mm = 10^Mmed/5/10^6
          Mm20 = 10^M20/5/10^6
          Mm80 = 10^M80/5/10^6
          ydata = input$pLDHdata
          y = as.numeric(strsplit(ydata,",")[[1]])
          xdata = input$PDdata
          x = as.numeric(strsplit(xdata,",")[[1]])
          slope = lm(y~x)[[1]][2]
          intercept = lm(y~x)[[1]][1]
          pLDHmed = pmax(Mm*slope+intercept,.01)
          pLDH20 = pmax(Mm20*slope+intercept,.01)
          pLDH80 = pmax(Mm80*slope+intercept,.01)
          plot(-3:(length(Mm)-4),log10(pLDHmed),type="l",ylim=c(-2,5),ylab="log10([pLDH]), ng/mL")
          lines(-3:(length(Mm)-4),log10(pLDH20),lty=2)
          lines(-3:(length(Mm)-4),log10(pLDH80),lty=2)
          write.csv(pLDHmed,'pLDH.txt')
        }
        if(input$pLDHmodel == "Dynamic"){
          pLDHmed = 0*Mmed
          pLDH20 = 0*Mmed
          pLDH80 = 0*Mmed
          gr = input$pLDHgr/1000
          dr = log(2)/input$pLDHdr
          for(i in 2:length(Mmed)){
            pLDHmed[i] = pLDHmed[i-1]+gr*10^Mmed[i-1]-dr*pLDHmed[i-1]
            pLDH20[i] = pLDH20[i-1]+gr*10^M20[i-1]-dr*pLDH20[i-1]
            pLDH80[i] = pLDH80[i-1]+gr*10^M80[i-1]-dr*pLDH80[i-1]
          }
          plot(-3:(length(Mmed)-4),log10(pLDHmed),type="l",ylim=c(-2,8),ylab="log10([pLDH]), ng/mL")
          lines(-3:(length(Mmed)-4),log10(pLDH20),lty=2)
          lines(-3:(length(Mmed)-4),log10(pLDH80),lty=2)
          write.csv(pLDHmed,'pLDH.txt')
        }
      }
    })
    
    ###################### Diagnostic Test Server ############################
    
    output$whichTest = renderUI({
      selectInput("whichTest","Which Diagnostic Test Would You Like to Calibrate?",c("Light Microscopy (Asexual)","Light Microscopy (Gametocyte)","LAMP","PCR","RDT (Ordinary)","RDT (Highly Sensitive)"))
    })
    
    output$whichMarkerTest = renderUI({
      if(input$whichTest=="RDT (Ordinary)" | input$whichTest=="RDT (Highly Sensitive)"){
        selectInput("whichMarkerTest","RDT for Which Marker?",c("HRP2","pLDH"))
      }
    })
    
    output$type1 = renderUI({
      sliderInput("type1","Enter the False Positive Rate (%)",value=1,min=.1,max=15,step=.1)
    })
    
    output$type2 = renderUI({
      sliderInput("type2","Enter the False Negative Rate (%)",value=1,min=.1,max=15,step=.1)
    })
    
    output$lm1Thresh = renderUI({
      if(input$whichTest=="Light Microscopy (Asexual)" | input$whichTest=="Light Microscopy (Gametocyte)"){
        sliderInput("lm1Thresh","Enter the Threshold of Detectability (Parasites/uL)",value=25,min=1,max=100,step=1)
      }
    })
    
    output$dnaThresh = renderUI({
      if(input$whichTest=="PCR" | input$whichTest=="LAMP"){
        sliderInput("dnaThresh","Enter the Threshold of Detectability (Parasites/nL",value=50,min=1,max=100,step=1)
      }
    })
    
    output$RDTThresh = renderUI({
      if(input$whichTest=="RDT (Ordinary)" | input$whichTest=="RDT (Highly Sensitive)"){
        sliderInput("RDTThresh","Enter the Threshold of Detectability (ng/mL)",value=11,min=.1,max=30,step=.1)
      }
    })
    
    
    output$lm1Slope = renderUI({
      sliderInput("lm1Slope","How Steep is the Transition from Undetectable to Detectable?",value=0,min=-2,max=2,step=.1)
    })
    
    output$testInf = renderUI({
      selectInput("testInf","Which Plot Would You Like to See?",c("Test Response Curve","Infection Time Series"))
    })
    
    output$DTPlot = renderPlot({
      M = read.csv("MzData.txt")
      M = as.matrix(M[,2:ncol(M)])
      M = M[,5:ncol(M)]
      quantM = colQuantiles(M,probs=seq(0, 1, 0.1))
      Mmed = quantM[,5]
      G = read.csv('GtData.txt')
      G = as.matrix(G[,2:ncol(G)])
      quantG = colQuantiles(G,probs=seq(0,1,.1))
      Gmed = quantG[,5]
      HRP2 = read.csv('HRP2.txt')
      HRP2 = log10(as.matrix(HRP2[,2:ncol(HRP2)]))
      pLDH = read.csv('pLDH.txt')
      pLDH = log10(as.matrix(pLDH[,2:ncol(pLDH)]))
      if(input$whichTest=="Light Microscopy (Asexual)"){
        y = Mmed
        thresh = input$lm1Thresh
        pd = seq(0,100,1)
        s = 0
        ylab = "Probability of Positive Test"
        xlab = "Parasites/uL"
      }
      if(input$whichTest=="Light Microscopy (Gametocyte)"){
        y = Gmed
        thresh = input$lm1Thresh
        pd = seq(0,100,1)
        s = 0
        ylab = "Probability of Positive Test"
        xlab = "Parasites/uL"
      }
      if(input$whichTest=="PCR" | input$whichTest == "LAMP"){
        Gmed = Gmed[1:(length(Gmed)-10)]
        y = log10(10^Gmed+10^Mmed)
        thresh=input$dnaThresh/1000
        pd = seq(0,100,1)
        s = 0
      }
      if(input$whichTest=="RDT (Ordinary)" | input$whichTest=="RDT (Highly Sensitive)"){
        if(input$whichMarkerTest=="HRP2"){
          y = HRP2
          thresh = input$RDTThresh
          pd = seq(0,30,.1)
          s = 0
          ylab = "Probability of Positive Test"
          xlab = "[HRP2] ng/mL"
        }
        if(input$whichMarkerTest=="pLDH"){
          y = pLDH
          thresh = input$RDTThresh
          pd = seq(0,30,.1)
          s = 0
          ylab = "Probability of Positive Test"
          xlab = "[pLDH] ng/uL"
        }
      }
      
      p = input$type1 + (100-input$type1-input$type2)*sigmoidX(10^y/5/10^6,thresh,10^(input$lm1Slope+s),Inf)
      
      if(input$testInf=="Infection Time Series"){
        par(mfrow=c(2,1),mai = c(1, .8, 0.1, 0.1))
        ylim = c(0,max(y)+1)
        plot(-3:(length(y)-4),y,type="l",ylim=ylim)
        abline(h=log10(thresh*10^6*5),lty=2)
        plot(-3:(length(p)-4),p,type="l",ylim=c(0,100))
        abline(h=c(input$type1,(100-input$type2)),lty=2)
      }
      if(input$testInf=="Test Response Curve"){
        par(mfrow=c(1,1))
        plot(pd,input$type1/100+(100-input$type1-input$type2)/100*sigmoidX(pd,thresh,10^(input$lm1Slope+s),Inf),type="l",ylim=c(0,1))
        abline(v=thresh)
        abline(h=c(input$type1/100,(100-input$type2)/100),lty=2)
      }
    },height=400)
    
    ############################# Rx Server ##################################
    
    output$whichDrug = renderUI({
      selectInput("whichDrug","Which Drug Would You Like to Callibrate?",c("Artemisinin","Quinine","Chloroquine","Cipargamin"))
    })
    
    output$regimen = renderUI({
      selectInput("regimen","How Strictly Do They Adhere to Dosage?",c("Strictly","Partially"))
    })
    
    output$timeTreated = renderUI({
      sliderInput("timeTreated","Time Treatment is Initiated",value=0,min=0,max=300)
    })
    
    
    output$RxPlot = renderPlot({
      
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
      #calculate PD effects on M and G
      for(t in -3:295){
        PD = getPD(t,Rx)
        Mpd[t+5] = dPdt_tent(t,Mpd[t+4],params,PD)
        if(t==0) {
          Mpd[t+5] = m0
        }
        if(is.na(Mpd[t+5])){
          Mpd[t+5] = 0
        }
        if(t > delay){
          G[t] = pmax(log10(10^(G[t-1]-gdk)+10^(Mpd[t-(delay)+3]+log10(gbk))),0)
        }
      }
      plot(-3:(length(Mpd)-4),Mpd,type="l",ylim=c(0,11))
      lines(-3:(length(Mpd)-4),G,lty=2)
    })
    
    
    ########################## Run Simulation Server ##################
    
    
    output$treatment = renderUI({
      checkboxInput("treatment","Do you want to include treatment?",value=F)
    })
    
    observeEvent(input$runPfLOME, {
      if(input$treatment){treat = unique(sort(sample(bites,10)+8))}
      if(input$treatment==F){treat = -100}
      human = doOneHuman(bites,moi,age,treat)
      output$plotPfLOME = renderPlot({
        plotHistory(human,bites,moi,age)
      })
    })
    
  }
  
  ######################## End of Server; Extra Functions not in PfLOME ##################
  
  Mz0Mod = function(maxDec,Imm,xh,b){
    Mz0 = 1-sigmoid01(Imm,xh,b,maxDec)
    return(Mz0)
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