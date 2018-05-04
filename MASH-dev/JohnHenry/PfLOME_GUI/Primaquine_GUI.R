library(shiny)
library("R6")
source("PfLOME_Pathogen.R")
source("PfLOME_Human.R")
##human sources ImmuneState and HealthState classes
source("Rx.R")
##source ("eventTimeSeries.R") ##source this if you want to pull from mbites
source("Primaquine.R")


ui <- fluidPage(
  
  # App title ----
  titlePanel("Primaquine Effect Size"),
  
  tabsetPanel(type="tabs",
              
              #########################   Infection Profile Tab   #####################
              
              tabPanel("Infection Profile",
  
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
    
                    # Sidebar panel for inputs ----
                    sidebarPanel(
      
                    # Input: Slider for the number of bins ----
                    sliderInput(inputId = "MZ0",
                          label = "Initial log10 Parasite Numbers (post emergence from liver)",
                          min = 2,
                          max = 6,
                          step=.1,
                          value = 4.2),
      
                    sliderInput(inputId = "MaxPD",
                          label="Maximum log10 Parasite Burden",
                          min=8,
                          max = 13,
                          step=.1,
                          value=11.5),
      
                    sliderInput(inputId = "PeakD",
                          label = "Time of Peak Parasite Burden (days since emergence from liver)",
                          min=10,
                          max=30,
                          step=1,
                          value=20),
      
                    sliderInput(inputId = "Duration",
                          label = "Duration of Infection (in days)",
                          min = 200,
                          max = 365,
                          step = 1,
                          value = 250
                          )
      
                    ),
    
            mainPanel(
      
            plotOutput(outputId = "tentPlot")
      
            )
         )
      ),
      
      ##########################   Gametocyte Tab   ########################
      
      tabPanel("Gametocyte Production",
               sidebarLayout(
                 
                  sidebarPanel(
                   
                    sliderInput(inputId = "Ggr",
                                label = "Gametocyte Production Per Asexual Parasite Per Day",
                                min = .001,
                                max = .1,
                                step = .001,
                                value = .01
                    ),
                    
                    sliderInput(inputId = "Gdk",
                                label = "Gametocyte Halflife (in Days)",
                                min = 2,
                                max = 14,
                                step = 1,
                                value = 6
                    ) 
                   
                  ),
                  
                  mainPanel(
                    
                    plotOutput(outputId = "GamPlot")
                    
                  )
                 
               )
      ),
      
      #######################   Transmission Tab   ##########################
      
      tabPanel("Transmission",
               
               sidebarLayout(
                 
                 sidebarPanel(
                   
                    sliderInput(inputId = "xh",
                                label = "Half Maximum Transmission Efficiency",
                                min = 1,
                                max = 11,
                                step = .1,
                                value = 7
                    ),
                    
                    sliderInput(inputId = "pow",
                                label = "Steepness Parameter",
                                min = 0,
                                max = 2.5,
                                step = .01,
                                value = 1
                    ),
                    
                    sliderInput(inputId = "MaxEfficiency",
                                label = "Maximum Transmission Efficiency",
                                min = .1,
                                max = 1,
                                step = .01,
                                value = 1
                    ),
                    
                    radioButtons(inputId = "WhichView",
                                 label = "Which Plot Would You Like to See?",
                                 choices = c("Conversion Function", "Single Infection")
                    
                    )
                   
                 ),
                 
                 mainPanel(
                   
                   plotOutput(outputId = "TEPlot")
                   
                 )
                 
               )
      ),
      
      ###########################   Fever and Treatment Tab   ################################
      
      tabPanel("Fever and Treatment",
               
               sidebarLayout(
                 
                 sidebarPanel(
                   
                   sliderInput(inputId = "feverThresh",
                               label = "Fever Threshold (log10 Parasite Density)",
                               min = 8,
                               max = 11,
                               step = .1,
                               value = 9.5
                   ),
                   
                   checkboxInput(inputId = "Treat",
                                 label = "Treat Infection (if symptomatic)",
                                 value = FALSE
                   ),
                   
                   checkboxInput(inputId = "PQ",
                                 label = "Include Primaquine in Treatment",
                                 value = FALSE
                   ),
                   
                   sliderInput(inputId = "TreatmentDelay",
                                 label = "Delay Between First Fever and Treatment (in Days)",
                                 min = 1,
                                 max = 14,
                                 step = 1,
                                 value = 7
                   )
                   
                 ),
                 
                 mainPanel(
                   plotOutput(outputId = "FeverPlot")
                 )
          )
      ),
      
      ########################   Simulation Tab   ########################################
      
      tabPanel("Simulation",
                    
               plotOutput(outputId = "simPlot")
                  
      )
   )
)

    # Define server logic required to draw a histogram ----
    server <- function(input, output) {
  
      
    ####################     Infection Profile Server   #####################
      
    output$tentPlot <- renderPlot({
    
    #set simulation parameters
    t = 1
    dt = 1
    tFinal = 365
    PQ = F
    
    #create and infect human
    person = Human$new(1,IncImm=F,IncPfPed=F)
    person$infectHuman(0,1)
    
    #set tent parameters from input
    person$get_pathogen()$get_Pf()[[1]]$set_mnMZ0(input$MZ0)
    person$get_pathogen()$get_Pf()[[1]]$set_Pt(input$MZ0)
    person$get_pathogen()$get_Pf()[[1]]$set_mnMaxPD(input$MaxPD)
    person$get_pathogen()$get_Pf()[[1]]$set_mnPeakD(input$PeakD)
    person$get_pathogen()$get_Pf()[[1]]$set_mnDuration(input$Duration)
    
    #simulate person
    simPerson2(person,tFinal,dt,PQ,F)
    
    #update tent parameter file
    write.csv(c(input$MZ0,input$MaxPD,input$PeakD,input$Duration),file='tentPAR.csv')
    
    #plot results
    plotPerson(person,F,F)
    
  })
    
  ###################    Gametocytes Server ###################
  
    output$GamPlot <- renderPlot({
      
      #read and extract tent parameters
      tp = read.csv('tentPAR.csv')
      tp = as.matrix(tp)[,2]
      MZ0 = tp[1]
      MaxPD = tp[2]
      PeakD = tp[3]
      Duration = tp[4]
      
      #set simulation parameters
      t = 1
      dt = 1
      tFinal = 365
      PQ = F
      
      #create and infect human
      person = Human$new(1,IncImm=F,IncPfPed=F)
      person$infectHuman(0,1)
      
      #set tent parameters
      person$get_pathogen()$get_Pf()[[1]]$set_mnMZ0(MZ0)
      person$get_pathogen()$get_Pf()[[1]]$set_Pt(MZ0)
      person$get_pathogen()$get_Pf()[[1]]$set_mnMaxPD(MaxPD)
      person$get_pathogen()$get_Pf()[[1]]$set_mnPeakD(PeakD)
      person$get_pathogen()$get_Pf()[[1]]$set_mnDuration(Duration)
      
      #set gametocyte parameters
      person$get_pathogen()$get_Pf()[[1]]$set_ggr(input$Ggr)
      person$get_pathogen()$get_Pf()[[1]]$set_gdk(input$Gdk)
      
      #simulate person
      simPerson2(person,tFinal,dt,PQ,F)
      
      #plot results
      plotPerson(person,T,F)
      
      #update gametocyte parameter csv
      write.csv(c(input$Ggr,input$Gdk),file='gamPAR.csv')
      
    })
    
    
    #####################    Transmission Server    ###########################
    
    
    output$TEPlot <- renderPlot({
      
      #read and extract tent parameters
      tp = read.csv('tentPAR.csv')
      tp = as.matrix(tp)[,2]
      MZ0 = tp[1]
      MaxPD = tp[2]
      PeakD = tp[3]
      Duration = tp[4]
      
      #read and extract gametocyte parameters
      gp = read.csv('gamPAR.csv')
      gp = as.matrix(gp)[,2]
      ggr = gp[1]
      gdk = gp[2]
      
      #set simulation parameters
      t = 1
      dt = 1
      tFinal = 365
      PQ = F
      
      #set tent parameters
      person = Human$new(1,IncImm=F,IncPfPed=F)
      person$infectHuman(0,1)
      person$get_pathogen()$get_Pf()[[1]]$set_mnMZ0(MZ0)
      person$get_pathogen()$get_Pf()[[1]]$set_Pt(MZ0)
      person$get_pathogen()$get_Pf()[[1]]$set_mnMaxPD(MaxPD)
      person$get_pathogen()$get_Pf()[[1]]$set_mnPeakD(PeakD)
      person$get_pathogen()$get_Pf()[[1]]$set_mnDuration(Duration)
      
      #set gametocyte parameters
      person$get_pathogen()$get_Pf()[[1]]$set_ggr(ggr)
      person$get_pathogen()$get_Pf()[[1]]$set_gdk(gdk)
      
      #set transmission parameters from input
      person$get_pathogen()$set_teMed(input$xh)
      person$get_pathogen()$set_teSlope(10^input$pow)
      person$get_pathogen()$set_teMax(input$MaxEfficiency)
      
      #simulate person
      simPerson2(person,tFinal,dt,PQ,F)
      
      #define external sigmoid function for demonstration
      sigmoid = function(x,xh,a){
        (x/xh)^a/((x/xh)^a+1)
      }
      x = seq(0,13,.01)
      
      #plot output
      par(mfrow=c(2,1))
      plot(x,input$MaxEfficiency*sigmoid(x,input$xh,10^input$pow),type="l",xlab = "log10 Gametocyte Numbers",ylab="Tranmsission Efficiency",ylim=c(0,1))
      PlotTE(person)
      
      #update sigmoid transmission efficiency parameters
      write.csv(c(input$xh,input$pow,input$MaxEfficiency),'tePAR.csv')
      
    })
    
  ####################    Fever and Treatment Server    ########################
    
  output$FeverPlot <- renderPlot({
    
    #read and extract tent parameters
    tp = read.csv('tentPAR.csv')
    tp = as.matrix(tp)[,2]
    MZ0 = tp[1]
    MaxPD = tp[2]
    PeakD = tp[3]
    Duration = tp[4]
    
    #read and extract gametocyte parameters
    gp = read.csv('gamPAR.csv')
    gp = as.matrix(gp)[,2]
    ggr = gp[1]
    gdk = gp[2]
    
    #read and extract transmission parameters
    tep = read.csv('tePAR.csv')
    tep = as.matrix(tep)[,2]
    xh = tep[1]
    pow = tep[2]
    MaxEfficiency = tep[3]
    
    #set simulation parameters
    t = 1
    dt = 1
    tFinal = 365
    PQ = input$PQ
    Treat = input$Treat
    Delay = input$TreatmentDelay
    
    #set tent function parameters
    person = Human$new(1,IncImm=F,IncPfPed=F)
    person$infectHuman(0,1)
    person$get_pathogen()$get_Pf()[[1]]$set_mnMZ0(MZ0)
    person$get_pathogen()$get_Pf()[[1]]$set_Pt(MZ0)
    person$get_pathogen()$get_Pf()[[1]]$set_mnMaxPD(MaxPD)
    person$get_pathogen()$get_Pf()[[1]]$set_mnPeakD(PeakD)
    person$get_pathogen()$get_Pf()[[1]]$set_mnDuration(Duration)
    
    #set gametocyte parameters
    person$get_pathogen()$get_Pf()[[1]]$set_ggr(ggr)
    person$get_pathogen()$get_Pf()[[1]]$set_gdk(gdk)
    
    #set transmission parameters
    person$get_pathogen()$set_teMed(xh)
    person$get_pathogen()$set_teSlope(10^pow)
    person$get_pathogen()$set_teMax(MaxEfficiency)
    
    #set fever threshold
    person$get_healthState()$set_feverThresh(input$feverThresh)
    
    #simulate person
    simPerson2(person,tFinal,dt,PQ,Treat,Delay)
    
    #plot results
    par(mfrow=c(2,1),"lheight" = 3)
    plotPerson(person,T,T)
    PlotTE(person)
    
    #update fever and treatment parameter csv
    write.csv(c(input$feverThresh,input$TreatmentDelay),'feverTreatPAR.csv')
    
  })
    
    
    ##########################    Simulation Tab     ################################
  
  output$simPlot <- renderPlot({
      
      #read and extract tent parameters
      tp = read.csv('tentPAR.csv')
      tp = as.matrix(tp)[,2]
      MZ0 = tp[1]
      MaxPD = tp[2]
      PeakD = tp[3]
      Duration = tp[4]
      
      #read and extract gametocyte parameters
      gp = read.csv('gamPAR.csv')
      gp = as.matrix(gp)[,2]
      ggr = gp[1]
      gdk = gp[2]
      
      #read and extract transmission parameters
      tep = read.csv('tePAR.csv')
      tep = as.matrix(tep)[,2]
      xh = tep[1]
      pow = tep[2]
      MaxEfficiency = tep[3]
      
      #read and extract fever and treatment parameters
      ftp = read.csv('feverTreatPAR.csv')
      ftp = as.matrix(ftp)[,2]
      feverThresh = ftp[1]
      Delay = ftp[2]
      
      
      #set simulation parameters
      t = 1
      dt = 1
      tFinal = 365
      
      #set tent function parameters
      person = Human$new(1,IncImm=F,IncPfPed=F)
      person$infectHuman(0,1)
      person$get_pathogen()$get_Pf()[[1]]$set_mnMZ0(MZ0)
      person$get_pathogen()$get_Pf()[[1]]$set_Pt(MZ0)
      person$get_pathogen()$get_Pf()[[1]]$set_mnMaxPD(MaxPD)
      person$get_pathogen()$get_Pf()[[1]]$set_mnPeakD(PeakD)
      person$get_pathogen()$get_Pf()[[1]]$set_mnDuration(Duration)
      
      person2 = Human$new(1,IncImm=F,IncPfPed=F)
      person2$infectHuman(0,1)
      person2$get_pathogen()$get_Pf()[[1]]$set_mnMZ0(MZ0)
      person2$get_pathogen()$get_Pf()[[1]]$set_Pt(MZ0)
      person2$get_pathogen()$get_Pf()[[1]]$set_mnMaxPD(MaxPD)
      person2$get_pathogen()$get_Pf()[[1]]$set_mnPeakD(PeakD)
      person2$get_pathogen()$get_Pf()[[1]]$set_mnDuration(Duration)
      
      person3 = Human$new(1,IncImm=F,IncPfPed=F)
      person3$infectHuman(0,1)
      person3$get_pathogen()$get_Pf()[[1]]$set_mnMZ0(MZ0)
      person3$get_pathogen()$get_Pf()[[1]]$set_Pt(MZ0)
      person3$get_pathogen()$get_Pf()[[1]]$set_mnMaxPD(MaxPD)
      person3$get_pathogen()$get_Pf()[[1]]$set_mnPeakD(PeakD)
      person3$get_pathogen()$get_Pf()[[1]]$set_mnDuration(Duration)
      
      #set gametocyte parameters
      person$get_pathogen()$get_Pf()[[1]]$set_ggr(ggr)
      person$get_pathogen()$get_Pf()[[1]]$set_gdk(gdk)
      
      person2$get_pathogen()$get_Pf()[[1]]$set_ggr(ggr)
      person2$get_pathogen()$get_Pf()[[1]]$set_gdk(gdk)
      
      person3$get_pathogen()$get_Pf()[[1]]$set_ggr(ggr)
      person3$get_pathogen()$get_Pf()[[1]]$set_gdk(gdk)
      
      #set transmission parameters
      person$get_pathogen()$set_teMed(xh)
      person$get_pathogen()$set_teSlope(10^pow)
      person$get_pathogen()$set_teMax(MaxEfficiency)
      
      person2$get_pathogen()$set_teMed(xh)
      person2$get_pathogen()$set_teSlope(10^pow)
      person2$get_pathogen()$set_teMax(MaxEfficiency)
      
      person3$get_pathogen()$set_teMed(xh)
      person3$get_pathogen()$set_teSlope(10^pow)
      person3$get_pathogen()$set_teMax(MaxEfficiency)
      
      #set fever threshold
      person$get_healthState()$set_feverThresh(input$feverThresh)
      
      person2$get_healthState()$set_feverThresh(input$feverThresh)
      
      person3$get_healthState()$set_feverThresh(input$feverThresh)
      
      #simulate untreated person
      simPerson2(person,tFinal,dt,F,F,Delay)
      
      #simulate treated person, no PQ
      simPerson2(person2,tFinal,dt,F,T,Delay)
      
      #simulate treated person, include PQ
      simPerson2(person3,tFinal,dt,T,T,Delay)
      
      #plot results
      layout(matrix(c(1,4,2,4,3,4),3,2,byrow=T))
      PlotTETot(person,1)
      PlotTETot(person2,2)
      PlotTETot(person3,3)
      
      TE1 = sum(person$get_history()$TE)
      TE2 = sum(person2$get_history()$TE)
      TE3 = sum(person3$get_history()$TE)
      
      PlotEffectSize(TE1,TE2,TE3)
    
  })
  
  
}

shinyApp(ui = ui, server = server)

