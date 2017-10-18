library(shiny)
library(shinythemes)
library(matrixStats)
library(plotly)
source ("tent_PfLOME.R")
source("PfTentGUI.R")

##user interface layout
ui = fluidPage(theme = shinytheme("flatly"),
               
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
)



##server layout
server = function(input,output){
  
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
  
}


shinyApp(ui=ui,server=server)
