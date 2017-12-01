library(shiny)
library(shinythemes)
library(matrixStats)
library(plotly)
source ("tent_PfLOME.R")
source("PfTentGUI.R")

##user interface layout
ui = fluidPage(theme = shinytheme("flatly"),


  ##Initial infection
  uiOutput("ddays"),
  uiOutput("gbk"),
  uiOutput("delay"),
  plotOutput("GtPlot")

)



##server layout
server = function(input,output){

  output$ddays = renderUI({
    sliderInput("ddays","Enter the Average Gametocyte Lifespan (in Days)",value=6,min=1,max=15,step=.5)
  })

  output$gbk = renderUI({
    sliderInput("gbk","Enter the Gametocyte Birth Rate",value=.01,min=.001,max=.05,step=.001)
  })

  output$delay = renderUI({
    sliderInput("delay","Enter the Lag (in Days) Between Merozoite Release and Gametocytogenesis",value=10,min=0,max=20)
  })


  output$GtPlot = renderPlot({
    M = read.csv("MzData.txt")
    M = as.matrix(M[,2:ncol(M)])
    G = M[,5:ncol(M)]*0
    Del = matrix(rep(0,nrow(M)*input$delay),nrow=nrow(M))
    G = cbind(Del,G)
    gdk = log(2)/input$ddays
    for(i in (input$delay+8):ncol(G)){
      G[,i-3] = pmax(log10(10^(G[,i-4]-gdk)+input$gbk*10^(M[,i-(input$delay)])),0)
    }
    write.csv(G,"GtData.txt")
    muGam = colMeans(G)
    quantGam = colQuantiles(G,probs=seq(0, 1, 0.1))
    x = (-3):(300+input$delay)

    ##update Gt profile plot
    plot(x,muGam,type="l",xlim=c(-3,300+input$delay),ylim=c(0,10),ylab="log10 Gametocyte Densities",xlab="Time (in Days)",main="Infection Profile - log10 Gametocyte Burden")
    lines(x,quantGam[,3],lty=2, col="blue")
    lines(x,quantGam[,6],lty=2, col="green")
    lines(x,quantGam[,9],lty=2, col="red")
    legend(250, 10, legend=c("Mean", "20th Percentile", "Median", "80th Percentile"),
           col=c("black", "blue","green","red"), lty=c(1,2,2,2), cex=1.2)
  })

}


shinyApp(ui=ui,server=server)
