library(shiny)
library(matrixStats)
source ("tent_PfLOME.R")

##user interface layout
ui = fluidPage(

  ##shape of lgo10 infection profile - tent function
  radioButtons(inputId = "shape",
               label = "Log Parasite Density Shape",
               choices = c("Tent" = "tent",
                           "Red Tent" = "rtent")),

  ##Initial infection
  radioButtons(inputId = "Mz0",
               label = "Distribution of Initial Merozoites",
               choices = c("Normal" = "norm",
                           "Exponential" = "exp")),
  uiOutput("muMz0"),
  uiOutput("sigmaMz0"),
  plotOutput("Mz0Plot"),

  ##peak of infection
  radioButtons(inputId = "peakt",
               label = "Distribution of Times of Peak of Infection",
               choices = c("Log Normal" = "lnorm",
                           "Normal" = "norm")),
  uiOutput("muPeakt"),
  uiOutput("sigmaPeakt"),
  plotOutput("peaktPlot"),

  ##length of infection
  radioButtons(inputId = "tfin",
               label = "Distribution of Times of End of Infection",
               choices = c("Geometric" = "geom")),
  uiOutput("muTfin"),
  plotOutput("tfinPlot"),

  ##maximum log10 parasite burden
  radioButtons(inputId = "maxPD",
               label = "Distribution of Maximum Parasite Density",
               choices = c("Normal" = "norm")),
  uiOutput("muMaxPD"),
  uiOutput("sigmaMaxPD"),
  plotOutput("maxPDPlot"),

  ##Infection Profile Stochastic Projections
  radioButtons(inputId = "display",
               label = "Which plot would you like to display?",
               choices = c("Mean + Quantiles" = "meanq",
                           "Mean of Each Parameter" = "parmean")),
  plotOutput("PDPlot")
)



##server layout
server = function(input,output){

  ##update for profile - include noise?
  noisy = renderUI({
    noisy=F
    if(input$shape=="rtent"){
      noisy=T
    }
  })

  ##update expected initial Mz population
  output$muMz0 = renderUI({
    if(input$Mz0=="norm" | input$Mz0=="exp") {
      numericInput("muMz0","Enter the mean numer of initial Merozoites",value=4.2)
    }
  })

  ##update sd of initial Mz population
  output$sigmaMz0 = renderUI({
    if(input$Mz0=="norm") {
      numericInput("sigmaMz0","Enter the SD of the number of initial Merozoites",value=.1)
    }
  })

  ##update expected time of peak Mz burden
  output$muPeakt = renderUI({
    if(input$peakt=="lnorm"|input$peakt=="norm") {
      numericInput("muPeakt","Enter the Mean log Time Past 18 Days of the Peak Size of the Merozoite Population",value=1.1)
    }
  })

  ##update sd of time of peak Mz burden
  output$sigmaPeakt = renderUI({
    if(input$peakt=="lnorm"|input$peakt=="norm") {
      numericInput("sigmaPeakt","Enter the SD of the Time of the Peak Size of the Merozoite Population",value=.5)
    }
  })

  ##update expected length of infection
  output$muTfin = renderUI({
    if(input$tfin=="geom") {
      numericInput("muTfin","Enter the Mean Length of Infection (in days)",value=200)
    }
  })

  ##update expected maximum Mz burden
  output$muMaxPD = renderUI({
    if(input$maxPD=="norm"){
      numericInput("muMaxPD","Enter Mean log10 Peak Population Density of Merozoites",value=10.5)
    }
  })

  ##update sd of maximum Mz burden
  output$sigmaMaxPD = renderUI({
    if(input$maxPD=="norm"){
      numericInput("sigmaMaxPD","Enter the SD of the log10 Peak Population Density of Merozoites",value=.5)
    }
  })

  ##update distribution of initial Mz burden
  output$Mz0Plot = renderPlot({
    m = input$muMz0
    if(input$Mz0=="norm"){
      s = input$sigmaMz0
      x = seq(-3*s,3*s,.1*s)+m
      plot(x,dnorm(x,m,s),type="l",xlab="log10 Numbers of Merozoites", ylab="Probability", main="Distribution of initial Merozoite Burden")
    }
    if(input$Mz0=="exp"){
      x = seq(0,5*m,.1)
      plot(x,dexp(x,1/m),type="l")
    }
  })

  ##update distribution of time of peak burden
  output$peaktPlot = renderPlot({
    s = input$sigmaPeakt
    m = input$muPeakt
    if(input$peakt=="lnorm"){
      x = seq(.1,5*m,.1)
      plot(x,dlnorm(x,m,s),type="l",xlab = "Time (in Days)",ylab="Probability",main="Distribution of Time of Peak Merozoite Burden")
    }
    if(input$peakt=="norm"){
      x = seq(-3*s,3*s,.1*s)+m
      plot(x,dnorm(x,m,s),type="l")
    }
  })

  ##update distribution of peak burden
  output$maxPDPlot = renderPlot({
    s = input$sigmaMaxPD
    m = input$muMaxPD
    if(input$maxPD=="norm"){
      x = seq(-3*s,3*s,.1*s)+m
      plot(x,dnorm(x,m,s),type="l",xlab="Peak log10 Merozoite Numbers", ylab = "Probability", main="Distribution of Peak log10 Merozoite Burden")
    }
  })

  ##update distribution of length of infection
  output$tfinPlot = renderPlot({
    m = input$muTfin
    x = seq(0,1.5*m,1)
    plot(x,dgeom(x,1/m),type="l",xlab="Length of Infection (in Days)",ylab="Probability",main="Distribution of Length of Infection")
  })

  ##update PD profile plot
  output$PDPlot = renderPlot({
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
        M[i,5:(length(x)+4)] = tent(x,M[i,1],M[i,4],M[i,2],M[i,3])
      }
      write.csv(M, "MzData.txt")
      mutent = colMeans(M[1:n,5:(length(x)+4)])
      quanttent = colQuantiles(M[1:n,5:(length(x)+4)],probs=seq(0, 1, 0.1))
      plot(x,mutent,type="l",xlim=c(-3,300),ylim=c(0,12),ylab="log10 Parasite Densities",xlab="Time (in Days)",main="Infection Profile - log10 Merozoite Burden")
      lines(x,quanttent[,3], lty=2, col="blue")
      lines(x,quanttent[,6], lty=2, col="green")
      lines(x,quanttent[,9], lty=2, col="red")
      legend(250, 12, legend=c("Mean", "20th Percentile", "Median", "80th Percentile"),
             col=c("black", "blue","green","red"), lty=c(1,2,2,2), cex=1.2)
    }

    if(input$display == "meanq") plotTent(500,Mz0,input$muMz0,input$sigmaMz0,peakt,input$muPeakt,input$sigmaPeakt,tfin,input$muTfin,maxPD,input$muMaxPD,input$sigmaMaxPD)
    if(input$display == "parmean") {
      x = (-3):(1*(300))/1
      plot(x,tent(x,input$muMz0,input$muMaxPD,input$muPeakt,input$muTfin),type="l",xlab="Time (in Days)", ylab="log10 Merozoite Burden", main="Infection Profile - log10")
    }
  })
}


shinyApp(ui=ui,server=server)
