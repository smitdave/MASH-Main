################################################################################################################
# MBITES-BRO GUI
# HMSC/Sean Wu
################################################################################################################
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$plotMosquitoLifespans=renderPlotly({PlotLyMosquitoLifespans(dataIn)})
  output$plotBMInterval=renderPlotly({PlotLyBMInterval(dataIn)})
  output$plotHumanBMInterval=renderPlotly({PlotLyHumanBMInterval(dataIn)})
  output$plotHumanBM=renderPlotly({PlotLyHumanBM(dataIn)})
  output$plotLandscape=renderPlotly({plotLandscape(LANDSCAPE)})
  output$plotKernels=renderPlotly({KernelPlots(LANDSCAPE$sugarSites,LANDSCAPE$feedSites,N=20)})

  # mat = matrix(sample(18, 18), 9,9)
  # colors=list("#555555","#95E455","pink","red","purple","cyan","blue","yellow","grey")
  # colors=list("red","#555555","grey","pink","cyan","#95E455","blue","purple","yellow")
  # names=list("B","D","E","F","L","M","O","R","S")
  # rownames(mat)=names
  # colnames(mat)=names
  # output$chordDiagram=renderChorddiag({chorddiag(mat,width="1000px",height="1000px",groupColors=colors,chordedgeColor=NULL)})
  # output$matrixPlotTransitions=renderPlotly({plot_ly(z=mat,type="heatmap",colorscale="Greys")})
  # print(mat)

  observeEvent(input$runModel,{
    cat("Running Model...\n")
    PP<<-list(
      bfa.p=input$bfa.p, bfa.s=input$bfa.s, bfa.t=input$bfa.t, Q=input$Q,
      ppr.p=input$ppr.p, ppr.t=input$ppr.t, reFeed=input$reFeed,
      ela.p=input$ela.p, ela.s=input$ela.s, ela.t=input$ela.t
    )
    #PP=DHM.Basic.Pset(ela.p=.5)
    print(PP)
    dataIn<<-DHM.Basic.Cohort(PP)
    output$plotMosquitoLifespans=renderPlotly({PlotLyMosquitoLifespans(dataIn)})
    output$plotBMInterval=renderPlotly({PlotLyBMInterval(dataIn)})
    output$plotHumanBMInterval=renderPlotly({PlotLyHumanBMInterval(dataIn)})
    output$plotHumanBM=renderPlotly({PlotLyHumanBM(dataIn)})
    print(dataIn)
    #tmpFile <- tempfile(fileext = ".png")
    #browseURL(tmpFile)
  })
  observeEvent(input$exportPlots,{
    export(PlotLyMosquitoLifespans(dataIn),file="./Plots/MosquitoLifespans.png")
    export(PlotLyBMInterval(dataIn),file="./Plots/BMInterval.png")
    export(PlotLyHumanBMInterval(dataIn),file="./Plots/HumanBMInterval.png")
    export(PlotLyHumanBM(dataIn),file="./Plots/HumanBM.png")
  })
  observeEvent(input$randomLandscape,{
    LANDSCAPE<<-switch(input$pointGen,
      "poisson"=makeLandscape(nF=input$nF,nA=input$nA,nS=input$nS,nM=input$nM,aquaMod=input$aquaMod,pointGen=pointsPoisson,hhSize=input$hhSize,hhMin=input$hhMin,aquaSD=0.025),
      "clustered"=makeLandscape(nF=input$nF,nA=input$nA,nS=input$nS,nM=input$nM,aquaMod=input$aquaMod,pointGen=pointsClustered,hhSize=input$hhSize,hhMin=input$hhMin,aquaSD=0.025),
      "lattice"=makeLandscape(nF=input$nF,nA=input$nA,nS=input$nS,nM=input$nM,aquaMod=input$aquaMod,pointGen=pointsLattice,hhSize=input$hhSize,hhMin=input$hhMin,aquaSD=0.025),
      "overdispersed"=makeLandscape(nF=input$nF,nA=input$nA,nS=input$nS,nM=input$nM,aquaMod=input$aquaMod,pointGen=pointsOverdispersed,hhSize=input$hhSize,hhMin=input$hhMin,aquaSD=0.025)
    )
    output$plotLandscape=renderPlotly({plotLandscape(LANDSCAPE)})
    source=switch(input$kernelSource,"1"=LANDSCAPE$sugarSites,"2"=LANDSCAPE$swarmSites,"3"=LANDSCAPE$aquaSites,"4"=LANDSCAPE$feedSites)
    destiny=switch(input$kernelSource,"1"=LANDSCAPE$sugarSites,"2"=LANDSCAPE$swarmSites,"3"=LANDSCAPE$aquaSites,"4"=LANDSCAPE$feedSites)
    output$plotKernels=renderPlotly({KernelPlots(source,destiny,N=20)})
  })
  observeEvent(input$kernelSource,{
    source=switch(input$kernelSource,"1"=LANDSCAPE$sugarSites,"2"=LANDSCAPE$swarmSites,"3"=LANDSCAPE$aquaSites,"4"=LANDSCAPE$feedSites)
    destiny=switch(input$kernelSource,"1"=LANDSCAPE$sugarSites,"2"=LANDSCAPE$swarmSites,"3"=LANDSCAPE$aquaSites,"4"=LANDSCAPE$feedSites)
    output$plotKernels=renderPlotly({KernelPlots(source,destiny,N=20)})
  })
  observeEvent(input$kernelDestiny,{
    source=switch(input$kernelSource,"1"=LANDSCAPE$sugarSites,"2"=LANDSCAPE$swarmSites,"3"=LANDSCAPE$aquaSites,"4"=LANDSCAPE$feedSites)
    destiny=switch(input$kernelSource,"1"=LANDSCAPE$sugarSites,"2"=LANDSCAPE$swarmSites,"3"=LANDSCAPE$aquaSites,"4"=LANDSCAPE$feedSites)
    output$plotKernels=renderPlotly({KernelPlots(source,destiny,N=20)})
  })
  observeEvent(input$loadJSON,{
    no_cores <- detectCores() - 1
    print(no_cores)
    print("Testing JSON")
    print(getwd())
    bionomics=importBionomics(directory=paste0(getwd(),"/"))
    history=importHistory(directory=paste0(getwd(),"/"))
    #print(bionomics)
    sampledIndex=runif(1,min=1,max=length(history))
    #print(as.numeric(input$dissectedMosquito))
    output$dissectedMosquito <- renderUI({selectInput("dissectedMosquito", "Mosquito ID", 1:length(bionomics))})
    output$dissectionPlot=renderPlotly({dissectMosquito(history[[as.numeric(input$dissectedMosquito)]])})
    #output$chordDiagram=renderChorddiag({chorddiagramPopulationStates(history)})
    output$chordDiagram2=renderPlot({circlizeStatesTransitionMatrix(history)})
    output$chordDiagram3=renderPlot({circlizeStatesTransitionMatrixNormalized(history)})
    output$matrixPlot=renderPlotly({plotStatesTransitionsMatrix(history)})
    output$statesNetwork=renderPlot({graphStatesHistoryInMosquitoPopulation(history)})
  })
  observeEvent(input$exportParameters, {
    PP<<-list(
      bfa.p=input$bfa.p, bfa.s=input$bfa.s, bfa.t=input$bfa.t, Q=input$Q,
      ppr.p=input$ppr.p, ppr.t=input$ppr.t, reFeed=input$reFeed,
      ela.p=input$ela.p, ela.s=input$ela.s, ela.t=input$ela.t
    )
    print("XML Created")
    print(PP)
    dput(PP,file="CONFIG_MBITES_PP.R")
    #exportMBITESMosquitoParameters(PP)
  })
  observeEvent(input$importParameters, {
    print("XML Loaded")
    #PP<<-importMBITESMosquitoParameters()
    PP<<-dget(file="CONFIG_MBITES_PP.R")
    print(PP)
  })
})

