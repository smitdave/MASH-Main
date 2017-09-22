library(shiny)

source("sinusoidalHazard.R")

shinyServer(function(input, output) {

	output$HazardFunctionPlot <- renderPlot({
		t <- input$thour/24
		P <- input$P
		o <- input$o
		b <- input$b
		p <- input$p
		N <- 10^(input$logN)
		checkIt(t,P,o,b,p,N)
	},height=800)
})


