# Fun Hazard Function Shiny interface
# Dave Smith & Bobby Reiner
# 2016/01/25

# Adapted very loosely from shinytse7 by Andy South

#to run type this in R console
#library(shiny)
#runApp('DHM')

if (!require("devtools")) install.packages("devtools")
library(devtools)
if (!require("shinyTable")) install_github("shinyTable", "trestletech")

list.of.packages <- c("deldir", "stpp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(markdown)
library(shinyTable) #for htable editable tables
library(shinythemes) #for new themes


# Define UI 
#shinyUI(fluidPage(
shinyUI(fluidPage(theme = shinytheme("cerulean"),  #fine, light blue header, buttons white
#shinyUI(fluidPage(theme = shinytheme("cosmo"), #ugly black header, black buttons
#shinyUI(fluidPage(theme = shinytheme("flatly"), #close to before, dk blue header, buttons medium grey
#shinyUI(fluidPage(theme = shinytheme("journal"), #ugly font
#shinyUI(fluidPage(theme = shinytheme("readable"), #ok, white background, blue text
#shinyUI(fluidPage(theme = shinytheme("spacelab"), #nice, greay header, buttons nr black
#shinyUI(fluidPage(theme = shinytheme("united"), #nice orange/red header, grey buttons, good for a temporary change !

  #navbarPage sets up navbar, title appears on left
  navbarPage("Fun Hazard Function Simulator",
		tabPanel("Hazard Function Parameters",
			###########################
			##   Hazard Function Parameters  ##
			###########################
			sidebarLayout(        
				sidebarPanel(
					sliderInput("thour",
						"Begining of 24 hour window",
						min = 0,
             max = 24,
             value = 1.4,
						step= 0.1
					),
					sliderInput("P",
						"Proportion of the total hazard taking place in 24 hours",
						min = 0,
             max = 1,
             value = .98,
						step= 0.01
					),
					sliderInput("o",
						"Timing of max activity",
						min = 0,
             max = 24,
             value = 12
					),
					sliderInput("b",
						"Not sure what b is",
						min = 0,
             max = 2,
             value = 1.1,
						step= 0.1
					),
					sliderInput("p",
						"Not sure what p is",
						min = 0,
             max = 4,
             value = 2
					),
					sliderInput("logN",
						"log (base 10) of popultion size",
						min = 1,
             max = 4,
             value = 1
					)
				), # end sidebarPanel
     
				mainPanel(
					plotOutput("HazardFunctionPlot") 
				) # end mainPanel              
			) # end pageWithSidebar        
		)
	)
)
)
