################################################################################################################
# MBITES: Gadget
# October 2017
# Instructions: set the working directory to the folder this file is stored in
################################################################################################################
library(shiny)
library(miniUI)
library(ggplot2)
library(shinythemes)
library(shinyjs)
library(shinydashboard)
library(plotly)
library(markdown)
library(igraph)
if(system("whoami",intern=TRUE)=="chipdelmal"){
  DIR="/Users/chipdelmal/Documents/Github/MASH-MAIN/MASH-dev/HectorSanchez/MBITES_GUI/New/"
  setwd(DIR)
}else if(system("whoami",intern=TRUE)=="QIAN"){
  DIR="/Users/QIAN/Desktop/Documents/Github/MASH-MAIN/MASH-dev/HectorSanchez/MBITES_GUI/New/"
  setwd(DIR)
}
################################################################################################################
# CONSTANTS
################################################################################################################
INITIAL_GUI_WIDTH=1250
INITIAL_GUI_HEIGHT=1000
THEME="flatly"
################################################################################################################
mbitesGadget=function(...){
  #########################################################################################
  # UI
  #########################################################################################
  ui=shinyUI(fluidPage(theme=shinytheme(THEME),
      titlePanel(h1("MBITES Gadget")),
      navbarPage("Select an Activity: ",
          #################################################################################
          tabPanel("Setup Parameters",
              navlistPanel(
                "Select:",
                tabPanel("Welcome",
                  includeMarkdown("instructions.md"),
                  img(src='boutFull.png',align="center",width="50%")
                ),
                tabPanel("Bouts",
                  tabsetPanel(
                     tabPanel("F","Test"),
                     tabPanel("L","Test"),
                     tabPanel("B","Test"),
                     tabPanel("R","Test"),
                     tabPanel("O","Test"),
                     tabPanel("S","Test"),
                     tabPanel("M","Test"),
                     tabPanel("E","Test")
                  ),
                  h1(""),
                  actionButton("button","An action button",width="50%"),
                  actionButton("button","An action button",width="50%")
                ),
                tabPanel("Landscape")
              )
          ),
          #################################################################################
          tabPanel("Run the Model"),
          #################################################################################
          tabPanel("Analyse Data"),
          #################################################################################
          tabPanel("Read More",
            tabsetPanel(selected="The Project",
               tabPanel("The Model",includeMarkdown("model.md")),
               tabPanel("The Project",includeMarkdown("project.md")),
               tabPanel("People",includeMarkdown("team.md"))
          )
        )
      )
    )
  )
  #########################################################################################
  # SERVER
  #########################################################################################
  server <- function(input, output, session) {
    cat(getwd())
    output$plot <- renderPlot({
      ggplot(data, aes_string(xvar, yvar)) + geom_point()
    })
    observeEvent(input$done, {
      stopApp(brushedPoints(data, input$brush))
    })
  }
  #########################################################################################
  # RUN
  #########################################################################################
  runGadget(ui,server,viewer=dialogViewer("ggbrush",width=INITIAL_GUI_WIDTH,height=INITIAL_GUI_HEIGHT))
}

mbitesGadget()