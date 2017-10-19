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
INITIAL_GUI_WIDTH=2000
INITIAL_GUI_HEIGHT=1000
VAR_DESCR_COL_WIDTH=3
VAR_SLIDE_COL_WIDTH=4
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
                ###########################################################################
                tabPanel("Bouts",
                  tabsetPanel(
                     ######################################################################
                     tabPanel("F",
                              fluidRow(h1("")),
                              fluidRow(
                                column(VAR_DESCR_COL_WIDTH,helpText("bfa.p: Probability of surviving a blood-feed attempt.")),
                                column(VAR_SLIDE_COL_WIDTH,sliderInput("bfa.p",NULL,min=0,max=1,value=.9))
                              ),
                              fluidRow(
                                column(VAR_DESCR_COL_WIDTH,helpText("bfa.s: Probability of success in a blood-feed attempt.")),
                                column(VAR_SLIDE_COL_WIDTH,sliderInput("bfa.s",NULL,min=0,max=1,value=.3))
                              ),
                              fluidRow(
                                column(VAR_DESCR_COL_WIDTH,helpText("bfa.t: Mean time between blood-feed attempts (days).")),
                                column(VAR_SLIDE_COL_WIDTH,sliderInput("bfa.t",NULL,min=0,max=5,value=.65,step=.05))
                              ),
                              fluidRow(
                                column(VAR_DESCR_COL_WIDTH,helpText("Q: Proportion of Bites on Humans.")),
                                column(VAR_SLIDE_COL_WIDTH,sliderInput("Q",NULL,min=0,max=1,value=.9))
                              ),
                              h1(),h1()
                      ),
                      #####################################################################
                      tabPanel("L","Test"),
                      #####################################################################
                      tabPanel("B","Test"),
                      #####################################################################
                      tabPanel("R","Test"),
                      #####################################################################
                      tabPanel("O","Test"),
                      #####################################################################
                      tabPanel("S","Test"),
                      #####################################################################
                      tabPanel("M","Test"),
                      #####################################################################
                      tabPanel("E","Test")
                  ),
                  h1(""),
                  h1(""),
                  h1(""),
                  tags$hr(),
                  fluidRow(
                    column(6,actionButton("button","Export Bouts Parameters",width="100%")),
                    column(6,actionButton("button","Read Bouts Parameters",width="100%"))
                  )
                ),
                ###########################################################################
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