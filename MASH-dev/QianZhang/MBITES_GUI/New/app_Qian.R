################################################################################################################
# MBITES: Gadget
# October 2017
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
}else if(system("whoami",intern=TRUE)=="qianzh"){
  DIR="/Users/qianzh/project/MASH-Main/MASH-dev/QianZhang/MBITES_GUI/NEW"
  setwd(DIR)
}
################################################################################################################
# CONSTANTS
################################################################################################################
INITIAL_GUI_WIDTH=2000
INITIAL_GUI_HEIGHT=1000
VAR_DESCR_COL_WIDTH=3
VAR_SLIDE_COL_WIDTH=4
THEME="flatly"#themeSource="https://bootswatch.com/flatly/"
################################################################################################################
mbitesGadget=function(...){
  #########################################################################################
  # UI
  #########################################################################################
  ui=shinyUI(fluidPage(theme=shinytheme(THEME),
      tags$head(tags$script("
        window.onload = function() {
            $('#nav a:contains(\"Options\")').parent().addClass('hide');
            $('#nav a:contains(\"Bouts\")').parent().addClass('hide');
            $('#nav a:contains(\"Landscape\")').parent().addClass('hide');
            $('#nav a:contains(\"Ecology\")').parent().addClass('hide');
        };

        Shiny.addCustomMessageHandler('activeNavs', function(nav_label) {
            $('#nav a:contains(\"' + nav_label + '\")').parent().removeClass('hide');
        });
   ")),
                       
      titlePanel(h1("MBITES Gadget")),
      navbarPage("Welcome ", id = "nav",
          #################################################################################
          tabPanel("Get Started",
              navlistPanel(widths = c(2,4),
                tabPanel("Overview",
                  includeMarkdown("instructions.md"),
                  img(src='boutFull.png',align="center",width="50%")
                ),
                ###########################################################################
                tabPanel("Launch a project",
                  h2("Welcome to MBITES!"), 
                  hr(),
                  h4("To launch your project, please choose:"),
                  radioButtons("project", "", 
                    c("First time user (Run our demo project)" = "demo",
                      "Start a new project" = "new",
                      "Work on an existing project" = "exist")),
                  uiOutput("prepath.box"),
                  hr(),
                  actionButton("launchgo", "Go!")
                )
              )
           ),
          #################################################################################
          tabPanel(title = "Options", value = "options",
            fluidRow(
              tags$hr(),
              column(12,radioButtons("boutbutton", "", 
                                     c("Love MASH" = "love",
                                       "MASH is awesome" = "awesome"))
            )
          )),
          #################################################################################
          tabPanel(title = "Bouts",   value = "bouts",              
            helpText("test")
          ),
          #################################################################################
          tabPanel("Landscape",                  
            fluidRow(
              tags$hr(),
              column(12,actionButton("button","Load Data",width="100%",class="btn btn-success"))
            )
          ),
          #################################################################################
          tabPanel("Ecology",                  
            fluidRow(
              tags$hr(),
              column(12,actionButton("button","Load Data",width="100%",class="btn btn-success"))
            )
          ),
          #################################################################################
          tabPanel("About",
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
    observe({
        if (input$launchgo > 0) {
            session$sendCustomMessage('activeNavs', 'Options')
        }
    })

    observe({
        if (input$launchgo > 0) {
            session$sendCustomMessage('activeNavs', 'Bouts')
        }
    })

    observe({
        if (input$launchgo > 0) {
            session$sendCustomMessage('activeNavs', 'Landscape')
        }
    })
    output$prepath.box <- renderUI({
      if(input$project == "exist"){
        textInput("prepath", "Please provide the previous working directory (the folder contains .json file)", "")
      }
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