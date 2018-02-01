library(shiny)
library(miniUI)
library(ggplot2)
library(shinythemes)
library(shinyjs)
library(shinydashboard)
library(plotly)
library(markdown)
library(igraph)
library(jsonlite)
library(shinyFiles)
library(stringr)
library(chorddiag)
library(markovchain)
library(circlize)
library(gridExtra)
library(igraph)
library(reshape2)
library(RColorBrewer)


################################################################################################################
# CONSTANTS
################################################################################################################
INITIAL_GUI_WIDTH = 2000
INITIAL_GUI_HEIGHT = 1000
VAR_DESCR_COL_WIDTH = 3
VAR_SLIDE_COL_WIDTH = 4
THEME = "flatly"#themeSource="https://bootswatch.com/flatly/"

#########################################################################################
# UI
#########################################################################################
ui = shinyUI(fluidPage(theme = shinytheme(THEME),
                       tags$head(tags$script("
                                             window.onload = function() {
                                             $('#nav a:contains(\"Options\")').parent().addClass('hide');
                                             $('#nav a:contains(\"Simulation\")').parent().addClass('hide');
                                             $('#nav a:contains(\"Bouts\")').parent().addClass('hide');
                                             $('#nav a:contains(\"Landscape\")').parent().addClass('hide');
                                             $('#nav a:contains(\"Ecology\")').parent().addClass('hide');
                                             $('#nav a:contains(\"Pathogen\")').parent().addClass('hide');
                                             };
                                             
                                             Shiny.addCustomMessageHandler('activeNavs', function(nav_label) {
                                             $('#nav a:contains(\"' + nav_label + '\")').parent().removeClass('hide');
                                             });
                                             ")),
                       
                       titlePanel(h1("MBITES GUI")),
                       navbarPage("Welcome ", id = "nav",
                                  #################################################################################
                                  tabPanel("Get Started", value = 'start',
                                           navlistPanel(widths = c(3,9),
                                                        tabPanel("Overview",
                                                                 includeMarkdown("instructions.md"),
                                                                 img(src='boutFull.png',align="center",width="50%")
                                                        ),
                                                        ###########################################################################
                                                        tabPanel("Launch a project",
                                                                 h2("Welcome to MBITES!"),
                                                                 hr(),
                                                                 h4("To launch your project, please choose:"),
                                                                 tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'))),
                                                                 radioButtons("project", "",
                                                                              c("First time user (Run our demo project)" = "demo",
                                                                                "Start a new project" = "new",
                                                                                "Work on an existing project" = "exist")),
                                                                 conditionalPanel(condition = "input.project == 'demo'",
                                                                                  wellPanel(
                                                                                    radioButtons("whichDemo", label = "Start with:", 
                                                                                                 choices = c("Default Demo" = "default_demo",
                                                                                                             "Customized Demo" = "cust_demo")),
                                                                                    conditionalPanel(condition = "input.whichDemo == 'cust_demo'",
                                                                                                     fileInput("load demo", "Choose your json file", 
                                                                                                               accept = ".json")
                                                                                    )
                                                                                  ),
                                                                                  actionButton("createDemoFolder", "Run Demo")
                                                                 ),
                                                                 conditionalPanel(condition = "input.project == 'new'",
                                                                                  textInput("new_proj_name", "Name your project/folder", "new_project"),
                                                                                  actionButton("createNewFolder", "Create")
                                                                 ),
                                                                 conditionalPanel(condition = "input.project == 'exist'",
                                                                                  radioButtons("exist_file", label = "select your file format:",
                                                                                               choices = c(".json" = "json",
                                                                                                           ".csv" = "csv"), selected = "json"),
                                                                                  conditionalPanel(condition = "input.exist_file == 'csv'",
                                                                                      fileInput('file_project', 'Choose .csv File',
                                                                                                accept=c('text/csv',
                                                                                                         'text/comma-separated-values,text/plain',
                                                                                                         '.csv')),
                                                                                      column(6,
                                                                                      wellPanel(checkboxInput('header_project', 'Header', TRUE),
                                                                                                radioButtons('sep_project', 'Separator',
                                                                                                             c(Comma=',',
                                                                                                               Semicolon=';',
                                                                                                               Tab='\t'),
                                                                                                             ',')))),
                                                                                  conditionalPanel(condition = "input.exist_file == 'json'",
                                                                                    fileInput('exist_json', "choose your json file", accept = ".json")
                                                                                    ),
                                                                                  hr(),
                                                                                  actionButton("launchgo", "Go!")
                                                                 )
                                                                 
                                                        )
                                           )
                                  ),
                                  
                                  #################################################################################
                                  tabPanel(title = "Landscape", value = 'landscape',
                                           uiOutput("panel_landscape")
                                  ),
                                  #################################################################################
                                  tabPanel(title = "Simulation", value = "simulation",
                                           uiOutput("sim_panel")
                                  ),
                                  
                                  #################################################################################
                                  tabPanel(title = "Options", value = "options",
                                          uiOutput("panel_options")
                                           ),
                                  #################################################################################
                                  tabPanel(title = "Bouts", value = "bouts",
                                           useShinyjs(),
                                           uiOutput("panel_bouts")
                                  ),
                                  #################################################################################
                                  tabPanel(title = "Pathogen", value = "pathogen",
                                           sidebarLayout(position = "right",
                                                         sidebarPanel(style = "overflow-y:scroll; max-height: 600px",
                                                                      helpText('Test')
                                                         ),
                                                         mainPanel(
                                                           helpText("test output")
                                                         )
                                           )
                                  ),
                                  
                                  
                                  #################################################################################
                                  tabPanel(title = "Ecology", value = "ecology",
                                           sidebarLayout(position = "right",
                                                         sidebarPanel(style = "overflow-y:scroll; max-height: 600px",
                                                                      checkboxInput("showEmerge", "Emerge", FALSE),
                                                                      conditionalPanel(condition = "input.showEmerge",
                                                                                       helpText("Parameters")
                                                                      ),
                                                                      checkboxInput("showEL4P", "EL4P", FALSE),
                                                                      conditionalPanel(condition = "input.showEL4P",
                                                                                       helpText("Parameters")
                                                                      )
                                                         ),
                                                         mainPanel(
                                                           helpText("test output")
                                                         )
                                           )
                                  ),
                                  #################################################################################
                                  tabPanel("About",
                                           tabsetPanel(selected = "The Project",
                                                       tabPanel("The Model",includeMarkdown("model.md")),
                                                       tabPanel("The Project",includeMarkdown("project.md")),
                                                       tabPanel("People",includeMarkdown("team.md"))
                                           )
                                  )
                       )
                       )
                       )