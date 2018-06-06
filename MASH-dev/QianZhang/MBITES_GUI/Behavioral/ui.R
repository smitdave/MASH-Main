###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     UI for MBITES GUI: Behavioral Options
#     MBITES Team
#     JUN 2018
#
###############################################################################


# check and load library
list_of_packages <- c("shiny", "shinythemes", "shinyjs", "shinydashboard", "miniUI", "ggplot2", "plotly", "markdown", "igraph", "shinyFiles", "stringr", "chorddiag","markovchain", "circlize", "gridExtra", "gridExtra", "reshape2", "RColorBrewer")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)){
  install.packages(new_packages)
}

lapply(list_of_packages, require, character.only = TRUE)

# set up theme

THEME = "flatly"#themeSource="https://bootswatch.com/flatly/"

#########################################################################################
# UI
#########################################################################################
ui = shinyUI(fluidPage(theme = shinytheme(THEME),
                       tags$head(tags$script("
                                             window.onload = function() {
                                             $('#nav a:contains(\"Initialize\")').parent().addClass('hide');
                                             $('#nav a:contains(\"Simulation\")').parent().addClass('hide');
                                             $('#nav a:contains(\"Ecology\")').parent().addClass('hide');
                                             $('#nav a:contains(\"Pathogen\")').parent().addClass('hide');
                                             };
                                             
                                             Shiny.addCustomMessageHandler('activeNavs', function(nav_label) {
                                             $('#nav a:contains(\"' + nav_label + '\")').parent().removeClass('hide');
                                             });
                                             ")),
                       
                       titlePanel(h1("MBITES: Behavioral Options")),
                       navbarPage("Welcome ", id = "nav",
                                  #################################################################################
                                  tabPanel("Get Started", value = 'start',
                                           navlistPanel(widths = c(3,9),
                                                        tabPanel("Overview",
                                                                 includeMarkdown("instructions.md"),
                                                                 img(src='MosquitoBout.jpeg',align="center",width="100%")
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
                                                                                                             "Customized Demo(.rds)" = "cust_demo")),
                                                                                    conditionalPanel(condition = "input.whichDemo == 'cust_demo'",
                                                                                                     fileInput("load_demo", "Choose your .rds file", 
                                                                                                               accept = ".rds")
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
                                                                                               choices = c(".rds" = "rds",".csv" = "csv"), selected = "rds"),
                                                                conditionalPanel(condition = "input.exist_file == 'csv'",
                                                                  fileInput('file_project', 'Choose .csv File',
                                                                    accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))),
                                                                conditionalPanel(condition = "input.exist_file == 'rds'",
                                                                  fileInput('exist_rds', "choose your .rds file", accept = ".rds")
                                                                  ),
                                                                                  hr(),
                                                                                  actionButton("launchgo", "Go!")
                                                                 )
                                                                 
                                                        )
                                           )
                                  ),
                                  
                                  #################################################################################
                                  tabPanel(title = "Initialize", value = 'initial',
                                           uiOutput("panel_initial")
                                  ),

                              
                                  #################################################################################
                                  tabPanel(title = "Simulation", value = "simulation",
                                           uiOutput("sim_panel")
                                  ),

                                  #################################################################################
                                  # tabPanel(title = "Review", value = "review",
                                  #          tableOutput('show_inputs')
                                  # ),

                                  #################################################################################
                                  tabPanel("About",
                                           tabsetPanel(selected = "The Project",
                                                       tabPanel("The Model",includeMarkdown("www/model.md")),
                                                       tabPanel("The Project",includeMarkdown("www/project.md")),
                                                       tabPanel("People",includeMarkdown("www/team.md"))
                                           )
                                  )
                       )
                       )
                       )