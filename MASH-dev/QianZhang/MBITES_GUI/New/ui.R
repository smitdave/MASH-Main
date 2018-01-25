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
                       
                       titlePanel(h1("MBITES Gadget")),
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
                                                                                  actionButton("createDemoFolder", "Run Demo")
                                                                 ),
                                                                 conditionalPanel(condition = "input.project == 'new'",
                                                                                  textInput("new_proj_name", "Name your project/folder", "new_project"),
                                                                                  actionButton("createNewFolder", "Create")
                                                                 ),
                                                                 conditionalPanel(condition = "input.project == 'exist'",
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
                                                                                                         ',')),
                                                                                  hr(),
                                                                                  actionButton("launchgo", "Go!"))
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
                                           fluidPage(
                                             helpText("Please set parameters here. To save the selected parameters, go to 
                                                      'Bouts' panel and click the button when both panels are done"),
                                             navlistPanel(widths = c(2,10),
                                                          # #########################################################################
                                                          #  tabPanel("Mosquito Features",
                                                          #    numericInput("N_female", "Number of Female Mosquitoes at Initialization", value = 50, min = 0, max = NA, step = 1),
                                                          #    numericInput("N_male", "Number of Male Mosquitoes at Initialization", value = 50, min = 0, max = NA, step = 1)
                                                          #    # helpText("ix_female"),
                                                          #    # helpText("ix_male"),
                                                          #    # helpText("genotype_female"),
                                                          #    # helpText("genotype_male")
                                                          #    ),
                                                          
                                                          #########################################################################
                                                          tabPanel("Resting Time",
                                                                   tabsetPanel(
                                                                     tabPanel("F",
                                                                              column(4,
                                                                                     h5("Mean Time Elapsed: "),
                                                                                     fluidRow(
                                                                                       column(4,selectInput("F_time_h", label = "Hours", choices = seq(0,24,1) , selected = 0, width = "100%")),
                                                                                       column(4,selectInput("F_time_m", label = "Minutes", choices = seq(0,55,5), selected = 30, width = "100%"))
                                                                                     ),
                                                                                     radioButtons("F_dist", "Distribution type:",
                                                                                                  c("Exponentional" = "exp",
                                                                                                    "Gamma" = "gamma"), inline = TRUE),
                                                                                     # conditionalPanel(condition = "input.F_dist == 'exp'",
                                                                                     #   checkboxInput("F_diur", "Diurnal Pattern", FALSE)
                                                                                     #   ),
                                                                                     # conditionalPanel(condition = "input.F_diur && input.F_dist == 'exp'",
                                                                                     #   sliderInput("F_diur_now", "Time when the waiting period starts (in hours)", min = 0, max = 24, value = 3, step = 0.25),
                                                                                     #   sliderInput("F_diur_peak", "Peak activity level (in hours)", min = 0, max = 24, value = 2, step = 0.25)
                                                                                     #   ),
                                                                                     conditionalPanel(condition = "input.F_dist == 'gamma'",
                                                                                                      sliderInput("f_wt_gamma_shape", "Shape Parameter", min = 0, max = 20, value = 3, step = 1)
                                                                                     ),
                                                                                     hr(),
                                                                                     uiOutput('f_time_slider')
                                                                                     
                                                                              ),
                                                                              
                                                                              column(6,
                                                                                     plotOutput("F_wt_plot_option")
                                                                              )
                                                                     ),
                                                                     
                                                                     tabPanel("B",
                                                                              column(4,
                                                                                     h5("Mean Time Elapsed: "),
                                                                                     fluidRow(
                                                                                       column(4,selectInput("B_time_h", label = "Hours", choices = seq(0,24,1) , selected = 0, width = "100%")),
                                                                                       column(4,selectInput("B_time_m", label = "Minutes", choices = seq(0,55,5), selected = 30, width = "100%"))
                                                                                     ),
                                                                                     radioButtons("B_dist", "Distribution type:",
                                                                                                  c("Exponentional" = "exp",
                                                                                                    "Gamma" = "gamma"), inline = TRUE),
                                                                                     # conditionalPanel(condition = "input.B_dist == 'exp'",
                                                                                     #   checkboxInput("B_diur", "Diurnal Pattern", FALSE)
                                                                                     #   ),
                                                                                     # conditionalPanel(condition = "input.B_diur && input.B_dist == 'exp'",
                                                                                     #   sliderInput("B_diur_now", "Time when the waiting period starts (in hours)", min = 0, max = 24, value = 3, step = 0.25),
                                                                                     #   sliderInput("B_diur_peak", "Peak activity level (in hours)", min = 0, max = 24, value = 2, step = 0.25)
                                                                                     #   ),
                                                                                     conditionalPanel(condition = "input.B_dist == 'gamma'",
                                                                                                      sliderInput("b_wt_gamma_shape", "Shape Parameter", min = 0, max = 20, value = 3, step = 1)
                                                                                     ),
                                                                                     hr(),
                                                                                     uiOutput('b_time_slider')
                                                                              ), 
                                                                              column(6,
                                                                                     plotOutput("B_wt_plot_option")
                                                                              )
                                                                     ),
                                                                     
                                                                     tabPanel("R",
                                                                              column(4,
                                                                                     h5("Mean Time Elapsed: "),
                                                                                     fluidRow(
                                                                                       column(4,selectInput("R_time_h", label = "Hours", choices = seq(0,24,1) , selected = 0, width = "100%")),
                                                                                       column(4,selectInput("R_time_m", label = "Minutes", choices = seq(0,55,5), selected = 30, width = "100%"))
                                                                                     ),
                                                                                     radioButtons("R_dist", "Distribution type:",
                                                                                                  c("Exponentional" = "exp",
                                                                                                    "Gamma" = "gamma"), inline = TRUE),
                                                                                     # conditionalPanel(condition = "input.R_dist == 'exp'",
                                                                                     #   checkboxInput("R_diur", "Diurnal Pattern", FALSE)
                                                                                     #   ),
                                                                                     # conditionalPanel(condition = "input.R_diur && input.R_dist == 'exp'",
                                                                                     #   sliderInput("R_diur_now", "Time when the waiting period starts (in hours)", min = 0, max = 24, value = 3, step = 0.25),
                                                                                     #   sliderInput("R_diur_peak", "Peak activity level (in hours)", min = 0, max = 24, value = 2, step = 0.25)
                                                                                     #   ),
                                                                                     conditionalPanel(condition = "input.R_dist == 'gamma'",
                                                                                                      sliderInput("r_wt_gamma_shape", "Shape Parameter", min = 0, max = 20, value = 3, step = 1)
                                                                                     ),
                                                                                     hr(),
                                                                                     uiOutput('r_time_slider')
                                                                              ), 
                                                                              column(6,
                                                                                     plotOutput("R_wt_plot_option")
                                                                              )
                                                                     ),
                                                                     
                                                                     tabPanel("L",
                                                                              column(4,
                                                                                     h5("Mean Time Elapsed: "),
                                                                                     fluidRow(
                                                                                       column(4,selectInput("L_time_h", label = "Hours", choices = seq(0,24,1) , selected = 0, width = "100%")),
                                                                                       column(4,selectInput("L_time_m", label = "Minutes", choices = seq(0,55,5), selected = 30, width = "100%"))
                                                                                     ),
                                                                                     radioButtons("L_dist", "Distribution type:",
                                                                                                  c("Exponentional" = "exp",
                                                                                                    "Gamma" = "gamma"), inline = TRUE),
                                                                                     # conditionalPanel(condition = "input.L_dist == 'exp'",
                                                                                     #   checkboxInput("L_diur", "Diurnal Pattern", FALSE)
                                                                                     #   ),
                                                                                     # conditionalPanel(condition = "input.L_diur && input.L_dist == 'exp'",
                                                                                     #   sliderInput("L_diur_now", "Time when the waiting period starts (in hours)", min = 0, max = 24, value = 3, step = 0.25),
                                                                                     #   sliderInput("L_diur_peak", "Peak activity level (in hours)", min = 0, max = 24, value = 2, step = 0.25)
                                                                                     #   ),
                                                                                     conditionalPanel(condition = "input.L_dist == 'gamma'",
                                                                                                      sliderInput("l_wt_gamma_shape", "Shape Parameter", min = 0, max = 20, value = 3, step = 1)
                                                                                     ),
                                                                                     hr(),
                                                                                     uiOutput('l_time_slider')
                                                                              ), 
                                                                              column(6,
                                                                                     plotOutput("L_wt_plot_option")
                                                                              )
                                                                     ),
                                                                     
                                                                     tabPanel("O",
                                                                              column(4,
                                                                                     h5("Mean Time Elapsed: "),
                                                                                     fluidRow(
                                                                                       column(4,selectInput("O_time_h", label = "Hours", choices = seq(0,24,1) , selected = 0, width = "100%")),
                                                                                       column(4,selectInput("O_time_m", label = "Minutes", choices = seq(0,55,5), selected = 30, width = "100%"))
                                                                                     ),
                                                                                     radioButtons("O_dist", "Distribution type:",
                                                                                                  c("Exponentional" = "exp",
                                                                                                    "Gamma" = "gamma"), inline = TRUE),
                                                                                     # conditionalPanel(condition = "input.O_dist == 'exp'",
                                                                                     #   checkboxInput("O_diur", "Diurnal Pattern", FALSE)
                                                                                     #   ),
                                                                                     # conditionalPanel(condition = "input.O_diur && input.O_dist == 'exp'",
                                                                                     #   sliderInput("O_diur_now", "Time when the waiting period starts (in hours)", min = 0, max = 24, value = 3, step = 0.25),
                                                                                     #   sliderInput("O_diur_peak", "Peak activity level (in hours)", min = 0, max = 24, value = 2, step = 0.25)
                                                                                     #   ),
                                                                                     conditionalPanel(condition = "input.O_dist == 'gamma'",
                                                                                                      sliderInput("o_wt_gamma_shape", "Shape Parameter", min = 0, max = 20, value = 3, step = 1)
                                                                                     ),
                                                                                     hr(),
                                                                                     uiOutput('o_time_slider')
                                                                              ), 
                                                                              column(6,
                                                                                     plotOutput("O_wt_plot_option")
                                                                              )
                                                                     ),
                                                                     
                                                                     tabPanel("S",
                                                                              column(4,
                                                                                     h5("Mean Time Elapsed: "),
                                                                                     fluidRow(
                                                                                       column(4,selectInput("S_time_h", label = "Hours", choices = seq(0,24,1) , selected = 0, width = "100%")),
                                                                                       column(4,selectInput("S_time_m", label = "Minutes", choices = seq(0,55,5), selected = 30, width = "100%"))
                                                                                     ),
                                                                                     radioButtons("S_dist", "Distribution type:",
                                                                                                  c("Exponentional" = "exp",
                                                                                                    "Gamma" = "gamma"), inline = TRUE),
                                                                                     # conditionalPanel(condition = "input.S_dist == 'exp'",
                                                                                     #   checkboxInput("S_diur", "Diurnal Pattern", FALSE)
                                                                                     #   ),
                                                                                     # conditionalPanel(condition = "input.S_diur && input.S_dist == 'exp'",
                                                                                     #   sliderInput("S_diur_now", "Time when the waiting period starts (in hours)", min = 0, max = 24, value = 3, step = 0.25),
                                                                                     #   sliderInput("S_diur_peak", "Peak activity level (in hours)", min = 0, max = 24, value = 2, step = 0.25)
                                                                                     #   ),
                                                                                     conditionalPanel(condition = "input.S_dist == 'gamma'",
                                                                                                      sliderInput("s_wt_gamma_shape", "Shape Parameter", min = 0, max = 20, value = 3, step = 1)
                                                                                     ),
                                                                                     hr(),
                                                                                     uiOutput('s_time_slider')
                                                                              ), 
                                                                              column(6,
                                                                                     plotOutput("S_wt_plot_option")
                                                                              )
                                                                     )
                                                                     # ,
                                                                     
                                                                     # tabPanel("M",
                                                                     #   column(4,
                                                                     #   h5("Mean Time Elapsed: "),
                                                                     #   fluidRow(
                                                                     #   column(4,selectInput("M_time_h", label = "Hours", choices = seq(0,24,1) , selected = 0, width = "100%")),
                                                                     #   column(4,selectInput("M_time_m", label = "Minutes", choices = seq(0,55,5), selected = 30, width = "100%"))
                                                                     #   ),
                                                                     #   radioButtons("M_dist", "Distribution type:",
                                                                     #     c("Exponentional" = "exp",
                                                                     #       "Gamma" = "gamma"), inline = TRUE),
                                                                     #   # conditionalPanel(condition = "input.M_dist == 'exp'",
                                                                     #   #   checkboxInput("M_diur", "Diurnal Pattern", FALSE)
                                                                     #   #   ),
                                                                     #   # conditionalPanel(condition = "input.M_diur && input.M_dist == 'exp'",
                                                                     #   #   sliderInput("M_diur_now", "Time when the waiting period starts (in hours)", min = 0, max = 24, value = 3, step = 0.25),
                                                                     #   #   sliderInput("M_diur_peak", "Peak activity level (in hours)", min = 0, max = 24, value = 2, step = 0.25)
                                                                     #   #   ),
                                                                     #   conditionalPanel(condition = "input.M_dist == 'gamma'",
                                                                     #     sliderInput("m_wt_gamma_shape", "Shape Parameter", min = 0, max = 20, value = 3, step = 1)
                                                                     #     )
                                                                     #   ), 
                                                                     #   column(6,
                                                                     #     plotOutput("M_wt_plot_option")
                                                                     #     )
                                                                     #   ),
                                                                     
                                                                     # tabPanel("E",
                                                                     #   column(4,
                                                                     #   h5("Mean Time Elapsed: "),
                                                                     #   fluidRow(
                                                                     #   column(4,selectInput("E_time_h", label = "Hours", choices = seq(0,24,1) , selected = 0, width = "100%")),
                                                                     #   column(4,selectInput("E_time_m", label = "Minutes", choices = seq(0,55,5), selected = 30, width = "100%"))
                                                                     #   ),
                                                                     #   radioButtons("E_dist", "Distribution type:",
                                                                     #     c("Exponentional" = "exp",
                                                                     #       "Gamma" = "gamma"), inline = TRUE),
                                                                     #   # conditionalPanel(condition = "input.E_dist == 'exp'",
                                                                     #   #   checkboxInput("E_diur", "Diurnal Pattern", FALSE)
                                                                     #   #   ),
                                                                     #   # conditionalPanel(condition = "input.E_diur && input.E_dist == 'exp'",
                                                                     #   #   sliderInput("E_diur_now", "Time when the waiting period starts (in hours)", min = 0, max = 24, value = 3, step = 0.25),
                                                                     #   #   sliderInput("E_diur_peak", "Peak activity level (in hours)", min = 0, max = 24, value = 2, step = 0.25)
                                                                     #   #   ),
                                                                     #   conditionalPanel(condition = "input.E_dist == 'gamma'",
                                                                     #     sliderInput("e_wt_gamma_shape", "Shape Parameter", min = 0, max = 20, value = 3, step = 1)
                                                                     #     )
                                                                     #   ), 
                                                                     #   column(6,
                                                                     #     plotOutput("E_wt_plot_option")
                                                                     #     )
                                                                     #   )
                                                                   )
                                                          ),
                                                          #############################################################################
                                                          tabPanel("Survival",
                                                                   tabsetPanel(
                                                                     tabPanel("Flight Energetics",
                                                                              column(4,
                                                                                     # sliderInput(inputId = "S_u", label ="Per-bout Energy Expenditure",
                                                                                     #              value = 1/7, min = 0, max = 1, step = 0.01),
                                                                                     sliderInput(inputId = "S_u_inv", label ="Number of Bouts",
                                                                                                 value = 7, min = 0, max = 20, step = 1),
                                                                                     hr(),
                                                                                     tags$h5("Survival Probability Function of Energy Reserves:"),
                                                                                     sliderInput(inputId = "S_a", label ="Shape Param a of per-bout Probability of Survival",
                                                                                                 value = 20, min = 0, max = 100, step = 1),
                                                                                     sliderInput(inputId = "S_b", label ="Shape Param b of per-bout Probability of Survival",
                                                                                                 value = 10, min = 0, max = 100, step = 1)
                                                                              ),
                                                                              column(6,
                                                                                     plotOutput("flight_energetics_plot")
                                                                              )
                                                                     ),
                                                                     tabPanel("Senescence",
                                                                              column(4,
                                                                                     checkboxInput(inputId = "SENESCE", label = "Mortality during Generic Flight", value = FALSE),
                                                                                     conditionalPanel(condition = "!input.SENESCE",
                                                                                                      sliderInput(inputId = "sns_a", label ="Exp: a",
                                                                                                                  value = 0.085, min = 0, max = 0.1, step = 0.001),
                                                                                                      sliderInput(inputId = "sns_b", label ="Exp: b",
                                                                                                                  value = 100, min = 0, max = 1000, step = 1)
                                                                                     )),
                                                                              column(6,
                                                                                     plotOutput("senescence_plot")
                                                                              )
                                                                     ),
                                                                     tabPanel("Damage",
                                                                              column(4,
                                                                                     checkboxInput(inputId = "TATTER", label = "During Generic Flight", value = FALSE),
                                                                                     sliderInput(inputId = "ttsz_p", label ="Zero-inflation for Tattering Damage",
                                                                                                 value = 0.5, min = 0, max = 1, step = 0.1),
                                                                                     sliderInput(inputId = "ttsz_mean", label ="Mean of Tattering Damage",
                                                                                                 value = 0.4, min = 0, max = 1, step = 0.01),
                                                                                     sliderInput(inputId = "ttsz_v", label ="Dispersion of Tattering Damage (a + b) in Beta(a,b)",
                                                                                                 value = 5, min = 0, max = 20, step = 0.1),
                                                                                     hr(),
                                                                                     sliderInput(inputId = "ttr_a", label ="Exp: a for Tattering Survival",
                                                                                                 value = 15, min = 0, max = 100, step = 1),
                                                                                     sliderInput(inputId = "ttr_b", label ="Exp: b for Tattering Survival",
                                                                                                 value = 500, min = 0, max = 1000, step = 10)),
                                                                              column(6,
                                                                                     plotOutput("tattering_beta_plot"),
                                                                                     plotOutput("tattering_exp_plot")
                                                                              )
                                                                              
                                                                     )
                                                                   )),
                                                          
                                                          #########################################################################
                                                          tabPanel("Blood Meal",
                                                                   column(8,
                                                                          checkboxInput("showB_Option", "Setting Blood Meal Parameters", FALSE),
                                                                          conditionalPanel(condition = "input.showB_Option",
                                                                                           #helpText("The following parameters also can be set under 'Bouts' Panel"),
                                                                                           checkboxInput("showBloodMeal_Option", "Blood Meal Size", FALSE),
                                                                                           conditionalPanel(condition = "input.showBloodMeal_Option",
                                                                                                            fluidRow(
                                                                                                              column(6,
                                                                                                                     sliderInput(inputId = "bm_mean", label ="Average Bloodmeal Size",
                                                                                                                                 value = 0.5, min = 0, max = 1, step = 0.01),
                                                                                                                     sliderInput(inputId = "bm_v", label ="Parameter v for a Bloodmeal Size: (a + b) in Beta(a,b)",
                                                                                                                                 value = 15, min = 0, max = 40, step = 0.5)
                                                                                                              ),
                                                                                                              column(6,
                                                                                                                     plotOutput("bm_Option_plot")
                                                                                                              )
                                                                                                            )),
                                                                                           hr(),
                                                                                           checkboxInput("overfeed_Option", "Overfeed", FALSE),
                                                                                           conditionalPanel(condition = "input.overfeed_Option",
                                                                                                            fluidRow(
                                                                                                              column(6,
                                                                                                                     sliderInput(inputId = "of_a", "Exp Param a for overfeeding as function of bmSize",
                                                                                                                                 value = 8, min = 5, max = 10, step = 0.01),
                                                                                                                     sliderInput(inputId = "of_b", "Exp Param b for overfeeding as function of bmSize",
                                                                                                                                 value = 5000, min = 0, max = 10000, step = 100)),
                                                                                                              column(6,
                                                                                                                     plotOutput("overfeeding_Option_plot")
                                                                                                              ))
                                                                                                            # hr(),
                                                                                                            # sliderInput(inputId = "preGblood_Option", label ="Amount of Energy a Blood Meal Contributes to Pre-gonotrophic Energy Requirement (%)",
                                                                                                            #  value = 0, min = 0, max = 100, step = 1),
                                                                                                            # sliderInput(inputId = "Q_Option", label ="Human Blood Index",
                                                                                                            #  value = 0.9, min = 0, max = 1, step = 0.1)
                                                                                           )))),
                                                          
                                                          #########################################################################
                                                          # tabPanel("Energetics",
                                                          # 	column(6,
                                                          # 	  # sliderInput(inputId = "S_u", label ="Per-bout Energy Expenditure",
                                                          #    #              value = 1/7, min = 0, max = 1, step = 0.01),
                                                          #     sliderInput(inputId = "S_u_inv", label ="Numbers of Bouts",
                                                          #                 value = 7, min = 0, max = 20, step = 1),
                                                          #     hr(),
                                                          #     tags$h4("As Function of Energy Reserves:"),
                                                          #     sliderInput(inputId = "S_a", label ="Shape Param a of per-bout Probabilityof Survival",
                                                          #                 value = 20, min = 0, max = 100, step = 1),
                                                          #     sliderInput(inputId = "S_b", label ="Shape Param b of per-bout Probabilityof Survival",
                                                          #                 value = 10, min = 0, max = 100, step = 1)
                                                          #    )),
                                                          #########################################################################
                                                          
                                                          tabPanel("Sugar Feeding",
                                                                   column(6,
                                                                          sliderInput(inputId = "S_sa", label ="Shape Param a of Probability to queue Sugar bout",
                                                                                      value = 20, min = 0, max = 100, step = 1),
                                                                          sliderInput(inputId = "S_sb", label ="Shape Param b of Probability to queue Sugar bout",
                                                                                      value = 10, min = 0, max = 100, step = 1),
                                                                          sliderInput(inputId = "energyPreG", label ="Pre-gonotrophic Energy Requirement",
                                                                                      value = 0, min = 0, max = 100, step = 1),
                                                                          hr(),
                                                                          checkboxInput("showS_Option", "Sugar Feeding Parameters:", FALSE),
                                                                          conditionalPanel(condition = "input.showS_Option",
                                                                                           #sliderInput(inputId = "S_time", label ="Mean Time Elapsed (in days)",
                                                                                           #value = 0.02, min = 0, max = 2, step = 0.01),
                                                                                           # h5("Mean Time Elapsed: "),
                                                                                           #   fluidRow(
                                                                                           #   column(3,selectInput("S_time_h_Option", label = "hours", choices = seq(0,24,1) , selected = 0)),
                                                                                           #   column(3,selectInput("S_time_m_Option", label = "Minutes", choices = seq(0,55,5), selected = 30))
                                                                                           #   ),
                                                                                           #helpText("The following parameters also can be set under 'Bouts' Panel"),
                                                                                           sliderInput(inputId = "S_succeed_Option", label ="Probability of Success",
                                                                                                       value = 0.99, min = 0.9, max = 1, step = 0.01),
                                                                                           sliderInput(inputId = "S_surv_Option", label ="Baseline Probability of Survival",
                                                                                                       value = 0.99, min = 0.9, max = 1, step = 0.01),
                                                                                           #textInput("S_wts", "Landing Spot Weights: Enter a vector (comma delimited)", "1,1,1,1,1"),
                                                                                           sliderInput(inputId = "preGsugar_Option", label ="Amount of Energy a Sugar Meal Contributes to Pre-gonotrophic Energy Requirement (%)",
                                                                                                       value = 0, min = 0, max = 100, step = 1))
                                                                   )),
                                                          
                                                          #########################################################################
                                                          # tabPanel("Senescence",
                                                          #   column(6,
                                                          #   checkboxInput(inputId = "SENESCE", label = "Mortality during Generic Flight", value = FALSE),
                                                          #   conditionalPanel(condition = "!input.SENESCE",
                                                          #   sliderInput(inputId = "sns_a", label ="Exp: a",
                                                          #               value = 0.085, min = 0, max = 0.1, step = 0.001),
                                                          #   sliderInput(inputId = "sns_b", label ="Exp: b",
                                                          #               value = 100, min = 0, max = 1000, step = 1)
                                                          #   )),
                                                          #   column(6,
                                                          #     plotOutput("senescence_plot")
                                                          #     )),
                                                          ###########################################################################
                                                          # tabPanel("Damage",
                                                          #   checkboxInput(inputId = "TATTER", label = "During Generic Flight", value = FALSE),
                                                          #   sliderInput(inputId = "ttsz_p", label ="Zero-inflation for Tattering Damage",
                                                          #               value = 0.5, min = 0, max = 1, step = 0.1),
                                                          #   sliderInput(inputId = "ttsz_a", label ="Shape Param: a for Tattering Damage",
                                                          #               value = 5, min = 0, max = 100, step = 1),
                                                          #   sliderInput(inputId = "ttsz_b", label ="Shape Param: b for Tattering Damage",
                                                          #               value = 95, min = 0, max = 100, step = 1),
                                                          #   sliderInput(inputId = "ttr_a", label ="Exp: a for Tattering Survival",
                                                          #               value = 15, min = 0, max = 100, step = 1),
                                                          #   sliderInput(inputId = "ttr_b", label ="Exp: b for Tattering Survival",
                                                          #               value = 500, min = 0, max = 1000, step = 10)
                                                          #   ),
                                                          ################################################################################
                                                          
                                                          
                                                          tabPanel("Estivation"
                                                          ),
                                                          tabPanel("Maturation"
                                                          ),
                                                          tabPanel("Mating"
                                                          ),
                                                          tabPanel("Male Mosquitoes"
                                                          ),
                                                          tabPanel("Timing",
                                                                   sliderInput(inputId = "gammaShape", label ="Shape Param for Gamma Distributed Dwell Times:",
                                                                               value = 0.1, min = 1, max = 10, step = 0.1),
                                                                   sliderInput(inputId = "PfEIP", label ="Entomological Inoculation Period for Plasmodium falciparum During MosquitoFemale$probing()",
                                                                               value = 12, min = 0, max = 100, step = 1)
                                                          ),
                                                          tabPanel("Resting Spot",
                                                                   h4("Landing Spot Weights: Enter a vector (comma delimited) for each bout"),
                                                                   textInput("F_wts", "F: Blood Feeding Search", "1,1,1,1,1"),
                                                                   textInput("B_wts", "B: Blood Feeding Attempt", "1,1,1,1,1"),
                                                                   textInput("R_wts", "R: Post-prandial Resting", "1,1,1,1,1"),
                                                                   textInput("L_wts", "L: Egg Laying Search", "1,1,1,1,1"),
                                                                   textInput("O_wts", "O: Egg Laying Attempt", "1,1,1,1,1"),
                                                                   textInput("M_wts", "M: Mating", "1,1,1,1,1"),
                                                                   textInput("S_wts", "S: Sugar Feeding Attempt", "1,1,1,1,1")
                                                                   
                                                                   
                                                                   
                                                          ),
                                                          tabPanel("Egg",
                                                                   sliderInput(inputId = "bs_m", label ="Mean of Normally-distributed Egg Batch Size",
                                                                               value = 30, min = 0, max = 100, step = 1),
                                                                   sliderInput(inputId = "bs_v", label ="Standard Deviation of Normally-distributed Egg Batch Size",
                                                                               value = 30, min = 0, max = 100, step = 1),
                                                                   sliderInput(inputId = "maxBatch", label ="Maximum Egg Batch Size",
                                                                               value = 30, min = 0, max = 100, step = 1),
                                                                   sliderInput(inputId = "emt_m", label ="Mean of Normally-distributed Egg Batch Maturation Time",
                                                                               value = 3, min = 0, max = 10, step = 1),
                                                                   sliderInput(inputId = "emt_v", label ="Standard Deviation of Normally-distributed Egg Batch Maturation Time",
                                                                               value = 1, min = 0, max = 10, step = 1),
                                                                   sliderInput(inputId = "eggT", label ="Minimum Time to Egg Maturation",
                                                                               value = 0, min = 0, max = 10, step = 1),
                                                                   sliderInput(inputId = "eggP", label ="Minimum Provision to Produce Eggs",
                                                                               value = 0, min = 0, max = 10, step = 1)
                                                                   
                                                          )
                                                          
                                             )
                                             
                                             )),
                                  #################################################################################
                                  tabPanel(title = "Bouts", value = "bouts",
                                           useShinyjs(),
                                           fluidPage(
                                             sidebarLayout(position = "right",
                                                           sidebarPanel(
                                                             helpText("Please choose the bouts:"),
                                                             checkboxInput("showF", "F: Blood Feeding Search", FALSE),
                                                             checkboxInput("showB", "B: Blood Feeding Attempt", FALSE),
                                                             checkboxInput("showR", "R: Post-prandial Resting", FALSE),
                                                             checkboxInput("showL", "L: Egg Laying Search", FALSE),
                                                             checkboxInput("showO", "O: Egg Laying Attempt", FALSE),
                                                             checkboxInput("showM", "M: Mating", FALSE),
                                                             checkboxInput("showS", "S: Sugar Feeding Attempt", FALSE),
                                                             checkboxInput("showE", "E: Estivation", FALSE),
                                                             checkboxInput("showMale", "Male Mosquitoes", FALSE),
                                                             actionButton('save_inputs_bout', 'Save inputs',width = "100%"),
                                                             tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});')))
                                                           )
                                                           ,
                                                           mainPanel(
                                                             fluidRow(
                                                               column(7,helpText("Set parameters for selected bouts:"))
                                                             ),
                                                             # fluidRow(
                                                             # column(7,
                                                             tabsetPanel(
                                                               id = "boutbar",
                                                               tabPanel(
                                                                 title = "F",
                                                                 value = "bout_f",
                                                                 uiOutput('panel_f')
                                                               ),
                                                               tabPanel(
                                                                 title = "B",
                                                                 value = "bout_b",
                                                                 uiOutput('panel_b')
                                                               ),
                                                               tabPanel(
                                                                 title = "R",
                                                                 value = "bout_r",
                                                                 uiOutput('panel_r')
                                                               ),
                                                               tabPanel(
                                                                 title = "L",
                                                                 value = "bout_l",
                                                                 uiOutput('panel_l')
                                                               ),
                                                               tabPanel(
                                                                 title = "O",
                                                                 value = "bout_o",
                                                                 uiOutput('panel_o')
                                                               ),
                                                               tabPanel(
                                                                 title = "M",
                                                                 value = "bout_m",
                                                                 uiOutput('panel_m')
                                                               ),
                                                               tabPanel(
                                                                 title = "S",
                                                                 value = "bout_s",
                                                                 uiOutput('panel_s')
                                                               ),
                                                               tabPanel(
                                                                 title = "E",
                                                                 value = "bout_e",
                                                                 uiOutput('panel_e')
                                                               ),
                                                               tabPanel(
                                                                 title = "Male",
                                                                 value = "bout_male",
                                                                 uiOutput('panel_male')
                                                               )
                                                             ))
                                                           # ,
                                                           #   column(5,
                                                           #     plotOutput("bm_plot"),
                                                           #     plotOutput("overfeeding_plot")
                                                           #     )
                                                           #   )
                                                           # )
                                             ))
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