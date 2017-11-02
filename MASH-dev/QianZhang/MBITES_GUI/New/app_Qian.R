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
library(jsonlite)
if(system("whoami",intern=TRUE) == "chipdelmal"){
  DIR = "/Users/chipdelmal/Documents/Github/MASH-MAIN/MASH-dev/HectorSanchez/MBITES_GUI/New/"
  setwd(DIR)
}else if(system("whoami",intern=TRUE) == "qianzh"){
  DIR = "/Users/qianzh/project/MASH-Main/MASH-dev/QianZhang/MBITES_GUI/NEW"
  setwd(DIR)
}else if(system("whoami",intern=TRUE) == "smitdave"){
  DIR = "/Users/smitdave/github/MASH-Main/MASH-Main/MASH-dev/QianZhang/MBITES_GUI/NEW"
  setwd(DIR)
}


################################################################################################################
# CONSTANTS
################################################################################################################
INITIAL_GUI_WIDTH = 2000
INITIAL_GUI_HEIGHT = 1000
VAR_DESCR_COL_WIDTH = 3
VAR_SLIDE_COL_WIDTH = 4
THEME = "flatly"#themeSource="https://bootswatch.com/flatly/"
################################################################################################################
mbitesGadget = function(...){
  #########################################################################################
  # UI
  #########################################################################################
  ui = shinyUI(fluidPage(theme = shinytheme(THEME),
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
            fluidPage(
              helpText("Please choose parameters:"),
              navlistPanel(widths = c(2,10),
                #########################################################################
                tabPanel("Senescence",
                  checkboxInput(inputId = "SENEESCE", label = "Mortality during Generic Flight", value = FALSE),
                  sliderInput(inputId = "sns_a", label ="Exp: a",
                              value = 0.085, min = 0, max = 0.1, step = 0.001),
                  sliderInput(inputId = "sns_b", label ="Exp: b",
                              value = 100, min = 0, max = 1000, step = 1)
                  ),
                ###########################################################################
                tabPanel("Wing Tattering",
                  checkboxInput(inputId = "TATTER", label = "During Generic Flight", value = FALSE),
                  sliderInput(inputId = "ttsz_p", label ="Zero-inflation for Tattering Damage",
                              value = 0.5, min = 0, max = 1, step = 0.1),
                  sliderInput(inputId = "ttsz_a", label ="Shape Param: a for Tattering Damage",
                              value = 5, min = 0, max = 100, step = 1),
                  sliderInput(inputId = "ttsz_b", label ="Shape Param: b for Tattering Damage",
                              value = 95, min = 0, max = 100, step = 1),
                  sliderInput(inputId = "ttr_a", label ="Exp: a for Tattering Survival",
                              value = 15, min = 0, max = 100, step = 1),
                  sliderInput(inputId = "ttr_b", label ="Exp: b for Tattering Survival",
                              value = 500, min = 0, max = 1000, step = 10)
                  ),
                ################################################################################
                tabPanel("Blood Feeding"
                  ),
                tabPanel("Energetics",
                  sliderInput(inputId = "S_u", label ="Per-bout Energy Expenditure",
                              value = 1/7, min = 0, max = 1, step = 0.01),
                  hr(),
                  tags$h4("As Function of Energy Reserves:"),
                  sliderInput(inputId = "S_a", label ="Shape Param a of per-bout Probabilityof Survival",
                              value = 20, min = 0, max = 100, step = 1),
                  sliderInput(inputId = "S_b", label ="Shape Param b of per-bout Probabilityof Survival",
                              value = 10, min = 0, max = 100, step = 1),
                  sliderInput(inputId = "S_sa", label ="Shape Param a of Probability to queue Sugar bout",
                              value = 20, min = 0, max = 100, step = 1),
                  sliderInput(inputId = "S_sb", label ="Shape Param b of Probability to queue Sugar bout",
                              value = 10, min = 0, max = 100, step = 1),
                  sliderInput(inputId = "energyPreG", label ="Pre-gonotrophic Energy Requirement",
                              value = 0, min = 0, max = 100, step = 1)
                  ),
                tabPanel("Sugar Feeding"
                  ),
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
                tabPanel("Resting Spot"
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
          tabPanel(title = "Bouts",   value = "bouts",
            fluidPage(
              sidebarLayout(position = "right",
                sidebarPanel(style = "overflow-y:scroll; max-height: 600px",
                  helpText("Please Select the Bouts and Set Parameters:"),
                  checkboxInput("showF", "F: Blood Feeding Search", FALSE),
                  # conditionalPanel(condition = "input.showF",
                  #   wellPanel(sliderInput(inputId = "F_time", label ="Mean Time Elapsed (in days)",
                  #             value = 0.02, min = 0, max = 1, step = 0.01),
                  #             sliderInput(inputId = "F_succeed", label ="Probability of Success",
                  #             value = 0.98, min = 0.9, max = 1, step = 0.01),
                  #             sliderInput(inputId = "F_surv", label ="Baseline Probability of Survival",
                  #             value = 0.99, min = 0.9, max = 1, step = 0.01),
                  #             textInput("F_wts", "Landing Spot Weights: Enter a vector (comma delimited)", "1,1,1,1,1")
                  #             )
                  #   ),

                  checkboxInput("showB", "B: Blood Feeding Attempt", FALSE),
                  conditionalPanel(condition = "input.showB",
                    wellPanel(sliderInput(inputId = "B_time", label ="Mean Time Elapsed (in days)",
                              value = 0.04, min = 0, max = 2, step = 0.01),
                              sliderInput(inputId = "B_succeed", label ="Probability of Success",
                              value = 0.95, min = 0.8, max = 1, step = 0.01),
                              sliderInput(inputId = "B_surv", label ="Baseline Probability of Survival",
                              value = 0.99, min = 0.9, max = 1, step = 0.01),
                              textInput("B_wts", "Landing Spot Weights: Enter a vector (comma delimited)", "1,1,1,1,1"),
                              checkboxInput("showhuman", "Human Host Encounter", FALSE),
                              conditionalPanel(condition = "input.showhuman",
                                wellPanel(sliderInput(inputId = "surviveH", label ="Survival Probability of Initial Encounter (Proceed to Probe)",
                              value = 0.99, min = 0.9, max = 1, step = 0.01),
                                          sliderInput(inputId = "probeH", label ="Probability Undeterred and Begin Probing",
                              value = 0.99, min = 0.9, max = 1, step = 0.01),
                                          sliderInput(inputId = "surviveprobeH", label ="Survival Probability of Probing",
                              value = 0.99, min = 0.9, max = 1, step = 0.01),
                                          sliderInput(inputId = "feedH", label ="Probability to Successfully blood feed",
                              value = 0.99, min = 0.9, max = 1, step = 0.01)
                                )),
                              checkboxInput("shownonhuman", "Non-human Host Encounter", FALSE),
                              conditionalPanel(condition = "input.shownonhuman",
                                wellPanel(sliderInput(inputId = "surviveZ", label ="Survival Probability of Initial Encounter (Proceed to Feed)",
                              value = 0.99, min = 0.9, max = 1, step = 0.01),
                                          sliderInput(inputId = "feedZ", label ="Probability to Successfully Blood Feed",
                              value = 0.99, min = 0.9, max = 1, step = 0.01)
                                  )
                                ),
                              sliderInput(inputId = "bm_a", label ="Shape Param a for Bloodmeal Size",
                              value = 7.5, min = 0, max = 20, step = 0.5),
                              sliderInput(inputId = "bm_b", label ="Shape Param b for Bloodmeal Size",
                              value = 2.5, min = 0, max = 20, step = 0.5),
                              checkboxInput("overfeed", "Overfeed", FALSE),
                              conditionalPanel(condition = "input.overfeed",
                                sliderInput(inputId = "of_a", "Exp Param a for overfeeding as function of bmSize",
                                  value = 8, min = 5, max = 10, step = 0.01),
                                sliderInput(inputId = "of_b", "Exp Param b for overfeeding as function of bmSize",
                                  value = 5000, min = 0, max = 10000, step = 100)),
                              sliderInput(inputId = "preGblood", label ="Amount of Energy a Blood Meal Contributes to Pre-gonotrophic Energy Requirement",
                              value = 0, min = 0, max = 100, step = 1),
                              sliderInput(inputId = "Q", label ="Human Blood Index",
                              value = 0.9, min = 0, max = 1, step = 0.1)
                              )
                    ),
                  
                  checkboxInput("showR", "R: Post-prandial Resting", FALSE),
                  conditionalPanel(condition = "input.showR",
                    wellPanel(sliderInput(inputId = "R_time", label ="Mean Time Elapsed (in days)",
                              value = 1, min = 0, max = 3, step = 0.01),
                              sliderInput(inputId = "R_surv", label ="Baseline Probability of Survival",
                              value = 0.99, min = 0.9, max = 1, step = 0.01),
                              textInput("R_wts", "Landing Spot Weights: Enter a vector (comma delimited)", "1,1,1,1,1"),
                              checkboxInput("REFEED", "Refeed", FALSE),
                              conditionalPanel(condition = "input.refeed",
                                sliderInput(inputId = "rf_a", "Exp Param a for refeeding as function of bmSize",
                                  value = 60, min = 0, max = 100, step = 1),
                                sliderInput(inputId = "rf_b", "Exp Param b for refeeding as function of bmSize",
                                  value = 5000, min = 0, max = 10000, step = 100)))
                    ),
                  
                  checkboxInput("showL", "L: Egg Laying Search", FALSE),
                  conditionalPanel(condition = "input.showL",
                    wellPanel(sliderInput(inputId = "L_time", label ="Mean Time Elapsed (in days)",
                              value = 0.02, min = 0, max = 2, step = 0.01),
                              sliderInput(inputId = "L_succeed", label ="Probability of Success",
                              value = 0.98, min = 0.8, max = 1, step = 0.01),
                              sliderInput(inputId = "L_surv", label ="Baseline Probability of Survival",
                              value = 0.99, min = 0.9, max = 1, step = 0.01),
                              textInput("L_wts", "Landing Spot Weights: Enter a vector (comma delimited)", "1,1,1,1,1"))
                    ),
                  
                  checkboxInput("showO", "O: Egg Laying Attempt", FALSE),
                  conditionalPanel(condition = "input.showO",
                    wellPanel(sliderInput(inputId = "O_time", label ="Mean Time Elapsed (in days)",
                              value = 0.04, min = 0, max = 2, step = 0.01),
                              sliderInput(inputId = "O_succeed", label ="Probability of Success",
                              value = 0.99, min = 0.9, max = 1, step = 0.01),
                              sliderInput(inputId = "O_surv", label ="Baseline Probability of Survival",
                              value = 0.99, min = 0.9, max = 1, step = 0.01),
                              textInput("O_wts", "Landing Spot Weights: Enter a vector (comma delimited)", "1,1,1,1,1"))
                    ),

                  checkboxInput("showM", "M: Mating", FALSE),
                  conditionalPanel(condition = "input.showM",
                    wellPanel(sliderInput(inputId = "M_time", label ="Mean Time Elapsed (in days)",
                              value = 0.02, min = 0, max = 2, step = 0.01),
                              sliderInput(inputId = "M_succeed", label ="Probability of Success",
                              value = 0.95, min = 0.9, max = 1, step = 0.01),
                              sliderInput(inputId = "M_surv", label ="Baseline Probability of Survival",
                              value = 0.99, min = 0.9, max = 1, step = 0.01),
                              textInput("M_wts", "Landing Spot Weights: Enter a vector (comma delimited)", "1,1,1,1,1"))
                    ),

                  checkboxInput("showS", "S: Sugar Feeding Attempt", FALSE),
                  conditionalPanel(condition = "input.showS",
                    wellPanel(sliderInput(inputId = "S_time", label ="Mean Time Elapsed (in days)",
                              value = 0.02, min = 0, max = 2, step = 0.01),
                              sliderInput(inputId = "S_succeed", label ="Probability of Success",
                              value = 0.99, min = 0.9, max = 1, step = 0.01),
                              sliderInput(inputId = "S_surv", label ="Baseline Probability of Survival",
                              value = 0.99, min = 0.9, max = 1, step = 0.01),
                              textInput("S_wts", "Landing Spot Weights: Enter a vector (comma delimited)", "1,1,1,1,1"),
                              sliderInput(inputId = "preGsugar", label ="Amount of Energy a Sugar Meal Contributes to Pre-gonotrophic Energy Requirement",
                              value = 0, min = 0, max = 100, step = 1))
                    ),
                  
                  checkboxInput("showE", "E: Estivation", FALSE),
                  conditionalPanel(condition = "input.showE",
                    wellPanel(helpText("test"))
                  ),

                  checkboxInput("showMale", "Male Mosquitoes", FALSE),
                  conditionalPanel(condition = "input.showMale",
                    wellPanel(
                      checkboxInput("showMaleS", "Sugar Feeding", FALSE),
                      conditionalPanel(condition = "input.showMaleS",
                        wellPanel("test")),
                      checkboxInput("showMaleM", "Mating", FALSE),
                      conditionalPanel(condition = "input.showMaleM",
                        wellPanel("test"))
                        )
                    ),
                  actionButton('save_inputs_bout', 'Save inputs')

                  ),
                
                mainPanel(
                  helpText("Output Plots"),
                                    conditionalPanel(condition = "input.showF",
                    wellPanel(sliderInput(inputId = "F_time", label ="Mean Time Elapsed (in days)",
                              value = 0.02, min = 0, max = 1, step = 0.01),
                              sliderInput(inputId = "F_succeed", label ="Probability of Success",
                              value = 0.98, min = 0.9, max = 1, step = 0.01),
                              sliderInput(inputId = "F_surv", label ="Baseline Probability of Survival",
                              value = 0.99, min = 0.9, max = 1, step = 0.01),
                              textInput("F_wts", "Landing Spot Weights: Enter a vector (comma delimited)", "1,1,1,1,1")
                              )
                    )
                  )
                )
            )),
          #################################################################################
          tabPanel(title = "Bout-beta", value = "bout_beta",
            useShinyjs(),
            sidebarLayout(position = "right",
              sidebarPanel(
                helpText("Please choose the bouts:"),
                checkboxInput("selectF", "F: XXX ", TRUE),
                checkboxInput("selectB", "B: XXX", TRUE),
                checkboxInput("selectR", "R: XXXX", TRUE)
                ),
              mainPanel(
                tabsetPanel(
                  id = "boutbar",
                  tabPanel(
                    title = "F",
                    value = "beta_f",
                    h1("test F")
                    ),
                  tabPanel(
                    title = "B",
                    value = "beta_b",
                    h1("test B")
                    ),
                  tabPanel(
                    title = "R",
                    value = "beta_r",
                    h1("test R")
                    )
                  )
                )
              )
            ),

          #################################################################################
          tabPanel(title = "Landscape", value = 'landscape',                 
            sidebarLayout(position = "right",
              sidebarPanel(style = "overflow-y:scroll; max-height: 600px",
                helpText("Please set the parameters"),
                checkboxInput("showPoints", "Points", FALSE),
                conditionalPanel(condition = "input.showPoints",
                  wellPanel(
                    helpText("f"),
                    helpText("m"),
                    helpText("s")
                    )
                  ),
                checkboxInput("showKernels", "Kernels(Female)", FALSE),
                conditionalPanel(condition = "input.showKernels",
                  wellPanel(
                    helpText("f"),
                    helpText("l"),
                    helpText("m"),
                    helpText("s")
                    )
                  ),
                checkboxInput("show_land_male", "Males", FALSE),
                conditionalPanel(condition = "input.show_land_male",
                  wellPanel(
                    helpText("M"),
                    helpText("s")
                    )
                  )
                ),
              mainPanel(
                helpText("test output")
                )
          )),
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
    observe({
        if (input$launchgo > 0) {
            session$sendCustomMessage('activeNavs', 'Ecology')
        }
    })
    output$prepath.box <- renderUI({
      if(input$project == "exist"){
        textInput("prepath", "Please provide the previous working directory (the folder contains .json file)", "")
      }
    })

    observe({
      toggle(condition = input$selectF, selector = "#boutbar li a[data-value=beta_f]")
    })
    observe({
      toggle(condition = input$selectR, selector = "#boutbar li a[data-value=beta_r]")
    })
    observe({
      toggle(condition = input$selectB, selector = "#boutbar li a[data-value=beta_b]")
    })



    observeEvent(input$done, {
      stopApp(brushedPoints(data, input$brush))
    })
    
    observeEvent(input$save_inputs_bout, {
      # Define inputs to save
      inputs_to_save_bout <- c('F_time', 'F_wts', 'F_time')
      # Declare inputs
      inputs_bout <- NULL
      # Append all inputs before saving to folder
      for(input.i in inputs_to_save_bout){
        inputs_bout <- append(inputs_bout, input[[input.i]])
      }
      # Inputs data.frame
      inputs_data_frame <- data.frame(inputId = inputs_to_save_bout, value = inputs_bout)
      # Save Inputs
      jsonOut=prettify(toJSON(inputs_data_frame))
      write(jsonOut,paste0(DIR,"/try.json"))
    }) 
    
  }
  #########################################################################################
  # RUN
  #########################################################################################
  runGadget(ui,server,viewer=dialogViewer("ggbrush",width=INITIAL_GUI_WIDTH,height=INITIAL_GUI_HEIGHT))
}

mbitesGadget()