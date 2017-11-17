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
                  column(6,
                  checkboxInput(inputId = "SENEESCE", label = "Mortality during Generic Flight", value = FALSE),
                  sliderInput(inputId = "sns_a", label ="Exp: a",
                              value = 0.085, min = 0, max = 0.1, step = 0.001),
                  sliderInput(inputId = "sns_b", label ="Exp: b",
                              value = 100, min = 0, max = 1000, step = 1)
                  ),
                  column(6,
                    plotOutput("senescence_plot")
                    )),
                ###########################################################################
                tabPanel("Damage",
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
                  checkboxInput("showMale", "Male Mosquitoes", FALSE)
                  ),
                mainPanel(
                  fluidRow(
                    column(7,helpText("Set parameters for selected bouts:")),
                    column(5,actionButton('save_inputs_bout', 'Save inputs',width = "50%"))),
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
                    checkboxInput("landscape_point_f", "f", FALSE),
                    conditionalPanel(condition = "input.landscape_point_f",
                    	radioButtons(inputId = "landscape_f_input", "Provide the locations and weights:",
                    		choices = c("Clusters" = "cluster",
                    					"Import x, y, w" = "imp_xyw",
                    					"Import x, y" = "imp_xy"
                    					),
                    		selected = "cluster"),
                    	uiOutput("landscape_f_file")
                    	),
                    checkboxInput("landscape_point_m", "m", FALSE),
                    checkboxInput("landscape_point_s", "s", FALSE)
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
                tabsetPanel(
                	id = "landscape_output",
                	tabPanel(
                		title = "Point: f",
                		value = "landscape_out_f",
                		plotOutput("panel_landscape_out_f")
                		)
                	)
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

    output$senescence_plot <- renderPlot({
      age <- seq(0, 50, 0.001)
      senescence_surv <- function(x, ...){
        (2 + input$sns_b)/(1 + input$sns_b) - exp(x * input$sns_a)/(input$sns_b + x * input$sns_a)
      }
      ggplot(data.frame(age), aes(age)) + stat_function(fun= senescence_surv) + ylim(0,1) +
        xlab("Chronological Age (days)") + ylab("Probability of Survival, per bout")
    })

    output$panel_landscape_out_f <- renderPlot({
    	 if(input$landscape_point_f & input$landscape_f_input == "cluster"){
    		getPoints = function(seed, nCenters,  rng, nPaC, nPaCvr, spr, centers=NULL){
			  set.seed(seed)
			  xCenters = runif(nCenters, -rng, rng)
			  yCenters = runif(nCenters, -rng, rng)

			  x = 0
			  y=0

			  n = pmax(5, rnbinom(nCenters,mu=nPaC,size=nPaCvr))
			  spread = rgamma(nCenters,1,1)*spr

			  for(i in 1:nCenters){
			    x = c(x,xCenters[i]+rnorm(n[i],0,spread[i]))
			    y = c(y,yCenters[i]+rnorm(n[i],0,spread[i]))
			  }
			  x = x[-1]
			  y = y[-1]

			  plot(x,y, pch = 15, col = "red") 
			  cbind(x,y) #return(list(xy=cbind(x,y), centers = cbind(xCenters, yCenters)))  
			}

			xy_f = getPoints(21,nCenters=5,rng=10,nPaC=12,nPaCvr=2,spr=1)
			xy_l = getPoints(21,nCenters=25,rng=10,nPaC=8,nPaCvr=2,spr=.4)
			N_l = length(xy_l[,1])
			w_l = rgamma(length(xy_f[,1]), 1,1)

			xy_f1 = getPoints(23,nCenters=25,rng=10,nPaC=10,nPaCvr=2,spr=.6)

			xy_f = rbind(xy_f, xy_f1)
			N_f = length(xy_f[,1])
			w_f = rgamma(length(xy_f[,1]), 1,1)

			plot(xy_f, pch = 15, col = "red", xlim = range(xy_f, xy_l), ylim = range(xy_f, xy_l))

			points(xy_l, pch =15, col = "blue")



    	 }
      
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
    output$landscape_f_file <- renderUI({
      if(input$landscape_f_input %in% c("imp_xy", "imp_xyw")){
        textInput("landscape_f_filepath", "Please provide the file path:)", "")
      }
    })

    #######################################################################
    output$panel_f <- renderUI({
        if (input$showF)
            column(6,
              wellPanel(
              #sliderInput(inputId = "F_time", label ="Mean Time Elapsed (in days)",
                              #value = 0.02, min = 0, max = 1, step = 0.01),
              
                h5("Mean Time Elapsed: "),
                fluidRow(
                column(6,selectInput("F_time_h", label = "hours", choices = seq(0,24,1) , selected = 0)),
                column(6,selectInput("F_time_m", label = "Minutes", choices = seq(0,55,5), selected = 30))
                ),
                sliderInput(inputId = "F_succeed", label ="Probability of Success",
                              value = 0.98, min = 0.9, max = 1, step = 0.01),
                sliderInput(inputId = "F_surv", label ="Baseline Probability of Survival",
                              value = 0.99, min = 0.9, max = 1, step = 0.01)#,
              #textInput("F_wts", "Landing Spot Weights: Enter a vector (comma delimited)", "1,1,1,1,1")
                    )
              )

    })



    output$panel_b <- renderUI({
        if (input$showB)
          column(6,
            wellPanel(
                      #sliderInput(inputId = "B_time", label ="Mean Time Elapsed (in days)",
                                  #value = 0.04, min = 0, max = 2, step = 0.01),
                      h5("Mean Time Elapsed: "),
                      fluidRow(
                        column(6,selectInput("B_time_h", label = "hours", choices = seq(0,24,1) , selected = 0)),
                        column(6,selectInput("B_time_m", label = "Minutes", choices = seq(0,55,5), selected = 30))
                              ),
                      sliderInput(inputId = "B_succeed", label ="Probability of Success",
                                  value = 0.95, min = 0.8, max = 1, step = 0.01),                #                     
                      sliderInput(inputId = "B_surv", label ="Baseline Probability of Survival",
                                  value = 0.99, min = 0.9, max = 1, step = 0.01),
                      #textInput("B_wts", "Landing Spot Weights: Enter a vector (comma delimited)", "1,1,1,1,1"),
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
                                  )),
                      checkboxInput("showBloodMeal", "Blood Meal Size", FALSE),
                      conditionalPanel(condition = "input.showBloodMeal",
                        wellPanel(
                          sliderInput(inputId = "bm_a", label ="Shape Param a for Bloodmeal Size",
                                  value = 7.5, min = 0, max = 20, step = 0.5),
                          sliderInput(inputId = "bm_b", label ="Shape Param b for Bloodmeal Size",
                                  value = 2.5, min = 0, max = 20, step = 0.5)
                          )
                        ),
                      checkboxInput("overfeed", "Overfeed", FALSE),
                      conditionalPanel(condition = "input.overfeed",
                        sliderInput(inputId = "of_a", "Exp Param a for overfeeding as function of bmSize",
                          value = 8, min = 5, max = 10, step = 0.01),
                        sliderInput(inputId = "of_b", "Exp Param b for overfeeding as function of bmSize",
                          value = 5000, min = 0, max = 10000, step = 100)),
                      sliderInput(inputId = "preGblood", label ="Amount of Energy a Blood Meal Contributes to Pre-gonotrophic Energy Requirement (%)",
                        value = 0, min = 0, max = 100, step = 1),
                      sliderInput(inputId = "Q", label ="Human Blood Index",
                        value = 0.9, min = 0, max = 1, step = 0.1)
                      ))
        })
    output$panel_r <- renderUI({
        if (input$showR)
          column(6,
          wellPanel(
            conditionalPanel(condition = "input.showR",
              wellPanel(#sliderInput(inputId = "R_time", label ="Mean Time Elapsed (in days)",
                #value = 1, min = 0, max = 3, step = 0.01),
                h5("Mean Time Elapsed: "),
                fluidRow(
                column(6,selectInput("R_time_h", label = "hours", choices = seq(0,24,1) , selected = 0)),
                column(6,selectInput("R_time_m", label = "Minutes", choices = seq(0,55,5), selected = 30))
                ),
                sliderInput(inputId = "R_surv", label ="Baseline Probability of Survival",
                value = 0.99, min = 0.9, max = 1, step = 0.01),
              #textInput("R_wts", "Landing Spot Weights: Enter a vector (comma delimited)", "1,1,1,1,1"),
              checkboxInput("REFEED", "Refeed", FALSE),
              conditionalPanel(condition = "input.refeed",
                sliderInput(inputId = "rf_a", "Exp Param a for refeeding as function of bmSize",
                  value = 60, min = 0, max = 100, step = 1),
                sliderInput(inputId = "rf_b", "Exp Param b for refeeding as function of bmSize",
                  value = 5000, min = 0, max = 10000, step = 100)))
              )))
        })    
    output$panel_l <- renderUI({
        if (input$showL)
          column(6,
            wellPanel(
              #sliderInput(inputId = "L_time", label ="Mean Time Elapsed (in days)",
                              #value = 0.02, min = 0, max = 2, step = 0.01),
              h5("Mean Time Elapsed: "),
                fluidRow(
                column(6,selectInput("L_time_h", label = "hours", choices = seq(0,24,1) , selected = 0)),
                column(6,selectInput("L_time_m", label = "Minutes", choices = seq(0,55,5), selected = 30))
                ),
              sliderInput(inputId = "L_succeed", label ="Probability of Success",
                              value = 0.98, min = 0.8, max = 1, step = 0.01),
              sliderInput(inputId = "L_surv", label ="Baseline Probability of Survival",
                              value = 0.99, min = 0.9, max = 1, step = 0.01)#,
              #textInput("L_wts", "Landing Spot Weights: Enter a vector (comma delimited)", "1,1,1,1,1")
                    ))
    })
    output$panel_o <- renderUI({
        if (input$showO)
          column(6,
            wellPanel(
              #sliderInput(inputId = "O_time", label ="Mean Time Elapsed (in days)",
                              #value = 0.04, min = 0, max = 2, step = 0.01),
              h5("Mean Time Elapsed: "),
                fluidRow(
                column(6,selectInput("O_time_h", label = "hours", choices = seq(0,24,1) , selected = 0)),
                column(6,selectInput("O_time_m", label = "Minutes", choices = seq(0,55,5), selected = 30))
                ),
              sliderInput(inputId = "O_succeed", label ="Probability of Success",
                              value = 0.99, min = 0.9, max = 1, step = 0.01),
              sliderInput(inputId = "O_surv", label ="Baseline Probability of Survival",
                              value = 0.99, min = 0.9, max = 1, step = 0.01)#,
              #textInput("O_wts", "Landing Spot Weights: Enter a vector (comma delimited)", "1,1,1,1,1")
              ))
    })
    output$panel_m <- renderUI({
        if (input$showM)
          column(6,
            wellPanel(
              #sliderInput(inputId = "M_time", label ="Mean Time Elapsed (in days)",
                              #value = 0.02, min = 0, max = 2, step = 0.01),
              h5("Mean Time Elapsed: "),
                fluidRow(
                column(6,selectInput("M_time_h", label = "hours", choices = seq(0,24,1) , selected = 0)),
                column(6,selectInput("M_time_m", label = "Minutes", choices = seq(0,55,5), selected = 30))
                ),
              sliderInput(inputId = "M_succeed", label ="Probability of Success",
                              value = 0.95, min = 0.9, max = 1, step = 0.01),
              sliderInput(inputId = "M_surv", label ="Baseline Probability of Survival",
                              value = 0.99, min = 0.9, max = 1, step = 0.01)#,
              #textInput("M_wts", "Landing Spot Weights: Enter a vector (comma delimited)", "1,1,1,1,1")
              ))
    })
    output$panel_s <- renderUI({
        if (input$showS)
          column(6,
            wellPanel(
              #sliderInput(inputId = "S_time", label ="Mean Time Elapsed (in days)",
                              #value = 0.02, min = 0, max = 2, step = 0.01),
              h5("Mean Time Elapsed: "),
                fluidRow(
                column(6,selectInput("S_time_h", label = "hours", choices = seq(0,24,1) , selected = 0)),
                column(6,selectInput("S_time_m", label = "Minutes", choices = seq(0,55,5), selected = 30))
                ),
              sliderInput(inputId = "S_succeed", label ="Probability of Success",
                              value = 0.99, min = 0.9, max = 1, step = 0.01),
              sliderInput(inputId = "S_surv", label ="Baseline Probability of Survival",
                              value = 0.99, min = 0.9, max = 1, step = 0.01),
              #textInput("S_wts", "Landing Spot Weights: Enter a vector (comma delimited)", "1,1,1,1,1"),
              sliderInput(inputId = "preGsugar", label ="Amount of Energy a Sugar Meal Contributes to Pre-gonotrophic Energy Requirement (%)",
                              value = 0, min = 0, max = 100, step = 1)))
    })    
    output$panel_e <- renderUI({
        if (input$showE)
          column(6,
            wellPanel(helpText("test")))
    })
    output$panel_male <- renderUI({
        if (input$showMale)
          column(6,
            wellPanel(
              checkboxInput("showMaleS", "Sugar Feeding", FALSE),
              conditionalPanel(condition = "input.showMaleS",
                wellPanel("test")),
              checkboxInput("showMaleM", "Mating", FALSE),
              conditionalPanel(condition = "input.showMaleM",
                wellPanel("test"))
              ))
    }) 
    observe({
      toggle(condition = input$showF, selector = "#boutbar li a[data-value=bout_f]")
    })
    observe({
      toggle(condition = input$showB, selector = "#boutbar li a[data-value=bout_b]")
    })
    observe({
      toggle(condition = input$showR, selector = "#boutbar li a[data-value=bout_r]")
    })
    observe({
      toggle(condition = input$showL, selector = "#boutbar li a[data-value=bout_l]")
    })
    observe({
      toggle(condition = input$showO, selector = "#boutbar li a[data-value=bout_o]")
    })
    observe({
      toggle(condition = input$showM, selector = "#boutbar li a[data-value=bout_m]")
    })
    observe({
      toggle(condition = input$showS, selector = "#boutbar li a[data-value=bout_s]")
    })
    observe({
      toggle(condition = input$showE, selector = "#boutbar li a[data-value=bout_e]")
    })
    observe({
      toggle(condition = input$showMale, selector = "#boutbar li a[data-value=bout_male]")
    })
    #########################################################################################



    observeEvent(input$done, {
      stopApp(brushedPoints(data, input$brush))
    })
    
    observeEvent(input$save_inputs_bout, {
      # Define inputs to save
      f_param_name <- c('F_time_m', 'F_time_h', 'F_succeed', 'F_surv',
        'F_wts')
      b_param_name <- c('B_time', 'B_succeed', 'B_surv', 'B_wts',
        'surviveH', 'probeH', 'surviveprobeH', 'feedH',
        'surviveZ', 'feedZ', 'bm_a', 'bm_b', 'overfeed', 'of_a', 'of_b', 'preGblood',
        'Q')
      r_param_name <- c('R_time', 'R_surv', 'R_wts', 'REFEED', 'rf_a', 'rf_b')
      l_param_name <- c('L_time', 'L_succeed','L_surv', 'L_wts')
      o_param_name <- c('O_time', 'O_succeed', 'O_surv','O_wts')
      m_param_name <- c('M_time', 'M_succeed', 'M_surv', 'M_wts')
      s_param_name <- c('S_time', 'S_succeed', 'S_surv', 'S_wts', 'preGsugar')
      # Declare inputs
      inputs_bout <- NULL
      inputs_name <- NULL
      # Append all inputs before saving to folder
      if(input$showF){
        inputs_name <- append(inputs_name,f_param_name)
        for(input.i in f_param_name){
          inputs_bout <- append(inputs_bout, input[[input.i]])
      }}
      if(input$showB){
        inputs_name <- append(inputs_name,b_param_name)
        for(input.i in b_param_name){
          inputs_bout <- append(inputs_bout, input[[input.i]])
      }}
      if(input$showR){
        inputs_name <- append(inputs_name,r_param_name)
        for(input.i in r_param_name){
          inputs_bout <- append(inputs_bout, input[[input.i]])
      }}
      if(input$showL){
        inputs_name <- append(inputs_name,l_param_name)
        for(input.i in l_param_name){
          inputs_bout <- append(inputs_bout, input[[input.i]])
      }}
      if(input$showO){
        inputs_name <- append(inputs_name,o_param_name)
        for(input.i in o_param_name){
          inputs_bout <- append(inputs_bout, input[[input.i]])
      }}
      if(input$showM){
        inputs_name <- append(inputs_name,m_param_name)
        for(input.i in m_param_name){
          inputs_bout <- append(inputs_bout, input[[input.i]])
      }}
      if(input$showS){
        inputs_name <- append(inputs_name,s_param_name)
        for(input.i in s_param_name){
          inputs_bout <- append(inputs_bout, input[[input.i]])
      }}

      # Inputs data.frame
      inputs_data_frame <- data.frame(inputId = inputs_name, value = inputs_bout)
      # Save Inputs
      jsonOut=prettify(toJSON(inputs_data_frame))
      write(jsonOut,paste0(DIR,"/bouts.json"))
    }) 
  }
  #########################################################################################
  # RUN
  #########################################################################################
  runGadget(ui,server,viewer=dialogViewer("ggbrush",width=INITIAL_GUI_WIDTH,height=INITIAL_GUI_HEIGHT))
}

mbitesGadget()