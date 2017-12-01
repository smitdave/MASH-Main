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
library(shinyFiles)
if(system("whoami",intern=TRUE) == "chipdelmal"){
  DIR = "/Users/chipdelmal/Documents/Github/MASH-MAIN/MASH-dev/HectorSanchez/MBITES_GUI/New/"
  setwd(DIR)
}else if(system("whoami",intern=TRUE) == "qianzh"){
  DIR = "/Users/qianzh/project/MASH-Main/MASH-dev/QianZhang/MBITES_GUI/NEW"
  setwd(DIR)
}else if(system("whoami",intern=TRUE) == "smitdave"){
  DIR = "/Users/smitdave/github/MASH-Main/MASH-Main/MASH-dev/QianZhang/MBITES_GUI/NEW"
  setwd(DIR)
}else if(system("whoami",intern=TRUE) == "dave"){
  DIR = "/Users/dave/GitHub/MASH-Main/MASH-dev/QianZhang/MBITES_GUI/NEW"
  setwd(DIR)
}else if(system("whoami",intern=TRUE) == "sanchez.hmsc"){
  DIR = "/Users/sanchez.hmsc/Documents/github/MASH-Main/MASH-dev/QianZhang/MBITES_GUI/NEW"
  setwd(DIR)
}else if(system("whoami",intern=TRUE) == "slwu89"){
  DIR = "/Users/slwu89/Desktop/git/MASH-Main-slwu89/MASH-dev/QianZhang/MBITES_GUI/NEW"
  setwd(DIR)
}else{
  setwd("your directory")
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
                tabPanel("Waiting Time",
                  column(6,
                    tabsetPanel(
                      tabPanel("F",
                        radioButtons("F_dist", "Distribution type:",
                          c("Exponentional" = "exp",
                            "Gamma" = "gamma"), inline = TRUE),
                        conditionalPanel(condition = "input.F_dist == 'exp'",
                          checkboxInput("F_diur", "Diurnal Pattern", FALSE)
                          )
                        ),
                      tabPanel("B",
                        radioButtons("B_dist", "Distribution type:",
                          c("Exponentional" = "exp",
                            "Gamma" = "gamma"), inline = TRUE),
                        conditionalPanel(condition = "input.B_dist == 'exp'",
                          checkboxInput("B_diur", "Diurnal Pattern", FALSE)
                          )),
                      tabPanel("R",
                        radioButtons("R_dist", "Distribution type:",
                          c("Exponentional" = "exp",
                            "Gamma" = "gamma"), inline = TRUE),
                        conditionalPanel(condition = "input.R_dist == 'exp'",
                          checkboxInput("R_diur", "Diurnal Pattern", FALSE)
                          )),
                      tabPanel("L",
                        radioButtons("L_dist", "Distribution type:",
                          c("Exponentional" = "exp",
                            "Gamma" = "gamma"), inline = TRUE),
                        conditionalPanel(condition = "input.L_dist == 'exp'",
                          checkboxInput("L_diur", "Diurnal Pattern", FALSE)
                          )),
                      tabPanel("O",
                        radioButtons("O_dist", "Distribution type:",
                          c("Exponentional" = "exp",
                            "Gamma" = "gamma"), inline = TRUE),
                        conditionalPanel(condition = "input.O_dist == 'exp'",
                          checkboxInput("O_diur", "Diurnal Pattern", FALSE)
                          )),
                      tabPanel("S",
                        radioButtons("S_dist", "Distribution type:",
                          c("Exponentional" = "exp",
                            "Gamma" = "gamma"), inline = TRUE),
                        conditionalPanel(condition = "input.S_dist == 'exp'",
                          checkboxInput("S_diur", "Diurnal Pattern", FALSE)
                          )),
                      tabPanel("M",
                        radioButtons("M_dist", "Distribution type:",
                          c("Exponentional" = "exp",
                            "Gamma" = "gamma"), inline = TRUE),
                        conditionalPanel(condition = "input.M_dist == 'exp'",
                          checkboxInput("M_diur", "Diurnal Pattern", FALSE)
                          )),
                      tabPanel("E",
                        radioButtons("E_dist", "Distribution type:",
                          c("Exponentional" = "exp",
                            "Gamma" = "gamma"), inline = TRUE),
                        conditionalPanel(condition = "input.E_dist == 'exp'",
                          checkboxInput("E_diur", "Diurnal Pattern", FALSE)
                          ))
                      )
                    )
                  ),
                #############################################################################
                tabPanel("Survival",
                tabsetPanel(
                    tabPanel("Flight Energetics",
                      column(6,
                    # sliderInput(inputId = "S_u", label ="Per-bout Energy Expenditure",
                   #              value = 1/7, min = 0, max = 1, step = 0.01),
                      sliderInput(inputId = "S_u_inv", label ="Number of Bouts",
                                  value = 7, min = 0, max = 20, step = 1),
                      hr(),
                      tags$h4("As Function of Energy Reserves:"),
                      sliderInput(inputId = "S_a", label ="Shape Param a of per-bout Probabilityof Survival",
                                  value = 20, min = 0, max = 100, step = 1),
                      sliderInput(inputId = "S_b", label ="Shape Param b of per-bout Probabilityof Survival",
                                  value = 10, min = 0, max = 100, step = 1)
                    ),
                      column(6,
                        plotOutput("flight_energetics_plot")
                        )
                      ),
                    tabPanel("Senescence",
                      column(6,
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
                      column(6,
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
              				checkboxInput("showBloodMeal_Option", "Blood Meal Size", FALSE),
                      		conditionalPanel(condition = "input.showBloodMeal_Option",
                          	# sliderInput(inputId = "bm_a_Option", label ="Shape Param a for Bloodmeal Size",
                           #        value = 7.5, min = 0, max = 20, step = 0.5),
                          	# sliderInput(inputId = "bm_b_Option", label ="Shape Param b for Bloodmeal Size",
                           #        value = 2.5, min = 0, max = 20, step = 0.5),
                            fluidRow(
                            column(6,
                              sliderInput(inputId = "bm_mean_Option", label ="Average Bloodmeal Size",
                                    value = 0.5, min = 0, max = 1, step = 0.01),
                              sliderInput(inputId = "bm_v_Option", label ="Parameter v for a Bloodmeal Size: (a + b) in Beta(a,b)",
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
    	                        sliderInput(inputId = "of_a_Option", "Exp Param a for overfeeding as function of bmSize",
    	                          value = 8, min = 5, max = 10, step = 0.01),
    	                        sliderInput(inputId = "of_b_Option", "Exp Param b for overfeeding as function of bmSize",
    	                          value = 5000, min = 0, max = 10000, step = 100)),
                              column(6,
                                plotOutput("overfeeding_Option_plot")
                                )),
	                      	hr(),
	                      	sliderInput(inputId = "preGblood_Option", label ="Amount of Energy a Blood Meal Contributes to Pre-gonotrophic Energy Requirement (%)",
	                        value = 0, min = 0, max = 100, step = 1),
	                      	sliderInput(inputId = "Q_Option", label ="Human Blood Index",
	                        value = 0.9, min = 0, max = 1, step = 0.1)
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
		              h5("Mean Time Elapsed: "),
		                fluidRow(
		                column(3,selectInput("S_time_h_Option", label = "hours", choices = seq(0,24,1) , selected = 0)),
		                column(3,selectInput("S_time_m_Option", label = "Minutes", choices = seq(0,55,5), selected = 30))
		                ),
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
                  checkboxInput("showMale", "Male Mosquitoes", FALSE)
                  ),
                mainPanel(
                  fluidRow(
                    column(7,helpText("Set parameters for selected bouts:")),
                    column(5,actionButton('save_inputs_bout', 'Save inputs',width = "50%"))),
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
          tabPanel(title = "Landscape", value = 'landscape',
          	useShinyjs(),
            sidebarLayout(position = "right",
              sidebarPanel(style = "overflow-y:scroll; max-height: 600px",
                helpText("Please set the parameters"),
                checkboxInput("showPoints", "Points", FALSE),
                conditionalPanel(condition = "input.showPoints",
                  wellPanel(
                    checkboxInput("landscape_point_f", "f", FALSE),
                    conditionalPanel(condition = "input.landscape_point_f",
                    	wellPanel(
                    	radioButtons(inputId = "landscape_f_input", "Provide the locations and weights:",
                    		choices = c("Clusters" = "cluster",
                    					"Import x, y, w" = "imp_xyw",
                    					"Import x, y" = "imp_xy"
                    					),
                    		selected = "cluster"),
                    	conditionalPanel(condition = "input.landscape_f_input != 'cluster'",
                    		fileInput('filef', 'Choose CSV File',
                       			accept=c('text/csv',
                                'text/comma-separated-values,text/plain',
                                '.csv')),
                    		wellPanel(checkboxInput('headerf', 'Header', TRUE),
                 			radioButtons('sepf', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','))
                    		),
                    	uiOutput("landscape_f_file")
                    	)),
                    checkboxInput("landscape_point_m", "m", FALSE),
                    conditionalPanel(condition = "input.landscape_point_m",
                    	wellPanel(
                    	radioButtons(inputId = "landscape_m_input", "Provide the locations and weights:",
                    		choices = c("Clusters" = "cluster",
                    					"Import x, y, w" = "imp_xyw",
                    					"Import x, y" = "imp_xy"
                    					),
                    		selected = "cluster"),
                    	conditionalPanel(condition = "input.landscape_m_input != 'cluster'",
                    		fileInput('filem', 'Choose CSV File',
                       			accept=c('text/csv',
                                'text/comma-separated-values,text/plain',
                                '.csv')),
                    		wellPanel(checkboxInput('headerm', 'Header', TRUE),
                 			radioButtons('sepm', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','))
                    		),
                    	uiOutput("landscape_m_file")
                    	)),
                    checkboxInput("landscape_point_s", "s", FALSE),
                    conditionalPanel(condition = "input.landscape_point_s",
                    	wellPanel(
                    	radioButtons(inputId = "landscape_s_input", "Provide the locations and weights:",
                    		choices = c("Clusters" = "cluster",
                    					"Import x, y, w" = "imp_xyw",
                    					"Import x, y" = "imp_xy"
                    					),
                    		selected = "cluster"),
                    	conditionalPanel(condition = "input.landscape_s_input != 'cluster'",
                    		fileInput('files', 'Choose CSV File',
                       			accept=c('text/csv',
                                'text/comma-separated-values,text/plain',
                                '.csv')),
                    		wellPanel(checkboxInput('headers', 'Header', TRUE),
                 			radioButtons('seps', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','))
                    		),
                    	uiOutput("landscape_s_file")
                    	))
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
                		),
                	tabPanel(
                		title = "Point: m",
                		value = "landscape_out_m",
                		plotOutput("panel_landscape_out_m")
                		),
                	tabPanel(
                		title = "Point: s",
                		value = "landscape_out_s",
                		plotOutput("panel_landscape_out_s")
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
      if(!input$SENESCE){
      ggplot(data.frame(age), aes(x = age)) + stat_function(fun= senescence_surv) + ylim(0,1) +
        xlab("Chronological Age (days)") + ylab("Probability of Survival, per bout")
    	}else{
    	ggplot(data.frame(age), aes(age)) + geom_hline(aes(yintercept = 1)) + ylim(0,1) +
    	scale_x_discrete()+
        xlab("Chronological Age (days)") + ylab("Probability of Survival, per bout")
    	}
    })

    output$bm_Option_plot <- renderPlot({
      if(input$showBloodMeal_Option){
        bm_a <- input$bm_mean_Option * input$bm_v_Option
        bm_b <- (1 - input$bm_mean_Option) * input$bm_v_Option
        curve(pbeta(x, bm_a, bm_b),ylab = "Probability", xlab = "Blood Meal Size")}
    })

    output$bm_plot <- renderPlot({
      if(input$showBloodMeal){
        bm_a <- input$bm_mean * input$bm_v
        bm_b <- (1 - input$bm_mean) * input$bm_v
        curve(pbeta(x, bm_a, bm_b),ylab = "Probability", xlab = "Blood Meal Size")}
    })

    output$overfeeding_plot <- renderPlot({
      if(input$overfeed){
        # bm_a <- input$bm_mean * input$bm_v
        # bm_b <- (1 - input$bm_mean) * input$bm_v
        a <- input$of_a
        b <- input$of_b
        curve(exp(a * x)/(exp(a * x) + b),
          ylab = "Mortality", xlab = "Blood Meal Size", ylim = c(0,1), xlim = c(0,1))}
    })

    output$overfeeding_Option_plot <- renderPlot({
      if(input$overfeed_Option){
        # bm_a_Option <- input$bm_mean_Option * input$bm_v_Option
        # bm_b_Option <- (1 - input$bm_mean_Option) * input$bm_v_Option
        a <- input$of_a_Option
        b <- input$of_b_Option
        curve(exp(a * x)/(exp(a * x) + b),
          ylab = "Mortality", xlab = "Blood Meal Size", ylim = c(0,1), xlim = c(0,1))}
    })

    output$flight_energetics_plot <- renderPlot({
        curve(exp(input$S_a * x)/(exp(input$S_a * x) + input$S_b), ylab = "Mortality", xlab = "Energy Reserves")
    })
    output$tattering_exp_plot <- renderPlot({
        curve(exp(input$ttr_a * x)/(exp(input$ttr_a * x) + input$ttr_b), ylab = "Mortality", xlab = "Wing Tattering",
          main = "Exponentional Distribution")
    })

    output$tattering_beta_plot <- renderPlot({
        #bm_a <- input$bm_mean * input$bm_v
        #bm_b <- (1 - input$bm_mean) * input$bm_v
        curve(pbeta(x, input$ttsz_a, input$ttsz_b),ylab = "Tattering Damage", xlab = "Wing Tattering", main = "Beta Distribution")
    })




######################################Landscape Output###########################################################
    dataF <- reactive({
	    req(input$filef)
	    inFileF <- input$filef
	    dfF <- read.csv(inFileF$datapath, header = input$headerf, sep = input$sepf)
	    return(dfF)
	  })
	output$contentsF <- renderTable({
	      dataF()
	  })
	dataM <- reactive({
	    req(input$filem)
	    inFileM <- input$filem
	    dfM <- read.csv(inFileM$datapath, header = input$headerm, sep = input$sepm)
	    return(dfM)
	  })
	output$contentsM <- renderTable({
	      dataM()
	  })
	dataS <- reactive({
	    req(input$files)
	    inFileS <- input$files
	    dfS <- read.csv(inFileS$datapath, header = input$headers, sep = input$seps)
	    return(dfS)
	  })
	output$contentsS <- renderTable({
	      dataS()
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

    	 }else{
    	 	xF <- dataF()[, 1:2]
	    	plot(xF, col="red")
    	 }

    })


    output$panel_landscape_out_m <- renderPlot({
    	 if(input$landscape_point_m & input$landscape_m_input == "cluster"){
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



    	 }else{
    	 	xM <- dataM()[, 1:2]
	    	plot(xM, col="blue")
    	 }

    })

    output$panel_landscape_out_s <- renderPlot({
    	 if(input$landscape_point_s & input$landscape_s_input == "cluster"){
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



    	 }else{
    	 	xS <- dataS()[, 1:2]
	    	plot(xS, col="green")
    	 }

    })

    observe({
      toggle(condition = input$landscape_point_f, selector = "#landscape_output li a[data-value=landscape_out_f]")
    })
    observe({
      toggle(condition = input$landscape_point_m, selector = "#landscape_output li a[data-value=landscape_out_m]")
    })
    observe({
      toggle(condition = input$landscape_point_s, selector = "#landscape_output li a[data-value=landscape_out_s]")
    })


######################################################################################################################
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






    #######################################################################
    output$panel_f <- renderUI({
        if (input$showF)
            column(6,
              #sliderInput(inputId = "F_time", label ="Mean Time Elapsed (in days)",
                              #value = 0.02, min = 0, max = 1, step = 0.01),
              wellPanel(
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
              ))

    })



    output$panel_b <- renderUI({
        if (input$showB)
          fluidRow(
          column(6,
              #sliderInput(inputId = "B_time", label ="Mean Time Elapsed (in days)",
                          #value = 0.04, min = 0, max = 2, step = 0.01),
            wellPanel(
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
                  # sliderInput(inputId = "bm_a", label ="Shape Param a for Bloodmeal Size",
                  #         value = 7.5, min = 0, max = 20, step = 0.5),
                  # sliderInput(inputId = "bm_b", label ="Shape Param b for Bloodmeal Size",
                  #         value = 2.5, min = 0, max = 20, step = 0.5)
                  sliderInput(inputId = "bm_mean", label ="Average Bloodmeal Size",
                          value = 0.5, min = 0, max = 1, step = 0.01),
                  sliderInput(inputId = "bm_v", label ="Sample size for a Bloodmeal Size: (a + b) in Beta(a,b)",
                          value = 15, min = 0, max = 40, step = 0.5)
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
              )),
          column(6,
            plotOutput(""),
            plotOutput("bm_plot"),
            plotOutput("overfeeding_plot")))
        })
    output$panel_r <- renderUI({
        if (input$showR)
          column(6,
            conditionalPanel(condition = "input.showR",
            #sliderInput(inputId = "R_time", label ="Mean Time Elapsed (in days)",
                #value = 1, min = 0, max = 3, step = 0.01),
              wellPanel(
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
                  value = 5000, min = 0, max = 10000, step = 100))))
              )
        })
    output$panel_l <- renderUI({
        if (input$showL)
          column(6,
              #sliderInput(inputId = "L_time", label ="Mean Time Elapsed (in days)",
                              #value = 0.02, min = 0, max = 2, step = 0.01),
            wellPanel(
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
              #sliderInput(inputId = "O_time", label ="Mean Time Elapsed (in days)",
                              #value = 0.04, min = 0, max = 2, step = 0.01),
            wellPanel(
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
              #sliderInput(inputId = "M_time", label ="Mean Time Elapsed (in days)",
                              #value = 0.02, min = 0, max = 2, step = 0.01),
            wellPanel(
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
              #sliderInput(inputId = "S_time", label ="Mean Time Elapsed (in days)",
                              #value = 0.02, min = 0, max = 2, step = 0.01),
            wellPanel(
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
              checkboxInput("showMaleS", "Sugar Feeding", FALSE),
              conditionalPanel(condition = "input.showMaleS",
                wellPanel("test")),
              checkboxInput("showMaleM", "Mating", FALSE),
              conditionalPanel(condition = "input.showMaleM",
                wellPanel("test")
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


    ##################Sync inputs for multiple pages##########################################
    ##########################Blood Meal######################################
    observe({
      	updateCheckboxInput(session, "showB", NULL, value = input$showB_Option)
  	})
  	observe({
      	updateCheckboxInput(session, "showB_Option", NULL, value = input$showB)
  	})

  	observe({
      	updateCheckboxInput(session, "showBloodMeal", NULL, value = input$showBloodMeal_Option)
  	})
  	observe({
      	updateCheckboxInput(session, "showBloodMeal_Option", NULL, value = input$showBloodMeal)
  	})

  	observe({
      	updateSliderInput(session, "bm_a", NULL, value = input$bm_a_Option)
  	})
  	observe({
      	updateSliderInput(session, "bm_a_Option", NULL, value = input$bm_a)
  	})

  	observe({
      	updateSliderInput(session, "bm_b", NULL, value = input$bm_b_Option)
  	})
  	observe({
      	updateSliderInput(session, "bm_b_Option", NULL, value = input$bm_b)
  	})

  	observe({
      	updateCheckboxInput(session, "overfeed", NULL, value = input$overfeed_Option)
  	})
  	observe({
      	updateCheckboxInput(session, "overfeed_Option", NULL, value = input$overfeed)
  	})

  	observe({
      	updateSliderInput(session, "of_a", NULL, value = input$of_a_Option)
  	})
  	observe({
      	updateSliderInput(session, "of_a_Option", NULL, value = input$of_a)
  	})

  	observe({
      	updateSliderInput(session, "of_b", NULL, value = input$of_b_Option)
  	})
  	observe({
      	updateSliderInput(session, "of_b_Option", NULL, value = input$of_b)
  	})

  	observe({
      	updateSliderInput(session, "preGblood", NULL, value = input$preGblood_Option)
  	})
  	observe({
      	updateSliderInput(session, "preGblood_Option", NULL, value = input$preGblood)
  	})

  	observe({
      	updateSliderInput(session, "Q", NULL, value = input$Q_Option)
  	})
  	observe({
      	updateSliderInput(session, "Q_Option", NULL, value = input$Q)
  	})

  	##########################Sugar Feeding######################################
    observe({
      	updateCheckboxInput(session, "showS", NULL, value = input$showS_Option)
  	})
  	observe({
      	updateCheckboxInput(session, "showS_Option", NULL, value = input$showS)
  	})

  	observe({
      	updateSelectInput(session, "S_time_h", NULL, selected = input$S_time_h_Option)
  	})
  	observe({
      	updateSelectInput(session, "S_time_h_Option", NULL, selected  = input$S_time_h)
  	})
  	observe({
      	updateSelectInput(session, "S_time_m", NULL, selected = input$S_time_m_Option)
  	})
  	observe({
      	updateSelectInput(session, "S_time_m_Option", NULL, selected = input$S_time_m)
  	})

  	observe({
      	updateSliderInput(session, "S_succeed", NULL, value = input$S_succeed_Option)
  	})
  	observe({
      	updateSliderInput(session, "S_succeed_Option", NULL, value = input$S_succeed)
  	})

  	observe({
      	updateSliderInput(session, "S_surv", NULL, value = input$S_surv_Option)
  	})
  	observe({
      	updateSliderInput(session, "S_surv_Option", NULL, value = input$S_surv)
  	})

  	observe({
      	updateSliderInput(session, "preGsugar", NULL, value = input$preGsugar_Option)
  	})
  	observe({
      	updateSliderInput(session, "preGsugar_Option", NULL, value = input$preGsugar)
  	})

    ##########################################################################################



    observeEvent(input$done, {
      stopApp(brushedPoints(data, input$brush))
    })

    observeEvent(input$save_inputs_bout, {
      # Define inputs to save
      f_param_name <- c('F_time_m', 'F_time_h', 'F_succeed', 'F_surv')
      b_param_name <- c('B_time_m', 'B_time_h','B_succeed', 'B_surv',
        'surviveH', 'probeH', 'surviveprobeH', 'feedH',
        'surviveZ', 'feedZ', 'bm_a', 'bm_b', 'overfeed', 'of_a', 'of_b', 'preGblood',
        'Q')
      r_param_name <- c('R_time_m', 'R_time_h','R_surv', 'REFEED', 'rf_a', 'rf_b')
      l_param_name <- c('L_time_m', 'L_time_h','L_succeed','L_surv')
      o_param_name <- c('O_time_m', 'O_time_h','O_succeed', 'O_surv')
      m_param_name <- c('M_time_m', 'M_time_h','M_succeed', 'M_surv')
      s_param_name <- c('S_time_m', 'S_time_h','S_succeed', 'S_surv', 'preGsugar')
      # Declare inputs
      inputs_bout <- NULL
      inputs_name <- NULL
      # Append all inputs before saving to folder
      if(input$showF){

        for(input.i in f_param_name){
          if(length(input[[input.i]]) != 0){
          inputs_name <- append(inputs_name,input.i)
          inputs_bout <- append(inputs_bout, input[[input.i]])
      }}}

      if(input$showB){
        for(input.i in b_param_name){
          if(length(input[[input.i]]) != 0){
          inputs_name <- append(inputs_name,input.i)
          inputs_bout <- append(inputs_bout, input[[input.i]])
      }}}

      if(input$showR){
        for(input.i in r_param_name){
          if(length(input[[input.i]]) != 0){
          inputs_name <- append(inputs_name,input.i)
          inputs_bout <- append(inputs_bout, input[[input.i]])
      }}}

      if(input$showL){
        for(input.i in l_param_name){
          if(length(input[[input.i]]) != 0){
          inputs_name <- append(inputs_name,input.i)
          inputs_bout <- append(inputs_bout, input[[input.i]])
      }}}

      if(input$showO){
        for(input.i in o_param_name){
          if(length(input[[input.i]]) != 0){
          inputs_name <- append(inputs_name,input.i)
          inputs_bout <- append(inputs_bout, input[[input.i]])
      }}}

      if(input$showM){
        for(input.i in m_param_name){
          if(length(input[[input.i]]) != 0){
          inputs_name <- append(inputs_name,input.i)
          inputs_bout <- append(inputs_bout, input[[input.i]])
      }}}

      if(input$showS){
        for(input.i in m_param_name){
          if(length(input[[input.i]]) != 0){
          inputs_name <- append(inputs_name,input.i)
          inputs_bout <- append(inputs_bout, input[[input.i]])
      }}}

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
