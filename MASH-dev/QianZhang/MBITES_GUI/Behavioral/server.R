###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     SERVER for MBITES GUI: Behavioral Options
#     MBITES Team 
#     JUN 2018
#
###############################################################################

server <- function(input, output, session){
  
  ############ Load Project ##################################################
	########### parameter default values ########################

	ParList <- reactive({
		if(input$project == 'demo'){
			if(input$whichDemo == 'cust_demo'){
				req(input$load_demo)
				demoFile <- input$load_demo
				return(readRDS(demoFile$datapath))
			}else{
				return(readRDS("demo/mbites_parameters_list.rds"))
			}}else if(input$project == 'exist'){
				if(input$exist_file == 'rds'){
					req(input$exist_file)
					rdsPar <- input$exist_rds
					return(readRDS(rdsPar$datapath))
				}else{
					req(input$file_project)
					csvPar <- input$file_project
					return(read.csv(csvPar$datapath, header = T, sep= input$sep_project))
				}
			}else{
				return(fromJSON("demo_json/demo.json", flatten=TRUE))
			}
		})

	############# new folder for new project ###########

	observeEvent(input$createNewFolder, {
		if(!file.exists(input$new_proj_name)){
			dir.create(input$new_proj_name)
			js_string_newproj <- 'alert("Created New Project Successfully!");'
			session$sendCustomMessage(type='jsCode', list(value = js_string_newproj))
			updateTabsetPanel(session, "nav", selected = "timing")
			toggle(selector = "#nav li a[data-value=start]")
		}else{
			js_string_folderexist <-'alert("Folder exists. Please rename your project and try it again!");'
			session$sendCustomMessage(type='jsCode', list(value = js_string_folderexist))
		}
	})
	
  ############ TabPanel Output ################################################
	############## Initial output ##############################
	output$panel_initial <- renderUI({
		fluidPage(
			helpText("Welcome to MBITES! Let's set up all parameters step by step."),
			navlistPanel(widths = c(2,10),

				######## Timing #########################
				tabPanel("Timing",
					helpText("test timing")
					),

				######## Blood Meal #####################
				tabPanel("Blood Meal",
					helpText("test Blood Meal")
					),

				######## Oogenesis ######################
				tabPanel("Oogenesis",
					helpText("test Oogenesis")
					),

				######## Energetics #####################
				tabPanel("Energetics",
					helpText("test Energetics")
					),

				#######  Oviposition ###################
				tabPanel("Oviposition",
					helpText("test Oviposition")
					),

				#######  Survival ######################
				tabPanel("Survival",
					helpText("test survival")
					),

				####### Pathogen #######################
				tabPanel("Pathogen",
					helpText("test pathgen")
					)




				)
			)
	}) 

	

	############## about output ############################
	output$panel_about <- renderUI({
		fluidPage(
			helpText("team information here")
			)
	})
}
