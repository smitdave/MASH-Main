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
list_of_packages <- c("shiny", "shinythemes", "shinyjs", "shinydashboard", "jsonlite", "miniUI")
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
			titlePanel(h1("MBITES: Behavioral Options")),
			navbarPage("Welcome ", id = "nav",
			##################### Overview ##############################################
            tabPanel("Get Started", value = 'start',
            	navlistPanel(widths = c(3,9),
            		tabPanel("Overview",
            			includeMarkdown("instructions.md"),
            			img(src='structure.png',align="center",width="100%", height= "150%")
            			),
		            ##################### load project #######################################
					tabPanel("Launch a project",
						h2("Welcome to MBITES!"),
						hr(),
						h4("To launch your project, please choose:"),
						radioButtons("project", "",
							c("First time user (Run our demo project)" = "demo",
								"Start a new project" = "new",
								"Work on an existing project" = "exist")),
						conditionalPanel(condition = "input.project == 'demo'",
							wellPanel(
								radioButtons("whichDemo", label = "Start with:",
									choices = c("Default Demo" = "default_demo","Customized Demo(.rds)" = "cust_demo")),
								conditionalPanel(condition = "input.whichDemo == 'cust_demo'",
									fileInput("load_demo", "Choose your .rds file", accept = ".rds")
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
						)))),

			###################### timing  ############################################
			tabPanel(title = "Initialization",value = "initial",
				uiOutput("panel_initial")
				),
			##################### About ##############################################
			tabPanel(title = "About",value = "about",
				uiOutput("panel_about")
				)


			)
))
