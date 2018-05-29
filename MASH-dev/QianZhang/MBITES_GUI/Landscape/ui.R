###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     UI for MBITES GUI: Landscape
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
			titlePanel(h1("MBITES: Landscape")),
			navbarPage("Welcome ", id = "nav",
			##################### Overview ##############################################
            tabPanel("Get Started", value = 'start',
            	navlistPanel(widths = c(3,9),
            		tabPanel("Overview"),
			tabPanel("Launch a project",
				h2("Welcome to MBITES!"),
				hr(),
				h4("To launch your project, please choose:"),
				radioButtons("project", "",
					c("First time user (Run our demo project)" = "demo",
						"Start a new project" = "new",
						"Work on an existing project" = "exist"))
				)

			))))
)