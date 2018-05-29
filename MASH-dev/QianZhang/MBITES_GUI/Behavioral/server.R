###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     SERVER for MBITES GUI: Landscape
#     MBITES Team
#     JUN 2018
#
###############################################################################

server <- function(input, output, seession){
	
  ############ TabPanel Output ################################################
	############## timing output ##############################################
	output$panel_timing <- renderUI({
		fluidPage(
			helpText("setup parameters for timing")
			)
	}) 

	############## bloodmeal output ############################################
	output$panel_bloodmeal <- renderUI({
		fluidPage(
			helpText("setup parameters for bloodmeal")
			)
	}) 

	############## oogenesis output ############################################
	output$panel_oogenesis <- renderUI({
		fluidPage(
			helpText("setup parameters for oogenesis")
			)
	})

	############## energetics output ############################################
	output$panel_energetics <- renderUI({
		fluidPage(
			helpText("setup parameters for energetics")
			)
	})

	############## oviposition output ############################################
	output$panel_oviposition <- renderUI({
		fluidPage(
			helpText("setup parameters for oviposition")
			)
	})

	############## survival output ############################################
	output$panel_survival <- renderUI({
		fluidPage(
			helpText("setup parameters for survival")
			)
	})

	############## pathogen output ############################################
	output$panel_pathogen <- renderUI({
		fluidPage(
			helpText("setup parameters for pathogen")
			)
	})

	############## about output ############################################
	output$panel_survival <- renderUI({
		fluidPage(
			helpText("team information here")
			)
	})
}
