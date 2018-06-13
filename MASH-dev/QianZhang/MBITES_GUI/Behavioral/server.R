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

server <- function(input, output, session) {
  cat(getwd())
  output$plot <- renderPlot({
    ggplot(data, aes_string(xvar, yvar)) + geom_point()
  })

  #################### Loading parameter rds/csv file #####################################

ParList <- reactive({
  if(input$project == 'demo'){
    if(input$whichDemo == 'cust_demo'){
      req(input$load_demo)
      demoFile <- input$load_demo
      return(readRDS(demoFile$datapath))
    }else{
      return(readRDS("default_demo/mbites_parameters_list.rds"))
    }}else if(input$project == 'exist'){
      if(input$exist_file == '.rds'){
        req(input$exist_file)
        rdsPar <- input$exist_rds
        return(readRDS(rdsPar$datapath))
      }else{
        req(input$file_project)
        csvPar <- input$file_project
        return(read.csv(csvPar$datapath, header = T, sep= ","))
        }
    }else{
      return(readRDS("default_demo/mbites_parameters_list.rds"))
    }
})


  #################### Parameters Output #####################################################
  output$panel_initial <- renderUI({
                            fluidPage(
                             helpText("Welcome to MBITES! Please set parameters here:"),
                             navlistPanel(widths = c(2,10),
                                          ######### Timing #######################
                                          tabPanel("Bouts",
                                            column(6,
                                              checkboxGroupButtons(inputId = "Search_bout", label = "Search:", choices = c("B", "O", "M", "S"), status = "success", 
                                                selected = c("B", "O"), checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")))),
                                            column(6,
                                              checkboxGroupButtons(inputId = "Attempt_bout", label = "Attempt:", choices = c("B", "O", "M", "S"), status = "success",
                                                selected = c("B", "O"), checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")))),
                                            tabsetPanel(id = "boutbar",
                                              tabPanel("B",
                                                helpText("test B"),
                                                column(6,
                                                conditionalPanel(condition = "input.Search_bout.includes('B')", 
                                                  wellPanel(
                                                    h4("test BS"))),
                                                conditionalPanel(condition = "input.Attempt_bout.includes('B')", 
                                                  wellPanel(
                                                    h4("test B"))))
                                                ),
                                              tabPanel("O",
                                                helpText("test O"),
                                                column(6,
                                                conditionalPanel(condition = "input.Search_bout.includes('O')", 
                                                  wellPanel(
                                                    h4("test OS"))),
                                                conditionalPanel(condition = "input.Attempt_bout.includes('O')", 
                                                  wellPanel(
                                                    h4("test O"))))
                                                ),
                                              tabPanel("M",
                                                helpText("test M"),
                                                column(6,
                                                conditionalPanel(condition = "input.Search_bout.includes('M')", 
                                                  wellPanel(
                                                    h4("test MS"))),
                                                conditionalPanel(condition = "input.Attempt_bout.includes('M')", 
                                                  wellPanel(
                                                    h4("test M"))))
                                                ),
                                              tabPanel("S",
                                                helpText("test S"),
                                                column(6,
                                                conditionalPanel(condition = "input.Search_bout.includes('S')", 
                                                  wellPanel(
                                                    h4("test SS"))),
                                                conditionalPanel(condition = "input.Attempt_bout.includes('S')", 
                                                  wellPanel(
                                                    h4("test S"))))
                                                )
                                                )
                                            ),
                                          ######### Timing #######################
                                          tabPanel("Timing",
                                            tabsetPanel(
                                              tabPanel("Timing model",
                                                helpText("Timing model for for attempt & search bout inter-launch time-to-event sampling distribution."),
                                                column(4,
                                                       selectInput("timing_model", label = "Timing Model", choice = list('Deterministic' = 1, "Exponential" = 2, "Gamma" = 3),selected = 'Deterministic'),
                                                       conditionalPanel(condition = "input.timing_model == 1",
                                                        tabsetPanel(id = "timing_model_1",
                                                          tabPanel("B",
                                                            conditionalPanel(condition = "input.Search_bout.includes('B')",
                                                              sliderInput(inputId = "wait_bs", label = "Blood feeding search:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              ),
                                                            conditionalPanel(condition = "!input.Search_bout.includes('B')",
                                                              helpText("Blood Feeding Search Bout is turned off")
                                                              ),
                                                            hr(),
                                                            conditionalPanel(condition = "input.Attempt_bout.includes('B')",
                                                              sliderInput(inputId = "wait_b", label = "Blood feeding attempt:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              ),
                                                            conditionalPanel(condition = "!input.Attempt_bout.includes('B')",
                                                              helpText("Blood Feeding Attempt Bout is turned off")
                                                              )
                                                            ),
                                                          tabPanel("O",
                                                            conditionalPanel(condition = "input.Search_bout.includes('O')",
                                                              sliderInput(inputId = "wait_os", label = "Oviposition search:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              ),
                                                            conditionalPanel(condition = "!input.Search_bout.includes('O')",
                                                              helpText("Oviposition Search Bout is turned off")
                                                              ),
                                                            hr(),
                                                            conditionalPanel(condition = "input.Attempt_bout.includes('O')",
                                                              sliderInput(inputId = "wait_o", label = "Oviposition attempt:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              ),
                                                            conditionalPanel(condition = "!input.Attempt_bout.includes('O')",
                                                              helpText("Oviposition Attempt Bout is turned off")
                                                              )
                                                            ),
                                                          tabPanel("M",
                                                            conditionalPanel(condition = "input.Search_bout.includes('M')",
                                                              sliderInput(inputId = "wait_ms", label = "Mating search:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              ),
                                                            conditionalPanel(condition = "!input.Search_bout.includes('M')",
                                                              helpText("Mating Search Bout is turned off")
                                                              ),
                                                            hr(),
                                                            conditionalPanel(condition = "input.Attempt_bout.includes('M')",
                                                              sliderInput(inputId = "wait_m", label = "Mating attempt:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              ),
                                                            conditionalPanel(condition = "!input.Attempt_bout.includes('M')",
                                                              helpText("Mating Attempt Bout is turned off")
                                                              )
                                                            ),
                                                          tabPanel("S",
                                                            conditionalPanel(condition = "input.Search_bout.includes('S')",
                                                              sliderInput(inputId = "wait_ss", label = "Sugar feeding search:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              ),
                                                            conditionalPanel(condition = "!input.Search_bout.includes('S')",
                                                              helpText("Sugar feeding Search Bout is turned off")
                                                              ),
                                                            hr(),
                                                            conditionalPanel(condition = "input.Attempt_bout.includes('S')",
                                                              sliderInput(inputId = "wait_s", label = "Sugar feeding attempt:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              ),
                                                            conditionalPanel(condition = "!input.Attempt_bout.includes('S')",
                                                              helpText("Sugar feeding Attempt Bout is turned off")
                                                              )
                                                            )
                                                        )),
                                                       conditionalPanel(condition = "input.timing_model == 2",
                                                        wellPanel(
                                                          h4("For attempt bouts:"),
                                                          sliderInput(inputId = "rate_b", label = "Inverse of average waiting time to next launch for blood feeding:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "tmin_b", label = "Minimum waiting time prior to next launch for blood feeding:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "rate_o", label = "Inverse of average waiting time to next launch for oviposition:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "tmin_o", label = "Minimum waiting time prior to next launch for oviposition:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "rate_m", label = "Inverse of average waiting time to next launch for mating:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "tmin_m", label = "Minimum waiting time prior to next launch for mating:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "rate_s", label = "Inverse of average waiting time to next launch for sugar feeding:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "tmin_s", label = "Minimum waiting time prior to next launch for sugar feeding:", value = 0.5 , min = 0, max = 1, step = 0.05)
                                                          ), 
                                                        wellPanel(
                                                          h4("For search bouts:"),
                                                          sliderInput(inputId = "rate_bs", label = "Inverse of average waiting time to next launch for blood feeding:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "tmin_bs", label = "Minimum waiting time prior to next launch for blood feeding:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "rate_os", label = "Inverse of average waiting time to next launch for oviposition:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "tmin_os", label = "Minimum waiting time prior to next launch for oviposition:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "rate_ms", label = "Inverse of average waiting time to next launch for mating:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "tmin_ms", label = "Minimum waiting time prior to next launch for mating:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "rate_ss", label = "Inverse of average waiting time to next launch for sugar feeding:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "tmin_ss", label = "Minimum waiting time prior to next launch for sugar feeding:", value = 0.5 , min = 0, max = 1, step = 0.05)
                                                          )
                                                        ),
                                                       conditionalPanel(condition = "input.timing_model == 3",
                                                        wellPanel(
                                                          h4("For attempt bouts:"),
                                                          sliderInput(inputId = "mean_b", label = "Inverse of average waiting time to next launch for blood feeding:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "cv_b", label = "Coefficient of variation between mean and variance of waiting time:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "mean_o", label = "Inverse of average waiting time to next launch for oviposition:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "cv_o", label = "Coefficient of variation between mean and variance of waiting time", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "mean_m", label = "Inverse of average waiting time to next launch for mating:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "cv_m", label = "Coefficient of variation between mean and variance of waiting time:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "mean_s", label = "Inverse of average waiting time to next launch for sugar feeding:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "cv_s", label = "Coefficient of variation between mean and variance of waiting time:", value = 0.5 , min = 0, max = 1, step = 0.05)
                                                          ), 
                                                        wellPanel(
                                                          h4("For search bouts:"),
                                                          sliderInput(inputId = "mean_bs", label = "Inverse of average waiting time to next launch for blood feeding:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "cv_bs", label = "Coefficient of variation between mean and variance of waiting time:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "mean_os", label = "Inverse of average waiting time to next launch for oviposition:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "cv_os", label = "Coefficient of variation between mean and variance of waiting time", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "mean_ms", label = "Inverse of average waiting time to next launch for mating:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "cv_ms", label = "Coefficient of variation between mean and variance of waiting time:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "mean_ss", label = "Inverse of average waiting time to next launch for sugar feeding:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                          sliderInput(inputId = "cv_ss", label = "Coefficient of variation between mean and variance of waiting time:", value = 0.5 , min = 0, max = 1, step = 0.05)
                                                          )
                                                        )
                                          )),

                                          tabPanel("PPR model",
                                            helpText("Post-prandial resting length sampling distribution:"),
                                            column(4,
                                                selectInput("ppr_model", label = "PPR Model", choice = list('Deterministic' = 1, "Exponential" = 2, "Gamma" = 3),selected = 'Deterministic'),
                                                conditionalPanel(condition = "input.ppr_model == 1",
                                                  wellPanel(
                                                    sliderInput(inputId = "wait_ppr", label = "Deterministic length of post-prandial resting bout:", value = 0.5 , min = 0, max = 1, step = 0.05)
                                                    )
                                                  ),


                                                conditionalPanel(condition = "input.ppr_model == 2",
                                                  wellPanel(
                                                    sliderInput(inputId = "rate_ppr", label = "Inverse of average length of post-prandial resting bout:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                    sliderInput(inputId = "tmin_ppr", label = "Minimum time of post-prandial resting bout:", value = 0.5 , min = 0, max = 1, step = 0.05)
                                                    )
                                                  ),

                                                conditionalPanel(condition = "input.ppr_model == 3",
                                                  wellPanel(
                                                    sliderInput(inputId = "mean_ppr", label = "Inverse of average length of post-prandial resting bout:", value = 0.5 , min = 0, max = 1, step = 0.05),
                                                    sliderInput(inputId = "cv_ppr", label = "Coefficient of variation between mean and variance of waiting time:", value = 0.5 , min = 0, max = 1, step = 0.05)
                                                    )
                                                  )

                                            )),
                                          tabPanel("Estivation model",
                                            column(4,
                                                selectInput("estivation_model", label = "Estivation Model", choice = list('Off' = 1, "Probabilistic" = 2, "Hard cut-off" = 3),selected = 'Off'),
                                                conditionalPanel(condition = "input.estivation_model == 2",
                                                  wellPanel(
                                                  sliderInput(inputId = "Emax", label = "Onset of dry season/period of estivation:", value = 90 , min = 0, max = 100, step = 1),
                                                  sliderInput(inputId = "Eb", label = "Scaling parameter for estivation probabilty:", value = 0.9 , min = 0, max = 1, step = 0.1),
                                                  sliderInput(inputId = "Ep", label = "Probability to survive estivation:", value = 0.5 , min = 0, max = 1, step = 0.1),
                                                  sliderInput(inputId = "eEndm", label = "Mean wake up day:", value = 100 , min = 0, max = 200, step = 1),
                                                  sliderInput(inputId = "eEndSd", label = "Standard deviation of wake up day:", value = 30 , min = 0, max = 200, step = 1)
                                                  )),
                                                conditionalPanel(condition = "input.estivation_model == 3",
                                                  wellPanel(
                                                  sliderInput(inputId = "estivationDay", label = "A calendar day to start estivation:", value = 1 , min = 1, max = 365, step = 1)
                                                  ))
                                            )),
                                          tabPanel("Mating",
                                            column(4,
                                                checkboxInput(inputId = "mating", label = "Mating", value = 0),
                                                conditionalPanel(condition = "input.mating == 1",
                                                  wellPanel(
                                                  sliderInput(inputId = "tSwarm", label = "Time of day of mating swarm formation (eg; noon is 12/24):", value = ParList()$tSwarm, min = 0, max = 1, step = 1/24, round = -3)
                                                  ))
                                                ))
                                          )),
                                          ######## Blood Meal #####################
                                          tabPanel("Blood Meal",
                                            column(4,
                                              helpText("Setup Blood Meal: "),
                                              wellPanel(
                                                sliderInput(inputId = "bm_a", label = "Alpha parameter of beta-distributed blood meal size:", value = ParList()$bm_a, min = 0, max = 10, step = 0.5),
                                                sliderInput(inputId = "bm_b", label = "Beta parameter of beta-distributed blood meal size:", value = ParList()$bm_b, min = 0, max = 10, step = 0.5),
                                                checkboxInput(inputId = "overfeeding", label = "Overfeeding", value = 1),
                                                conditionalPanel(condition = "input.overfeeding == 1",
                                                  sliderInput(inputId = "of_a", label = "Parameter a for probability of death from blood meal size:", value = ParList()$of_a, min = 0, max = 10, step = 0.5),
                                                  sliderInput(inputId = "of_b", label = "Parameter b for probability of death from blood meal size:", value = ParList()$of_b, min = 0, max = 10000, step = 100)
                                                  )
                                                )

                                              )
                                            ),

                                          ######## Oogenesis ######################
                                          tabPanel("Oogenesis",
                                            column(4,
                                                selectInput("oogenesis_model", label = "Oogenesis Model", choice = list('Option 1' = 1, "Option 2" = 2),selected = 'Option 1'),
                                                conditionalPanel(condition = "input.oogenesis_model == 1",
                                                  h4("Egg batch size proportional to blood meal size"),
                                                    sliderInput(inputId = "emt_m", label = "Mean of Gaussian distributed egg maturation time:", value = ParList()$emt_m, min = 0, max = 10, step = 1),
                                                    sliderInput(inputId = "emt_sd", label = "Standard deviation of Gaussian distributed egg maturation time:", value = ParList()$emt_sd, min = 0, max = 10, step = 1),
                                                    checkboxInput(inputId = "refeeding", label = "Refeeding behavior", value = 1),
                                                    conditionalPanel(condition = "input.refeeding == 1",
                                                      sliderInput(inputId = "rf_a", label = "Parameter a for refeeding probability:", value = ParList()$rf_a, min = 0, max = 20, step = 1),
                                                      sliderInput(inputId = "rf_b", label = "Parameter b for refeeding probability:", value = ParList()$rf_b, min = 0, max = 10, step = 1)
                                                      )
                                                    ),
                                                  conditionalPanel(condition = "input.oogenesis_model == 2",
                                                    h4("Egg batch size commits to development"),
                                                    sliderInput(inputId = "bloodPerEgg", label = "Amount of blood needed per egg", value = ParList()$bloodPerEgg, min = 0, max = 0.5, step = 0.05)
                                                  ),
                                                hr(),
                                                selectInput("eggsize_model", label = "Egg size Model", choice = list('Option 1' = 1, "Option 2" = 2),selected = 'Option 1'),
                                                conditionalPanel(condition = "input.eggsize_model == 1",
                                                  h4("sample Gaussian-distributed egg batch size"),
                                                  sliderInput(inputId = "bs_m", label = "Mean of Gaussian distribution:", value = ParList()$bs_m, min = 0, max = 50, step = 1),
                                                  sliderInput(inputId = "bs_sd", label = "Standard deviation of Gaussian distributed:", value = ParList()$bs_sd, min = 0, max = 50, step = 1)
                                                  ),
                                                conditionalPanel(condition = "input.eggsize_model == 2",
                                                  h4("Egg batch size is function of blood meal size"),
                                                  sliderInput(inputId = "maxBatch", label = "Maximum possible size of an egg batch", value = ParList()$maxBatch, min = 0, max = 100, step = 1)
                                                  )
                                                )
                                            ),

                                          ######## Energetics #####################
                                          tabPanel("Energetics",
                                            checkboxInput(inputId = "sugar", label = "Sugar feeding behavior", value = 1)
                                            ),

                                          #######  Oviposition ###################
                                          tabPanel("Oviposition",
                                            selectInput("aqua_model", label = "Aqua Model", choice = list('Emerge' = 'emerge', "EL4P" = 'EL4P'),selected = ParList()$aqua_model)
                                            ),

                                          #######  Survival ######################
                                          tabPanel("Survival",
                                            checkboxInput(inputId = "tattering", label = "Wing tattering derived contribution to mortality", value = FALSE),
                                            checkboxInput(inputId = "senescence", label = "Senescence derived contribution to mortality", value = FALSE)
                                            ),

                                          ####### Pathogen #######################
                                          tabPanel("Pathogen",
                                            helpText("Coming soon!")
                                            )
                                      ))
                                    })


##################### Output here ##########################################################################################


  #####################Simualtion output ########################################################
  output$sim_panel <- renderUI({
                          if(input$project == 'demo'){
                            sidebarLayout(position = "right",
                                          sidebarPanel(style = "overflow-y:scroll; max-height: 600px",
                                                       h4("Interested in parameters?"),
                                                       actionButton("JumpToMore", label = "Check it now!")
                                          ),
                                          mainPanel(
                                            helpText("insert plot here")

                                          ))
                          }else{
                            sidebarLayout(position = "right",
                                          sidebarPanel(style = "overflow-y:scroll; max-height: 600px",
                                                       h4("not for demo")
                                          ),
                                          mainPanel(
                                            helpText("not for demo")
                                          ))
                          }
  })
  

  ########################Demo Running and Plottting #########################################

  ################ Simulation ############################################################

############################### show all input #################################################################
  # AllInputs <- reactive({
  #   x <- reactiveValuesToList(input)
  #   data.frame(
  #     names = names(x),
  #     values = unlist(x, use.names = FALSE)
  #   )
  # })

  # output$show_inputs <- renderTable({
  #   AllInputs()
  # })


  


##################################     Observe Event   #######################################################
  
  ############################ Pipeline ###############################################
  
  observe({
    if (input$project == 'demo' && input$createDemoFolder > 0) {
      updateTabsetPanel(session, "nav", selected = "simulation")
    }
  })
  
  
  
  observe({
    if (input$project == 'exist' && input$launchgo > 0) {
      session$sendCustomMessage('activeNavs', 'Parameters')
      session$sendCustomMessage('activeNavs', 'Simulation')
      session$sendCustomMessage('activeNavs', 'Pathogen')
    }
  })
  observe({
    if (input$project == 'exist' && input$launchgo > 0) {
      updateTabsetPanel(session, "nav", selected = "initial")
    }
  })
  
  observe({
    if (input$project == 'demo' && input$createDemoFolder > 0) {
      session$sendCustomMessage('activeNavs', 'Simulation')
    }
  })
  observe({
    if (input$project == 'new' && input$createNewFolder > 0) {
      session$sendCustomMessage('activeNavs', 'Paramters')
      session$sendCustomMessage('activeNavs', 'Simulation')
      session$sendCustomMessage('activeNavs', 'Pathogen')
    }
  })
  
  observeEvent(input$JumpToSim,{
    session$sendCustomMessage('activeNavs', 'Simulation')
    updateTabsetPanel(session, "nav", selected = "simulation")
  })
  
  observeEvent(input$JumpToMore,{
    session$sendCustomMessage('activeNavs', 'Parameters')
    session$sendCustomMessage('activeNavs', 'Pathogen')
    updateTabsetPanel(session, "nav", selected = "initial")
  })
  
  observeEvent(input$createDemoFolder, {
    if(!file.exists("demo")){
      dir.create("demo")
      js_string <- 'alert("Created Demo Successfully!");'
      session$sendCustomMessage(type='jsCode', list(value = js_string))
    }else{
      unlink("demo", recursive = TRUE)
      dir.create("demo")
      js_string_2 <-'alert("New demo created! The previous demo has been removed.");'
      session$sendCustomMessage(type='jsCode', list(value = js_string_2))
    }
  })
  
  observeEvent(input$createNewFolder, {
    if(!file.exists(input$new_proj_name)){
      dir.create(input$new_proj_name)
      js_string_3 <- 'alert("Created New Project Successfully!");'
      session$sendCustomMessage(type='jsCode', list(value = js_string_3))
      updateTabsetPanel(session, "nav", selected = "initial")
      toggle(selector = "#nav li a[data-value=start]")
    }else{
      js_string_4 <-'alert("Folder exists. Please rename your project and try it again!");'
      session$sendCustomMessage(type='jsCode', list(value = js_string_4))
    }
  })
  
  observeEvent(input$save_inputs_bout, {
    js_string_5 <- 'alert("Parameters Saved!");'
    session$sendCustomMessage(type='jsCode', list(value = js_string_5))
  })

  observeEvent(input$run_demo_again, {
    js_string_7 <- 'alert("Your demo is updated! Please wait 1-2 minutes. The page will be automatically switched when it is done");'
    session$sendCustomMessage(type='jsCode', list(value = js_string_7))
    updateTabsetPanel(session, "nav", selected = "simulation")
  })
  
  observeEvent(input$save_demo_land, {
    js_string_6 <- 'alert("Selected Demo Sites have been saved in the folder: demo!");'
    session$sendCustomMessage(type='jsCode', list(value = js_string_6))
  })
  
  



  ##################### Observe Bouts ###########################################################
  observeEvent(input$Search_bout, {
    if("B" %in% input$Search_bout){
      updateTabsetPanel(session, "boutbar",selected = "B")
    }
  })
  observeEvent(input$Search_bout, {
    if("O" %in% input$Search_bout){
      updateTabsetPanel(session, "boutbar",selected = "O")
    }
  })

  observeEvent(input$Search_bout, {
    if("M" %in% input$Search_bout){
      updateTabsetPanel(session, "boutbar",selected = "M")
    }
  })
  observeEvent(input$Search_bout, {
    if("S" %in% input$Search_bout){
      updateTabsetPanel(session, "boutbar",selected = "S")
    }
  })
  observeEvent(input$Attempt_bout, {
    if("B" %in% input$Attempt_bout){
      updateTabsetPanel(session, "boutbar",selected = "B")
    }
  })
  observeEvent(input$Attempt_bout, {
    if("O" %in% input$Attempt_bout){
      updateTabsetPanel(session, "boutbar",selected = "O")
    }
  })

  observeEvent(input$Attempt_bout, {
    if("M" %in% input$Attempt_bout){
      updateTabsetPanel(session, "boutbar",selected = "M")
    }
  })
  observeEvent(input$Attempt_bout, {
    if("S" %in% input$Attempt_bout){
      updateTabsetPanel(session, "boutbar",selected = "S")
    }
  })
  
  ##########################################################################################
  
  
  
  observeEvent(input$done, {
    stopApp(brushedPoints(data, input$brush))
  })
  
  observeEvent(input$createDemoFolder, {
    toggle(selector = "#nav li a[data-value=start]")
  })
  #   observeEvent(input$createNewFolder, {
  #   toggle(selector = "#nav li a[data-value=start]")
  # })
  observeEvent(input$launchgo, {
    toggle(selector = "#nav li a[data-value=start]")
  })
  
}