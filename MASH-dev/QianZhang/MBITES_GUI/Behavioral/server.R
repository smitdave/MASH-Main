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
    # ttsz_a <- ParList()$ttsz_a
    # ttsz_b <- ParList()$ttsz_b
    #
    # ParList()$ttsz_mean <- ttsz_a/(ttsz_a + ttsz_b)
    # ParList()$ttsz_var <- (ttsz_a * ttsz_b)/((ttsz_a + ttsz_b)^2 * (ttsz_a + ttsz_b + 1))
})

  #################### Bouts Output #########################################################
  output$panel_bouts <- renderUI({
                          fluidPage(
                              tabsetPanel(id = "boutbar",
                                helpText("Welcome to MBITES! Please turn on/off the bouts here:"),
                                tabPanel("B",
                                      column(4,
                                          wellPanel(
                                            helpText("B: Blood Feeding: Enter a vector (comma delimited) here"),
                                            textInput("b_wts", "Landing Spot Weights:", value = paste(ParList()$b_wts, collapse = ","))),
                                          wellPanel(
                                            sliderInput(inputId = "Bs_succeed", label ="Probability of Success for Blood Feeding Search", value = ParList()$Bs_succeed, min = 0.8, max = 1, step = 0.01),                #
                                            sliderInput(inputId = "Bs_surv", label ="Baseline Probability of Survival for Blood Feeding Search",value = ParList()$Bs_surv, min = 0.9, max = 1, step = 0.01)
                                            ),
                                          wellPanel(
                                            sliderInput(inputId = "B_succeed", label ="Probability of Success for Blood Feeding Attempt", value = ParList()$B_succeed, min = 0.8, max = 1, step = 0.01),                #
                                            sliderInput(inputId = "B_surv", label ="Baseline Probability of Survival for Blood Feeding Attempt",value = ParList()$B_surv, min = 0.9, max = 1, step = 0.01)
                                            )
                                      )
                                      ),
                                    tabPanel("O",
                                      column(4,
                                          wellPanel(
                                            helpText("O: Oviposition: Enter a vector (comma delimited) here"),
                                            textInput("o_wts", "Landing Spot Weights:", value = paste(ParList()$o_wts, collapse = ","))
                                        ),
                                          wellPanel(
                                            sliderInput(inputId = "Os_succeed", label ="Probability of Success for Oviposition Search", value = ParList()$Os_succeed, min = 0.8, max = 1, step = 0.01),                #
                                            sliderInput(inputId = "Os_surv", label ="Baseline Probability of Survival for Oviposition Search",value = ParList()$Os_surv, min = 0.9, max = 1, step = 0.01)
                                          ),
                                          wellPanel(
                                            sliderInput(inputId = "O_succeed", label ="Probability of Success for Oviposition Attempt", value = ParList()$O_succeed, min = 0.8, max = 1, step = 0.01),                #
                                            sliderInput(inputId = "O_surv", label ="Baseline Probability of Survival for Oviposition Attempt",value = ParList()$O_surv, min = 0.9, max = 1, step = 0.01)
                                          )
                                        )
                                      ),
                                    tabPanel("M",
                                      column(4,
                                        conditionalPanel(condition = "!(input.M_search || input.M_attempt)",
                                          wellPanel(
                                            helpText("Mating is turned off"))
                                          ),
                                        conditionalPanel(condition = "input.M_search || input.M_attempt",
                                          wellPanel(
                                            helpText("M: Mating: Enter a vector (comma delimited) here"),
                                            textInput("m_wts", "Landing Spot Weights:", value = paste(ParList()$m_wts, collapse = ",")))
                                        ),
                                        checkboxInput(inputId = "M_search", label = "Mating Search", value = 0),
                                        conditionalPanel(condition = "input.M_search", 
                                          wellPanel(
                                            sliderInput(inputId = "Ms_succeed", label ="Probability of Success for Mating Search", value = ParList()$Ms_succeed, min = 0.8, max = 1, step = 0.01),                #
                                            sliderInput(inputId = "Ms_surv", label ="Baseline Probability of Survival for Mating Search",value = ParList()$Ms_surv, min = 0.9, max = 1, step = 0.01)
                                            )),
                                        checkboxInput(inputId = "M_attempt", label = "Mating Attempt", value = 0),
                                        conditionalPanel(condition = "input.M_attempt", 
                                          wellPanel(
                                            sliderInput(inputId = "M_succeed", label ="Probability of Success for Mating Attempt", value = ParList()$M_succeed, min = 0.8, max = 1, step = 0.01),                #
                                            sliderInput(inputId = "M_surv", label ="Baseline Probability of Survival for Mating Attempt",value = ParList()$M_surv, min = 0.9, max = 1, step = 0.01)
                                          ))
                                      )),
                                    tabPanel("S",
                                      column(4,
                                        conditionalPanel(condition = "!(input.S_search || input.S_attempt)",
                                          wellPanel(
                                            helpText("Sugar Feeding is turned off"))
                                          ),
                                        conditionalPanel(condition = "input.S_search || input.S_attempt",
                                          wellPanel(
                                            helpText("S: Sugar Feeding: Enter a vector (comma delimited) here"),
                                            textInput("s_wts", "Landing Spot Weights:", value = paste(ParList()$s_wts, collapse = ",")))
                                        ),
                                        checkboxInput(inputId = "S_search", label = "Sugar Feeding Search", value = 0),
                                        conditionalPanel(condition = "input.S_search", 
                                          wellPanel(
                                            sliderInput(inputId = "Ss_succeed", label ="Probability of Success for Sugar Feeding Search", value = ParList()$Ss_succeed, min = 0.8, max = 1, step = 0.01),                #
                                            sliderInput(inputId = "Ss_surv", label ="Baseline Probability of Survival for Sugar Feeding Search",value = ParList()$Ss_surv, min = 0.9, max = 1, step = 0.01)
                                            )),
                                        checkboxInput(inputId = "S_attempt", label = "Sugar Feeding Attempt", value = 0),
                                        conditionalPanel(condition = "input.S_attempt", 
                                          wellPanel(
                                            sliderInput(inputId = "S_succeed", label ="Probability of Success for Sugar Feeding Attempt", value = ParList()$S_succeed, min = 0.8, max = 1, step = 0.01),                #
                                            sliderInput(inputId = "S_surv", label ="Baseline Probability of Survival for Sugar Feeding Attempt",value = ParList()$S_surv, min = 0.9, max = 1, step = 0.01)
                                          ))
                                      ))


                              )
                            )
  })


  #################### Parameters Output #####################################################
  output$panel_initial <- renderUI({
                            fluidPage(
                             helpText("Please set up more parameters here:"),
                             navlistPanel(widths = c(2,10),
                                          ######### Timing #######################
                                          tabPanel("Timing",
                                            tabsetPanel(
                                              ######################### Timing model panel ##################################################################
                                              tabPanel("Timing model",
                                                helpText("Timing model for for attempt & search bout inter-launch time-to-event sampling distribution."),
                                                column(4,
                                                       selectInput("timing_model", label = "Timing Model", choice = list('Deterministic' = 1, "Exponential" = 2, "Gamma" = 3),selected = 'Deterministic'),
                                                        ######################### Timing model 1 ##################################################################
                                                       conditionalPanel(condition = "input.timing_model == 1",
                                                        tabsetPanel(id = "timing_model_1",
                                                          tabPanel("B",
                                                              wellPanel(
                                                                helpText("Search Bout:"),
                                                              sliderInput(inputId = "wait_bs", label = "Blood feeding search:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              ),
                                                            hr(),
                                                              wellPanel(
                                                                helpText("Attempt Bout:"),
                                                              sliderInput(inputId = "wait_b", label = "Blood feeding attempt:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              )
                                                            ),
                                                          tabPanel("O",
                                                              wellPanel(
                                                                helpText("Search Bout:"),
                                                              sliderInput(inputId = "wait_os", label = "Oviposition search:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              ),
                                                            hr(),
                                                              wellPanel(
                                                                helpText("Attempt Bout:"),
                                                              sliderInput(inputId = "wait_o", label = "Oviposition attempt:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              )
                                                            ),
                                                          tabPanel("M",
                                                            conditionalPanel(condition = "input.M_search == 1",
                                                              wellPanel(
                                                                helpText("Search Bout:"),
                                                              sliderInput(inputId = "wait_ms", label = "Mating search:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              )),
                                                            conditionalPanel(condition = "input.M_search == 0",
                                                              helpText("Mating Search Bout is turned off")
                                                              ),
                                                            hr(),
                                                            conditionalPanel(condition = "input.M_attempt == 1",
                                                              wellPanel(
                                                                helpText("Attempt Bout:"),
                                                              sliderInput(inputId = "wait_m", label = "Mating attempt:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              )),
                                                            conditionalPanel(condition = "input.M_attempt == 0",
                                                              helpText("Mating Attempt Bout is turned off")
                                                              )
                                                            ),
                                                          tabPanel("S",
                                                            conditionalPanel(condition = "input.S_search == 1",
                                                              wellPanel(
                                                                helpText("Search Bout:"),
                                                              sliderInput(inputId = "wait_ss", label = "Sugar feeding search:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              )),
                                                            conditionalPanel(condition = "input.S_search == 0",
                                                              helpText("Sugar Feeding Search Bout is turned off")
                                                              ),
                                                            hr(),
                                                            conditionalPanel(condition = "input.S_attempt == 1",
                                                              wellPanel(
                                                                helpText("Attempt Bout:"),
                                                              sliderInput(inputId = "wait_s", label = "Sugar feeding attempt:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              )),
                                                            conditionalPanel(condition = "input.S_attempt == 0",
                                                              helpText("Sugar Feeding Attempt Bout is turned off")
                                                              )
                                                            )
                                                        )),
                                                       ######################### Timing model 2 ##################################################################
                                                       conditionalPanel(condition = "input.timing_model == 2",
                                                        helpText("Define 'the Average waiting time' and 'the minimum waiting time prior to next launch':"),
                                                         tabsetPanel(id = "timing_model_2",
                                                          tabPanel("B",
                                                              wellPanel(
                                                                helpText("Search Bout:"),
                                                              sliderInput(inputId = "avg_bs", label = "Average:", value = 12 , min = 0, max = 24, step = 0.25), #1/rate_bs
                                                              sliderInput(inputId = "tmin_bs", label = "Minimum:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              ),
                                                            hr(),
                                                              wellPanel(
                                                                helpText("Attempt Bout:"),
                                                              sliderInput(inputId = "avg_b", label = "Average:", value = 12 , min = 0, max = 24, step = 0.25),
                                                              sliderInput(inputId = "tmin_b", label = "Minimum:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              )
                                                            ),
                                                          tabPanel("O",
                                                              wellPanel(
                                                                helpText("Search Bout:"),
                                                              sliderInput(inputId = "avg_os", label = "Average:", value = 12 , min = 0, max = 24, step = 0.25),
                                                              sliderInput(inputId = "tmin_os", label = "Minimum:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              ),
                                                            hr(),
                                                              wellPanel(
                                                                helpText("Attempt Bout:"),
                                                              sliderInput(inputId = "avg_o", label = "Average:", value = 12 , min = 0, max = 24, step = 0.25),
                                                              sliderInput(inputId = "tmin_o", label = "Minimum:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              )
                                                            ),
                                                          tabPanel("M",
                                                            conditionalPanel(condition = "input.M_search == 1",
                                                              wellPanel(
                                                                helpText("Search Bout:"),
                                                              sliderInput(inputId = "avg_ms", label = "Average:", value = 12 , min = 0, max = 24, step = 0.25),
                                                              sliderInput(inputId = "tmin_ms", label = "Minimum:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              )),
                                                            conditionalPanel(condition = "input.M_search == 0",
                                                              helpText("Mating Search Bout is turned off")
                                                              ),
                                                            hr(),
                                                            conditionalPanel(condition = "input.M_attempt == 1",
                                                              wellPanel(
                                                                helpText("Attempt Bout:"),
                                                              sliderInput(inputId = "avg_m", label = "Average:", value = 12 , min = 0, max = 24, step = 0.25),
                                                              sliderInput(inputId = "tmin_m", label = "Minimum:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              )),
                                                            conditionalPanel(condition = "input.M_attempt == 0",
                                                              helpText("Mating Attempt Bout is turned off")
                                                              )
                                                            ),
                                                          tabPanel("S",
                                                            conditionalPanel(condition = "input.S_search == 1",
                                                              wellPanel(
                                                                helpText("Search Bout:"),
                                                              sliderInput(inputId = "avg_ss", label = "Average:", value = 12 , min = 0, max = 24, step = 0.25),
                                                              sliderInput(inputId = "tmin_ss", label = "Minimum:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              )),
                                                            conditionalPanel(condition = "input.S_search == 0",
                                                              helpText("Sugar Feeding Search Bout is turned off")
                                                              ),
                                                            hr(),
                                                            conditionalPanel(condition = "input.S_attempt == 1",
                                                              wellPanel(
                                                                helpText("Attempt Bout:"),
                                                              sliderInput(inputId = "avg_s", label = "Average:", value = 12 , min = 0, max = 24, step = 0.25),
                                                              sliderInput(inputId = "tmin_s", label = "Minimum:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              )),
                                                            conditionalPanel(condition = "input.S_attempt == 0",
                                                              helpText("Sugar Feeding Attempt Bout is turned off")
                                                              )
                                                            )
                                                          )
                                                        ),

                                                       conditionalPanel(condition = "input.timing_model == 3",
                                                        helpText("Define 'the average waiting time' and 'Coefficient of variation between mean and variance of waiting time':"),
                                                        tabsetPanel(id = "timing_model_3",
                                                          tabPanel("B",
                                                              wellPanel(
                                                                helpText("Search Bout:"),
                                                              sliderInput(inputId = "ivs_mean_bs", label = "Average:", value = 12 , min = 0, max = 24, step = 0.25),
                                                              sliderInput(inputId = "cv_bs", label = "Coefficient:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              ),
                                                            hr(),
                                                              wellPanel(
                                                                helpText("Attempt Bout:"),
                                                              sliderInput(inputId = "ivs_mean_b", label = "Average:", value = 12 , min = 0, max = 24, step = 0.25),
                                                              sliderInput(inputId = "cv_b", label = "Coefficient:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              )
                                                            ),
                                                          tabPanel("O",
                                                              wellPanel(
                                                                helpText("Search Bout:"),
                                                              sliderInput(inputId = "ivs_mean_os", label = "Average:", value = 12 , min = 0, max = 24, step = 0.25),
                                                              sliderInput(inputId = "cv_os", label = "Coefficient:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              ),
                                                            hr(),
                                                              wellPanel(
                                                                helpText("Attempt Bout:"),
                                                              sliderInput(inputId = "ivs_mean_o", label = "Average:", value = 12 , min = 0, max = 24, step = 0.25),
                                                              sliderInput(inputId = "cv_o", label = "Coefficient:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              )
                                                            ),
                                                          tabPanel("M",
                                                            conditionalPanel(condition = "input.M_search == 1",
                                                              wellPanel(
                                                                helpText("Search Bout:"),
                                                              sliderInput(inputId = "ivs_mean_ms", label = "Average:", value = 12 , min = 0, max = 24, step = 0.25),
                                                              sliderInput(inputId = "cv_ms", label = "Coefficient:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              )),
                                                            conditionalPanel(condition = "input.M_search == 0",
                                                              helpText("Mating Search Bout is turned off")
                                                              ),
                                                            hr(),
                                                            conditionalPanel(condition = "input.M_attempt == 1",
                                                              wellPanel(
                                                                helpText("Attempt Bout:"),
                                                              sliderInput(inputId = "ivs_mean_m", label = "Average:", value = 12 , min = 0, max = 24, step = 0.25),
                                                              sliderInput(inputId = "cv_m", label = "Coefficient:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              )),
                                                            conditionalPanel(condition = "input.M_attempt == 0",
                                                              helpText("Mating Attempt Bout is turned off")
                                                              )
                                                            ),
                                                          tabPanel("S",
                                                            conditionalPanel(condition = "input.S_search == 1",
                                                              wellPanel(
                                                                helpText("Search Bout:"),
                                                              sliderInput(inputId = "ivs_mean_ss", label = "Average:", value = 12 , min = 0, max = 24, step = 0.25),
                                                              sliderInput(inputId = "cv_ss", label = "Coefficient:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              )),
                                                            conditionalPanel(condition = "input.S_search == 0",
                                                              helpText("Sugar Feeding Search Bout is turned off")
                                                              ),
                                                            hr(),
                                                            conditionalPanel(condition = "input.S_attempt == 1",
                                                              wellPanel(
                                                                helpText("Attempt Bout:"),
                                                              sliderInput(inputId = "ivs_mean_s", label = "Average:", value = 12 , min = 0, max = 24, step = 0.25),
                                                              sliderInput(inputId = "cv_s", label = "Coefficient:", value = 12 , min = 0, max = 24, step = 0.25)
                                                              )),
                                                            conditionalPanel(condition = "input.S_attempt == 0",
                                                              helpText("Sugar Feeding Attempt Bout is turned off")
                                                              )
                                                            )
                                                          )

                                                        )
                                          )),

                                      ########################## PPR model ################################################################################################################

                                          tabPanel("PPR model",
                                            helpText("Post-prandial resting length sampling distribution:"),
                                            column(4,
                                                selectInput("ppr_model", label = "PPR Model", choice = list('Deterministic' = 1, "Exponential" = 2, "Gamma" = 3),selected = 'Deterministic'),
                                                conditionalPanel(condition = "input.ppr_model == 1",
                                                  wellPanel(
                                                    sliderInput(inputId = "wait_ppr", label = "Deterministic length of post-prandial resting bout:", value = 24 , min = 0, max = 72, step = 0.25)
                                                    )
                                                  ),


                                                conditionalPanel(condition = "input.ppr_model == 2",
                                                  wellPanel(
                                                    sliderInput(inputId = "inv_rate_ppr", label = "Average length of post-prandial resting bout:", value = 24 , min = 0, max = 72, step = 0.25), #1/rate_ppr
                                                    sliderInput(inputId = "tmin_ppr", label = "Minimum time of post-prandial resting bout:", value = 24 , min = 0, max = 72, step = 0.25)
                                                    )
                                                  ),

                                                conditionalPanel(condition = "input.ppr_model == 3",
                                                  wellPanel(
                                                    sliderInput(inputId = "inv_mean_ppr", label = "Average length of post-prandial resting bout:", value = 24 , min = 0, max = 72, step = 0.25),
                                                    sliderInput(inputId = "cv_ppr", label = "Coefficient of variation between mean and variance of waiting time:", value = 24 , min = 0, max = 72, step = 0.25)
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
                                                conditionalPanel(condition = "input.M_attempt == 1 || input.M_search == 1",
                                                  wellPanel(
                                                  sliderInput(inputId = "tSwarm", label = "Time of day of mating swarm formation (eg; noon is 12/24):", value = ParList()$tSwarm, min = 0, max = 1, step = 1/24, round = -3)
                                                  )),
                                                conditionalPanel(condition = "!(input.M_attempt == 1 || input.M_search == 1)",
                                                  wellPanel(
                                                    helpText("Mating bout is turned off.")
                                                    )
                                                  )
                                                ))
                                          )),
                                          ######## Blood Meal #####################
                                          tabPanel("Blood Meal",
                                            fluidRow(
                                              column(4,
                                                helpText("Setup the beta-distributed Blood Meal: "),
                                                wellPanel(
                                                  sliderInput(inputId = "bm_u", label = "Mean of blood meal size:", value = ParList()$bm_a/(ParList()$bm_a + ParList()$bm_b) , min = 0, max = 1, step = 0.05), #bm_a = uv
                                                  sliderInput(inputId = "bm_v", label = "Sample size of blood meal size:", value = (ParList()$bm_a + ParList()$bm_b), min = 0, max = 20, step = 0.5) #bm_b = (1-u)v
                                                  )
                                                ),
                                              column(8,
                                                plotOutput("bm_plot")
                                                )
                                            ),
                                            fluidRow(
                                              column(4,
                                                wellPanel(
                                                  checkboxInput(inputId = "overfeeding", label = "Overfeeding", value = 1),
                                                  conditionalPanel(condition = "input.overfeeding == 1",
                                                    sliderInput(inputId = "of_a", label = "Parameter a of death from overfeeding:", value = ParList()$of_a, min = 0, max = 10, step = 0.5),
                                                    sliderInput(inputId = "of_b", label = "Parameter b of death from overfeeding:", value = ParList()$of_b, min = 0, max = 10000, step = 100)
                                                    )
                                                  )
                                                ),
                                              column(8,
                                                plotOutput("of_plot")
                                                )
                                            )),

                                          ######## Oogenesis ######################
                                          tabPanel("Oogenesis",
                                            column(4,
                                                selectInput("oogenesis_model", label = "Oogenesis Model", choice = list('Option 1' = 1, "Option 2" = 2),selected = 'Option 1'),
                                                conditionalPanel(condition = "input.oogenesis_model == 1",
                                                  h5("Egg batch size proportional to blood meal size"),
                                                  wellPanel(
                                                    sliderInput(inputId = "emt_m", label = "Mean of Gaussian distributed egg maturation time:", value = ParList()$emt_m, min = 0, max = 10, step = 1),
                                                    sliderInput(inputId = "emt_sd", label = "Standard deviation of Gaussian distributed egg maturation time:", value = ParList()$emt_sd, min = 0, max = 10, step = 1)),
                                                  wellPanel(
                                                    checkboxInput(inputId = "refeeding", label = "Refeeding behavior", value = 1),
                                                    conditionalPanel(condition = "input.refeeding == 1",
                                                      sliderInput(inputId = "rf_a", label = "Parameter a for refeeding probability:", value = ParList()$rf_a, min = 0, max = 20, step = 1),
                                                      sliderInput(inputId = "rf_b", label = "Parameter b for refeeding probability:", value = ParList()$rf_b, min = 0, max = 10, step = 1)
                                                      )
                                                    )),
                                                  conditionalPanel(condition = "input.oogenesis_model == 2",
                                                    h5("Egg batch size commits to development"),
                                                    sliderInput(inputId = "bloodPerEgg", label = "Amount of blood needed per egg", value = ParList()$bloodPerEgg, min = 0, max = 0.5, step = 0.05)
                                                  ),
                                                hr(),
                                                wellPanel(
                                                selectInput("eggsize_model", label = "Egg size Model", choice = list('Option 1' = 1, "Option 2" = 2),selected = 'Option 1'),
                                                conditionalPanel(condition = "input.eggsize_model == 1",
                                                  h5("sample Gaussian-distributed egg batch size"),
                                                  sliderInput(inputId = "bs_m", label = "Mean of Gaussian distribution:", value = ParList()$bs_m, min = 0, max = 50, step = 1),
                                                  sliderInput(inputId = "bs_sd", label = "Standard deviation of Gaussian distributed:", value = ParList()$bs_sd, min = 0, max = 50, step = 1)
                                                  ),
                                                conditionalPanel(condition = "input.eggsize_model == 2",
                                                  h5("Egg batch size is function of blood meal size"),
                                                  sliderInput(inputId = "maxBatch", label = "Maximum possible size of an egg batch", value = ParList()$maxBatch, min = 0, max = 100, step = 1)
                                                  )
                                                )),
                                            column(8,
                                              plotOutput("savespace"),
                                              plotOutput("rf_plot"))
                                            ),

                                          ######## Energetics #####################
                                          tabPanel("Energetics",
                                            checkboxInput(inputId = "sugar", label = "Sugar feeding behavior", value = 1),
                                            conditionalPanel(condition = "input.sugar",
                                              fluidRow(
                                                column(4,
                                                  wellPanel(
                                                    sliderInput(inputId = "S_sa", label ="Shape Param a of Probability to queue Sugar bout", value = ParList()$S_sa, min = 0, max = 100, step = 1),
                                                    sliderInput(inputId = "S_sb", label ="Shape Param b of Probability to queue Sugar bout", value = ParList()$S_sb, min = 0, max = 100, step = 1),
                                                    sliderInput(inputId = "S_w", label ="Shape Param a of Probability to queue Sugar bout", value = ParList()$S_sa, min = 0, max = 100, step = 1),
                                                    sliderInput(inputId = "S_p", label ="Shape Param b of Probability to queue Sugar bout", value = ParList()$S_sb, min = 0, max = 100, step = 1),
                                                    sliderInput(inputId = "energyPreG", label ="Pre-gonotrophic Energy Requirement", value = ParList()$energyPreG, min = 0, max = 100, step = 1),
                                                    sliderInput(inputId = "preGsugar", label ="Amount of Energy a Sugar Meal Contributes to Pre-gonotrophic Energy Requirement (%)", value = ParList()$preGsugar, min = 0, max = 100, step = 1),
                                                    sliderInput(inputId = "omega", label ="Omega", value = ParList()$omega, min = 0, max = 100, step = 1),
                                                    sliderInput(inputId = "energyFromBlood_b", label ="Half-maximum parameter for energy from blood", value = ParList()$energyFromBlood_b, min = 0, max = 1, step = 0.01)

                                                  )
                                                  )
                                                )
                                              )
                                            ),

                                          #######  Oviposition ###################
                                          tabPanel("Oviposition",
                                            selectInput("aqua_model", label = "Aqua Model", choice = list('Emerge' = 'emerge', "EL4P" = 'EL4P'),selected = ParList()$aqua_model)
                                            ),

                                          #######  Survival ######################
                                          tabPanel("Survival",
                                            checkboxInput(inputId = "tattering", label = "Wing-Tattering-Derived Contribution to Mortality", value = FALSE),
                                              conditionalPanel(condition = "input.tattering",
                                                fluidRow(
                                                  column(4,
                                                    wellPanel(
                                                      sliderInput(inputId = "ttsz_p", label = "Probability of Wing Damage Occurring:", value = ParList()$ttsz_p, min = 0, max = 1, step = 0.01)
                                                    )
                                                  )
                                                ),
                                                fluidRow(
                                                  column(4,
                                                    wellPanel(
                                                      h4("Select Parameters for How Much Damage Occurs:"),
                                                      h6("This distribution determines how much damage will occur during a bout"),
                                                      sliderInput(inputId = "ttsz_mean", label = "Mean of Damage Distribution:", value = ParList()$ttsz_a/(ParList()$ttsz_a + ParList()$ttsz_b), min = 0, max = 1, step = 0.01),
                                                      sliderInput(inputId = "ttsz_var", label = "Variance of Damage Distribution:", value = (ParList()$ttsz_a * ParList()$ttsz_b)/((ParList()$ttsz_a + ParList()$ttsz_b)^2 * (ParList()$ttsz_a + ParList()$ttsz_b + 1)), min = 0, max = 0.25, step = 0.01)
                                                      # sliderInput(inputId = "ttsz_a", label = "'a' Parameter of Beta Distribution:", value = ParList()$ttsz_a, min = 0, max = 10, step = 1),
                                                      # sliderInput(inputId = "ttsz_b", label = "'b' Parameter of Beta Distribution:", value = ParList()$ttsz_b, min = 0, max = 100, step = 1)
                                                    )
                                                  ),
                                                  column(6,
                                                    plotOutput("ttr_density_plot")
                                                  )
                                                ),
                                                fluidRow(
                                                  column(4,
                                                    wellPanel(
                                                      h4("Select Parameters for Damage-Induced Mortality:"),
                                                      h6("This distribution determines how much excess mortality is added by the amount of damage sustained"),
                                                      sliderInput(inputId = "ttr_a", label = "'a' Parameter:", value = ParList()$ttr_a, min = 0, max = 100, step = 1),
                                                      sliderInput(inputId = "ttr_b", label = "'b' Parameter:", value = ParList()$ttr_b, min = 0, max = 100, step = 1)
                                                    )
                                                  ),
                                                  column(6,
                                                    plotOutput("ttr_damage_plot")
                                                  )
                                                )
                                              ),
                                            checkboxInput(inputId = "senescence", label = "Senescence-Derived Contribution to Mortality", value = FALSE),
                                              conditionalPanel(condition = "input.senescence",
                                                fluidRow(
                                                  column(4,
                                                    wellPanel(
                                                      h5("Select Parameters for Senescence Distrbution:"),
                                                      sliderInput(inputId = "sns_a", label = "'a' Parameter:", value = ParList()$sns_a, min = 0, max = 1, step = 0.01),
                                                      sliderInput(inputId = "sns_b", label = "'b' Parameter:", value = ParList()$sns_b, min = 0, max = 1000, step = 10)
                                                    )
                                                  ),
                                                  column(6,
                                                    plotOutput("sns_plot")
                                                  )
                                                )

                                              )

                                            ),

                                          ####### Pathogen #######################
                                          tabPanel("Pathogen",
                                            helpText("Coming soon!")
                                            )
                                      ))
                                    })


##################### Output here ##########################################################################################


####################### Plot Output #####################################################################


####################### Parameters output ###############################################################

####################### Blood Meal Output ###############################################################
output$bm_plot <- renderPlot({
      bm_a <- input$bm_u * input$bm_v
      bm_b <- (1 - input$bm_u) * input$bm_v
      rBloodMealSize = function(bm_a, bm_b){rbeta(1,bm_a,bm_b)}
      hist(replicate(10000, rBloodMealSize(bm_a, bm_b)), 20, main = "Blood Meal Size", xlab = "Proportion of Full", probability=TRUE)
  })

output$of_plot <- renderPlot({
  if(input$overfeeding){
  pOverFeed <- function(x,of_a,of_b){exp(of_a*x)/(of_b + exp(of_a*x))}
  x=seq(0,1,length.out=100)
  plot(x, pOverFeed(x, input$of_a, input$of_b),type = "l", col ="blue", xlab= "Size of blood meal", ylab = "Mortality", main = "Overfeeding Mortality")}
})


#################### Oogenesis Output ####################################################################
output$rf_plot <- renderPlot({
  if(input$refeeding){
    pReFeed <- function(x,rf_a,rf_b){1 - exp(rf_a*x)/(exp(rf_b)+ exp(rf_a*x))}
    x=seq(0,1,length.out=100)
    plot(x, pReFeed(x, input$rf_a, input$rf_b),type = "l", col ="blue", xlab= "Size of blood meal", ylab = "Probability to refeed", main = "Refeeding")
  }
})

#################### Survival Output ####################################################################
output$ttr_density_plot <- renderPlot({
  beta_a <- input$ttsz_mean^2*((1-input$ttsz_mean)/input$ttsz_var^2 + 1/input$ttsz_mean)
  beta_b <- beta_a*(1/input$ttsz_mean - 1)
  x_values <- seq(0,1,length.out=100)

  plot(x_values, stats::dbeta(x_values, shape1 = beta_a, shape2 = beta_b), type = "l", col = "blue", xlab = "Physical Damage", ylab = "Density", main = "Physical Damage (Wing Tattering)")
})

output$sns_plot <- renderPlot({
  sns_a <- input$sns_a
  sns_b <- input$sns_b
  sns_func <- function(x, sns_a, sns_b) {
    (2+sns_b)/(1+sns_b) - exp(x*sns_a)/(sns_b+exp(x*sns_a))
  }
  x_values <- seq(0,30,length.out=100)

  plot(x_values, sns_func(x_values, sns_a, sns_b), type = "l", col = "blue", xlab = "Age (Days)", ylab = "Marginal Survival, per Bout", main = "Senescence")
})

output$ttr_damage_plot <- renderPlot({
  ttr_a <- input$ttr_a
  ttr_b <- input$ttr_b
  ttr_func <- function(x, ttr_a, ttr_b) {
    (2+exp(ttr_b))/(1+exp(ttr_b)) - exp(x*ttr_a)/(exp(ttr_b) + exp(x*ttr_a))
  }
  x_values <- seq(0,1,length.out=100)

  plot(x_values, ttr_func(x_values, ttr_a, ttr_b), type = "l", col = "blue", xlab = "Physical Damage", ylab = "Marginal Survival, per Bout", main = "Physical Damage (Wing Tattering)")
})

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
      session$sendCustomMessage('activeNavs', 'Bouts')
      session$sendCustomMessage('activeNavs', 'Parameters')
      session$sendCustomMessage('activeNavs', 'Simulation')
      session$sendCustomMessage('activeNavs', 'Pathogen')
    }
  })
  observe({
    if (input$project == 'exist' && input$launchgo > 0) {
      updateTabsetPanel(session, "nav", selected = "bouts")
    }
  })

  observe({
    if (input$project == 'demo' && input$createDemoFolder > 0) {
      session$sendCustomMessage('activeNavs', 'Simulation')
    }
  })
  observe({
    if (input$project == 'new' && input$createNewFolder > 0) {
      session$sendCustomMessage('activeNavs', 'Bouts')
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
    session$sendCustomMessage('activeNavs', 'Bouts')
    session$sendCustomMessage('activeNavs', 'Parameters')
    session$sendCustomMessage('activeNavs', 'Pathogen')
    updateTabsetPanel(session, "nav", selected = "bouts")
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





  ##################### Observe Bouts ########################################################### need debug

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
