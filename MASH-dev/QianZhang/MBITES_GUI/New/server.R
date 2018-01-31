#########################################################################################
# SERVER
#########################################################################################
server <- function(input, output, session) {
  cat(getwd())
  output$plot <- renderPlot({
    ggplot(data, aes_string(xvar, yvar)) + geom_point()
  })

  #################### Loading parameter json file #####################################

  demo_par = fromJSON("demo_json/demo.json", flatten=TRUE)

  ParList = demo_par
  
  #################### Option Output #####################################################
  output$panel_options <- renderUI({
                           fluidPage(
                             helpText("Please set parameters here. To save the selected parameters, go to 
                                      'Bouts' panel and click the button when both panels are done"),
                             navlistPanel(widths = c(2,10),
                                          
                                          #########################################################################
                                          tabPanel("Resting Time",
                                                   tabsetPanel(
                                                     tabPanel("F",
                                                              column(4,
                                                                     h5("Mean Time Elapsed: "),
                                                                     fluidRow(
                                                                       column(4,selectInput("F_time_h", label = "Hours", choices = seq(0,24,1) , selected = trunc(ParList$F_time), width = "100%")),
                                                                       column(4,selectInput("F_time_m", label = "Minutes", choices = seq(0,59,1), selected = (ParList$F_time%%1*60), width = "100%"))
                                                                     ),
                                                                     radioButtons("F_dist", "Distribution type:",
                                                                                  c("Exponentional" = "exp",
                                                                                    "Gamma" = "gamma"), selected = ParList$F_dist, inline = TRUE),

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
                                                                       column(4,selectInput("B_time_h", label = "Hours", choices = seq(0,24,1) , selected = trunc(ParList$B_time), width = "100%")),
                                                                       column(4,selectInput("B_time_m", label = "Minutes", choices = seq(0,59,1), selected = (ParList$B_time%%1*60), width = "100%"))
                                                                     ),
                                                                     radioButtons("B_dist", "Distribution type:",
                                                                                  c("Exponentional" = "exp",
                                                                                    "Gamma" = "gamma"), selected = ParList$B_dist, inline = TRUE),
 
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
                                                                       column(4,selectInput("R_time_h", label = "Hours", choices = seq(0,24,1) , selected = trunc(ParList$R_time), width = "100%")),
                                                                       column(4,selectInput("R_time_m", label = "Minutes", choices = seq(0,59,1), selected = (ParList$R_time%%1*60), width = "100%"))
                                                                     ),
                                                                     radioButtons("R_dist", "Distribution type:",
                                                                                  c("Exponentional" = "exp",
                                                                                    "Gamma" = "gamma"), selected = ParList$R_dist, inline = TRUE),

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
                                                                       column(4,selectInput("L_time_h", label = "Hours", choices = seq(0,24,1) , selected = trunc(ParList$L_time), width = "100%")),
                                                                       column(4,selectInput("L_time_m", label = "Minutes", choices = seq(0,59,1), selected = (ParList$L_time%%1*60), width = "100%"))
                                                                     ),
                                                                     radioButtons("L_dist", "Distribution type:",
                                                                                  c("Exponentional" = "exp",
                                                                                    "Gamma" = "gamma"), selected = ParList$L_dist,  inline = TRUE),

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
                                                                       column(4,selectInput("O_time_h", label = "Hours", choices = seq(0,24,1) , selected = trunc(ParList$O_time), width = "100%")),
                                                                       column(4,selectInput("O_time_m", label = "Minutes", choices = seq(0,59,1), selected = (ParList$O_time%%1*60), width = "100%"))
                                                                     ),
                                                                     radioButtons("O_dist", "Distribution type:",
                                                                                  c("Exponentional" = "exp",
                                                                                    "Gamma" = "gamma"), selected = ParList$O_dist,  inline = TRUE),

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

                                                     tabPanel("M",
                                                              column(4,
                                                                     h5("Mean Time Elapsed: "),
                                                                     fluidRow(
                                                                       column(4,selectInput("M_time_h", label = "Hours", choices = seq(0,24,1) , selected = trunc(ParList$M_time), width = "100%")),
                                                                       column(4,selectInput("M_time_m", label = "Minutes", choices = seq(0,59,1), selected = (ParList$M_time%%1*60), width = "100%"))
                                                                     ),
                                                                     radioButtons("M_dist", "Distribution type:",
                                                                                  c("Exponentional" = "exp",
                                                                                    "Gamma" = "gamma"), selected = ParList$M_dist, inline = TRUE),

                                                                     conditionalPanel(condition = "input.M_dist == 'gamma'",
                                                                                      sliderInput("m_wt_gamma_shape", "Shape Parameter", min = 0, max = 20, value = 3, step = 1)
                                                                     ),
                                                                     hr(),
                                                                     uiOutput('m_time_slider')
                                                              ), 
                                                              column(6,
                                                                     plotOutput("M_wt_plot_option")
                                                              )
                                                     ),
                                                     
                                                     tabPanel("S",
                                                              column(4,
                                                                     h5("Mean Time Elapsed: "),
                                                                     fluidRow(
                                                                       column(4,selectInput("S_time_h", label = "Hours", choices = seq(0,24,1) , selected = trunc(ParList$S_time), width = "100%")),
                                                                       column(4,selectInput("S_time_m", label = "Minutes", choices = seq(0,59,1), selected = (ParList$S_time%%1*60), width = "100%"))
                                                                     ),
                                                                     radioButtons("S_dist", "Distribution type:",
                                                                                  c("Exponentional" = "exp",
                                                                                    "Gamma" = "gamma"), selected = ParList$S_dist, inline = TRUE),
   
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
                                                     
                                                   )
                                          ),
                                          #############################################################################
                                          tabPanel("Survival",
                                                   tabsetPanel(
                                                     tabPanel("Flight Energetics",
                                                              column(4,
                                                                     sliderInput(inputId = "S_u_inv", label ="Number of Bouts",
                                                                                 value = round(1/ParList$S.u), min = 0, max = 20, step = 1),
                                                                     hr(),
                                                                     tags$h5("Survival Probability Function of Energy Reserves:"),
                                                                     sliderInput(inputId = "S_a", label ="Shape Param a of per-bout Probability of Survival",
                                                                                 value = ParList$S.a, min = 0, max = 100, step = 1),
                                                                     sliderInput(inputId = "S_b", label ="Shape Param b of per-bout Probability of Survival",
                                                                                 value = ParList$S.b, min = 0, max = 100, step = 1)
                                                              ),
                                                              column(6,
                                                                     plotOutput("flight_energetics_plot")
                                                              )
                                                     ),
                                                     tabPanel("Senescence",
                                                              column(4,
                                                                     checkboxInput(inputId = "SENESCE", label = "Mortality during Generic Flight", value = ParList$SENESCE),
                                                                     conditionalPanel(condition = "!input.SENESCE",
                                                                                      sliderInput(inputId = "sns_a", label ="Exp: a",
                                                                                                  value = ParList$sns.a, min = 0, max = 0.1, step = 0.001),
                                                                                      sliderInput(inputId = "sns_b", label ="Exp: b",
                                                                                                  value = ParList$sns.b, min = 0, max = 1000, step = 1)
                                                                     )),
                                                              column(6,
                                                                     plotOutput("senescence_plot")
                                                              )
                                                     ),
                                                     tabPanel("Damage",
                                                              column(4,
                                                                     checkboxInput(inputId = "TATTER", label = "During Generic Flight", value = ParList$TATTER),
                                                                     conditionalPanel(condition = "!input.TATTER",
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
                                                                                       value = 500, min = 0, max = 1000, step = 10))),
                                                                            column(6,
                                                                               plotOutput("tattering_beta_plot"),
                                                                               plotOutput("tattering_exp_plot")
                                                                    )
                                                              
                                                     )
                                                   )),
                                          
                                          #########################################################################
                                          tabPanel("Blood Meal",
                                                   column(8,
                                                          checkboxInput("showB_Option", "Setting Blood Meal Parameters", value = ("M"%in%ParList$stateSpace)),
                                                          conditionalPanel(condition = "input.showB_Option",
                                                                           #helpText("The following parameters also can be set under 'Bouts' Panel"),
                                                                           checkboxInput("showBloodMeal_Option", "Blood Meal Size", value = ("bm.a" %in% names(female_par))),
                                                                           conditionalPanel(condition = "input.showBloodMeal_Option",
                                                                                            fluidRow(
                                                                                              column(6,
                                                                                                     sliderInput(inputId = "bm_mean", label ="Average Bloodmeal Size",
                                                                                                                 value = ParList$bm.a/(ParList$bm.a + ParList$bm.b), min = 0, max = 1, step = 0.01),
                                                                                                     sliderInput(inputId = "bm_v", label ="Parameter v for a Bloodmeal Size: (a + b) in Beta(a,b)",
                                                                                                                 value = (ParList$bm.a + ParList$bm.b), min = 0, max = 40, step = 0.5)
                                                                                              ),
                                                                                              column(6,
                                                                                                     plotOutput("bm_Option_plot")
                                                                                              )
                                                                                            )),
                                                                           hr(),
                                                                           checkboxInput("overfeed_Option", "Overfeed", ParList$OVERFEED),
                                                                           conditionalPanel(condition = "input.overfeed_Option",
                                                                                            fluidRow(
                                                                                              column(6,
                                                                                                     sliderInput(inputId = "of_a", "Exp Param a for overfeeding as function of bmSize",
                                                                                                                 value = ParList$of.a, min = 5, max = 10, step = 0.01),
                                                                                                     sliderInput(inputId = "of_b", "Exp Param b for overfeeding as function of bmSize",
                                                                                                                 value = ParList$of.b, min = 0, max = 10000, step = 100)),
                                                                                              column(6,
                                                                                                     plotOutput("overfeeding_Option_plot")
                                                                                              ))
                              
                                                                           )))),
                                          

                                          tabPanel("Sugar Feeding",
                                                   column(6,
                                                          sliderInput(inputId = "S_sa", label ="Shape Param a of Probability to queue Sugar bout",
                                                                      value = ParList$S.sa, min = 0, max = 100, step = 1),
                                                          sliderInput(inputId = "S_sb", label ="Shape Param b of Probability to queue Sugar bout",
                                                                      value = ParList$S.sa, min = 0, max = 100, step = 1),
                                                          sliderInput(inputId = "energyPreG", label ="Pre-gonotrophic Energy Requirement",
                                                                      value = ParList$energyPreG, min = 0, max = 100, step = 1),
                                                          hr(),
                                                          checkboxInput("showS_Option", "Sugar Feeding Parameters:", value = ("S" %in% ParList$stateSpace)),
                                                          conditionalPanel(condition = "input.showS_Option",
                                                                        
                                                                           sliderInput(inputId = "S_succeed_Option", label ="Probability of Success",
                                                                                       value = ParList$S_succeed, min = 0.9, max = 1, step = 0.01),
                                                                           sliderInput(inputId = "S_surv_Option", label ="Baseline Probability of Survival",
                                                                                       value = ParList$S_surv, min = 0.9, max = 1, step = 0.01),
                                                                           #textInput("S_wts", "Landing Spot Weights: Enter a vector (comma delimited)", "1,1,1,1,1"),
                                                                           sliderInput(inputId = "preGsugar_Option", label ="Amount of Energy a Sugar Meal Contributes to Pre-gonotrophic Energy Requirement (%)",
                                                                                       value = ParList$preGsugar, min = 0, max = 100, step = 1))
                                                   )),
                                        
                                          
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
                             
                             )
  })

output$panel_bouts <- renderUI({
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
       ))
})
  ################## Waiting Time ###############################################
  set.seed(123)
  output$F_wt_plot_option <- renderPlot({
    if(input$F_dist == "exp"){
      f_t = as.numeric(input$F_time_h) + as.numeric(input$F_time_m)/60
      f_m = as.numeric(input$f_min_time)/60
      curve(dexp(x, rate = 1/(f_t - f_m), log = FALSE)/
              max(dexp(x, rate = 1/(f_t - f_m), log = FALSE)),
            xlab = "Exponentially-distributed bout lengths (in hours)", ylab = " Normalized Density", col = "Blue", lwd = 1.5, xlim = c(0,(f_m +24)))
      #}
    }else{
      f_t = as.numeric(input$F_time_h) + as.numeric(input$F_time_m)/60
      f_m = as.numeric(input$f_min_time)/60
      curve(dgamma(x, shape = input$f_wt_gamma_shape ,rate = 1/(f_t - f_m), log = FALSE)/
              max(dgamma(x, shape = input$f_wt_gamma_shape ,rate = 1/(f_t - f_m), log = FALSE)),
            xlab = "Gamma-distributed bout lengths", ylab = " Normalized Density", col = "Green", lwd = 1.5, xlim = c(0,(f_m +24)))
    }
  })
  
  output$B_wt_plot_option <- renderPlot({
    if(input$B_dist == "exp"){
      b_t = as.numeric(input$B_time_h) + as.numeric(input$B_time_m)/60
      b_m = as.numeric(input$b_min_time)/60
      curve(dexp(x, rate = 1/(b_t - b_m), log = FALSE)/
              max(dexp(x, rate = 1/(b_t - b_m), log = FALSE)),
            xlab = "Exponentially-distributed bout lengths (in hours)", ylab = " Normalized Density", col = "Blue", lwd = 1.5, xlim = c(0,(b_m +24)))
      #}
    }else{
      b_t = as.numeric(input$B_time_h) + as.numeric(input$B_time_m)/60
      b_m = as.numeric(input$b_min_time)/60
      curve(dgamma(x, shape = input$b_wt_gamma_shape ,rate = 1/(b_t - b_m), log = FALSE)/
              max(dgamma(x, shape = input$b_wt_gamma_shape ,rate = 1/(b_t - b_m), log = FALSE)),
            xlab = "Gamma-distributed bout lengths", ylab = " Normalized Density", col = "Green", lwd = 1.5, xlim = c(0,(b_m +24)))
    }
  })
  
  output$R_wt_plot_option <- renderPlot({
    if(input$R_dist == "exp"){
      r_t = as.numeric(input$R_time_h) + as.numeric(input$R_time_m)/60
      r_m = as.numeric(input$r_min_time)/60
      curve(dexp(x, rate = 1/(r_t - r_m), log = FALSE)/
              max(dexp(x, rate = 1/(r_t - r_m), log = FALSE)),
            xlab = "Exponentially-distributed bout lengths (in hours)", ylab = " Normalized Density", col = "Blue", lwd = 1.5, xlim = c(0,(r_m +24)))
      #}
    }else{
      r_t = as.numeric(input$R_time_h) + as.numeric(input$R_time_m)/60
      r_m = as.numeric(input$r_min_time)/60
      curve(dgamma(x, shape = input$r_wt_gamma_shape ,rate = 1/(r_t - r_m), log = FALSE)/
              max(dgamma(x, shape = input$r_wt_gamma_shape ,rate = 1/(r_t - r_m), log = FALSE)),
            xlab = "Gamma-distributed bout lengths", ylab = " Normalized Density", col = "Green", lwd = 1.5, xlim = c(0,(r_m +24)))
    }
  })
  
  output$L_wt_plot_option <- renderPlot({
    if(input$L_dist == "exp"){
      l_t = as.numeric(input$L_time_h) + as.numeric(input$L_time_m)/60
      l_m = as.numeric(input$l_min_time)/60
      curve(dexp(x, rate = 1/(l_t - l_m), log = FALSE)/
              max(dexp(x, rate = 1/(l_t - l_m), log = FALSE)),
            xlab = "Exponentially-distributed bout lengths (in hours)", ylab = " Normalized Density", col = "Blue", lwd = 1.5, xlim = c(0,(l_m +24)))
      #}
    }else{
      l_t = as.numeric(input$L_time_h) + as.numeric(input$L_time_m)/60
      l_m = as.numeric(input$l_min_time)/60
      curve(dgamma(x, shape = input$l_wt_gamma_shape ,rate = 1/(l_t - l_m), log = FALSE)/
              max(dgamma(x, shape = input$l_wt_gamma_shape ,rate = 1/(l_t - l_m), log = FALSE)),
            xlab = "Gamma-distributed bout lengths", ylab = " Normalized Density", col = "Green", lwd = 1.5, xlim = c(0,(l_m +24)))
    }
  })
  
  output$O_wt_plot_option <- renderPlot({
    if(input$O_dist == "exp"){
      o_t = as.numeric(input$O_time_h) + as.numeric(input$O_time_m)/60
      o_m = as.numeric(input$o_min_time)/60
      curve(dexp(x, rate = 1/(o_t - o_m), log = FALSE)/
              max(dexp(x, rate = 1/(o_t - o_m), log = FALSE)),
            xlab = "Exponentially-distributed bout lengths (in hours)", ylab = " Normalized Density", col = "Blue", lwd = 1.5, xlim = c(0,(o_m +24)))
      #}
    }else{
      o_t = as.numeric(input$O_time_h) + as.numeric(input$O_time_m)/60
      o_m = as.numeric(input$o_min_time)/60
      curve(dgamma(x, shape = input$o_wt_gamma_shape ,rate = 1/(o_t - o_m), log = FALSE)/
              max(dgamma(x, shape = input$o_wt_gamma_shape ,rate = 1/(o_t - o_m), log = FALSE)),
            xlab = "Gamma-distributed bout lengths", ylab = " Normalized Density", col = "Green", lwd = 1.5, xlim = c(0,(o_m +24)))
    }
  })

  output$M_wt_plot_option <- renderPlot({
    if(input$M_dist == "exp"){
      m_t = as.numeric(input$M_time_h) + as.numeric(input$M_time_m)/60
      m_m = as.numeric(input$m_min_time)/60
      curve(dexp(x, rate = 1/(m_t - m_m), log = FALSE)/
              max(dexp(x, rate = 1/(m_t - m_m), log = FALSE)),
            xlab = "Exponentially-distributed bout lengths (in hours)", ylab = " Normalized Density", col = "Blue", lwd = 1.5, xlim = c(0,(m_m +24)))
      #}
    }else{
      m_t = as.numeric(input$M_time_h) + as.numeric(input$M_time_m)/60
      m_m = as.numeric(input$m_min_time)/60
      curve(dgamma(x, shape = input$m_wt_gamma_shape ,rate = 1/(m_t - m_m), log = FALSE)/
              max(dgamma(x, shape = input$m_wt_gamma_shape ,rate = 1/(m_t - m_m), log = FALSE)),
            xlab = "Gamma-distributed bout lengths", ylab = " Normalized Density", col = "Green", lwd = 1.5, xlim = c(0,(m_m +24)))
    }
  })
  
  output$S_wt_plot_option <- renderPlot({
    if(input$S_dist == "exp"){
      s_t = as.numeric(input$S_time_h) + as.numeric(input$S_time_m)/60
      s_m = as.numeric(input$s_min_time)/60
      curve(dexp(x, rate = 1/(s_t - s_m), log = FALSE)/
              max(dexp(x, rate = 1/(s_t - s_m), log = FALSE)),
            xlab = "Exponentially-distributed bout lengths (in hours)", ylab = " Normalized Density", col = "Blue", lwd = 1.5, xlim = c(0,(s_m +24)))
      #}
    }else{
      s_t = as.numeric(input$S_time_h) + as.numeric(input$S_time_m)/60
      s_m = as.numeric(input$s_min_time)/60
      curve(dgamma(x, shape = input$s_wt_gamma_shape ,rate = 1/(s_t - s_m), log = FALSE)/
              max(dgamma(x, shape = input$s_wt_gamma_shape ,rate = 1/(s_t - s_m), log = FALSE)),
            xlab = "Gamma-distributed bout lengths", ylab = " Normalized Density", col = "Green", lwd = 1.5, xlim = c(0,(s_m +24)))
    }
  })
  
  
  ################# Survival ####################################################
  output$f_time_slider <- renderUI({
    sliderInput("f_min_time", "Minimal resting time in Minutes", min = 0, 
                max = (as.numeric(input$F_time_h) * 60 + as.numeric(input$F_time_m)), 
                value = round((as.numeric(input$F_time_h) * 60 + as.numeric(input$F_time_m)))/2, step = 1)
  })

  output$b_time_slider <- renderUI({
    sliderInput("b_min_time", "Minimal resting time in Minutes", min = 0, 
                max = (as.numeric(input$B_time_h) * 60 + as.numeric(input$B_time_m)), 
                value = round((as.numeric(input$B_time_h) * 60 + as.numeric(input$B_time_m)))/2, step = 1)
  })
  output$r_time_slider <- renderUI({
    sliderInput("r_min_time", "Minimal resting time in Minutes", min = 0, 
                max = (as.numeric(input$R_time_h) * 60 + as.numeric(input$R_time_m)), 
                value = round((as.numeric(input$R_time_h) * 60 + as.numeric(input$R_time_m)))/2, step = 1)
  })
  output$l_time_slider <- renderUI({
    sliderInput("l_min_time", "Minimal resting time in Minutes", min = 0, 
                max = (as.numeric(input$L_time_h) * 60 + as.numeric(input$L_time_m)), 
                value = round((as.numeric(input$L_time_h) * 60 + as.numeric(input$L_time_m)))/2, step = 1)
  })
  output$o_time_slider <- renderUI({
    sliderInput("o_min_time", "Minimal resting time in Minutes", min = 0, 
                max = (as.numeric(input$O_time_h) * 60 + as.numeric(input$O_time_m)), 
                value = round((as.numeric(input$O_time_h) * 60 + as.numeric(input$O_time_m)))/2, step = 1)
  })
  output$m_time_slider <- renderUI({
    sliderInput("m_min_time", "Minimal resting time in Minutes", min = 0, 
                max = (as.numeric(input$M_time_h) * 60 + as.numeric(input$M_time_m)), 
                value = round((as.numeric(input$M_time_h) * 60 + as.numeric(input$M_time_m)))/2, step = 1)
  })
  output$s_time_slider <- renderUI({
    sliderInput("s_min_time", "Minimal resting time in Minutes", min = 0, 
                max = (as.numeric(input$S_time_h) * 60 + as.numeric(input$S_time_m)), 
                value = round((as.numeric(input$S_time_h) * 60 + as.numeric(input$S_time_m)))/2, step = 1)
  })
  
  output$flight_energetics_plot <- renderPlot({
    curve(exp(input$S_a * x)/(exp(input$S_a * x) + input$S_b), ylab = "Survival Probability", xlab = "Energy Reserves",
          col = "Blue", lwd = 1.5)
  })
  
  output$senescence_plot <- renderPlot({

    if(!input$SENESCE){
      curve((2 + input$sns_b)/(1 + input$sns_b) - exp(x * input$sns_a)/(input$sns_b + x * input$sns_a), 
            ylim = c(0,1), xlim = c(0,50), col = "Blue",
            xlab = "Chronological Age (days)", ylab = "Probability of Survival, per bout")
    }else{
      curve((0 * x + 1), from = 0, to = 50, ylim = c(0,1), col = "Green",
            xlab = "Chronological Age (days)", ylab = "Probability of Survival, per bout")
    }
  })
  
  
  output$tattering_exp_plot <- renderPlot({
    if(!input$TATTER){
    curve((2 + input$ttr_b)/(1 + input$ttr_b) - exp(x * input$ttr_a)/(input$ttr_a + x * input$ttr_a),
          ylab = "Survival Probability", xlab = "Wing Tattering",
          main = "Exponentional Distribution", col = "Green", lwd = 1.5, ylim = c(0,1))}
  })
  
  output$tattering_beta_plot <- renderPlot({
    if(!input$TATTER){
    ttsz_a <- input$ttsz_mean * input$ttsz_v
    ttsz_b <- (1 - input$ttsz_mean) * input$ttsz_v
    curve(((x < input$ttsz_p)* 0 + (x >= input$ttsz_p)*dbeta(x, ttsz_a, ttsz_b))/
            max((x < input$ttsz_p)* 0 + (x >= input$ttsz_p)*dbeta(x, ttsz_a, ttsz_b)),
          ylab = "Normalized Density", xlab = "Wing Damage", main = "Beta Distribution", ylim = c(0,1), col = "Blue", lwd = 1.5)}
  })
  
  ################ Blood Meal ###################################################
  
  output$bm_Option_plot <- renderPlot({
    if(input$showBloodMeal_Option){
      bm_a <- input$bm_mean * input$bm_v
      bm_b <- (1 - input$bm_mean) * input$bm_v
      curve(dbeta(x, bm_a, bm_b)/max(dbeta(x, bm_a, bm_b)),ylab = "Normalized Density", xlab = "Blood Meal Size", col = "Blue", lwd = 1.5)}
  })
  
  output$overfeeding_Option_plot <- renderPlot({
    if(input$overfeed_Option){
      a <- input$of_a
      b <- input$of_b
      curve(exp(a * x)/(exp(a * x) + b),
            ylab = "Mortality", xlab = "Blood Meal Size", ylim = c(0,1), xlim = c(0,1), col = "Green", lwd = 1.5)}
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
  dataL <- reactive({
    req(input$filel)
    inFileL <- input$filel
    dfL <- read.csv(inFileL$datapath, header = input$headerl, sep = input$sepl)
    return(dfL)
  })
  output$contentsL <- renderTable({
    dataL()
  })
  
  output$panel_landscape_out_site <- renderPlot({
    if(input$showPoints){
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
        cbind(x,y) 
      }
      
      if(input$landscape_f_input == 'cluster'){
        xy_f = getPoints(21,nCenters=5,rng=10,nPaC=12,nPaCvr=2,spr=1)
        w_f = rgamma(length(xy_f[,1]), 1,1)
        xy_f = cbind(xy_f, w = rgamma(length(xy_f[,1]), 1, 1))
      }else if(input$landscape_f_input == 'imp_xyw'){
        xy_f = dataF()[,1:3]
      }else{
        xy_f = dataF()[,1:2]
        xy_f = cbind(xy_f, w = rgamma(length(xy_f[,1]), 1, 1))
      }
      N_f = length(xy_f[,1])
      
      if(input$landscape_l_input == 'cluster'){
        xy_l = getPoints(21,nCenters=25,rng=10,nPaC=8,nPaCvr=2,spr=.4)
        xy_l = cbind(xy_l, w = rgamma(length(xy_l[,1]), 1, 1))
      }else if(input$landscape_l_input == 'imp_xyw'){
        xy_l = dataL()[,1:3]
      }else{
        xy_l = dataL()[,1:2]
        xy_l= cbind(xy_l, w = rgamma(length(xy_l[,1]), 1, 1))
      }
      N_l = length(xy_l[,1])
      m_x = runif(10, -10, 10)/2
      m_y = runif(10, -10, 10)/2
      m_xy = cbind(x=m_x, y=m_y)
      xx = unique(c(xy_f[,1], xy_l[,1]))
      yy = unique(c(xy_f[,2], xy_l[,2])) 
      lx = length(xx)
      ix = sample(1:lx, 80)
      s_x = c(xx[ix], runif(40, -10, 10)/2)
      s_y = c(yy[ix], runif(40, -10, 10)/2)
      s_xy = cbind(x=s_x, y=s_y)
      
      xy_f= cbind(xy_f, w = rgamma(N_f, 1, 1))
      xy_l= cbind(xy_l, w = rgamma(N_l, 1, 1))
      m_xy = cbind(m_xy, w=rgamma(10,1,1))
      s_xy = cbind(s_xy, w=rgamma(120,1,1))
      
      plot(xy_f[,1], xy_f[,2], type = "n", pch = 3, col = "red", xlab = "East-West", ylab = "North-South")
      
      
      if(input$landscape_point_m){
        points(m_xy, pch=15, col = "orange", cex = m_xy[,3])}
      if(input$landscape_point_f){
        points(xy_f, pch = 21, bg = "red", cex = xy_f[,3])}
      if(input$landscape_point_l){
        points(xy_l, pch = 4, col = "blue", cex = xy_l[,3])}
      if(input$landscape_point_s){
        points(s_xy, pch=6, col=grey(0.5), cex=s_xy[,3])}
    }
  })
  
  
  observe({
    toggle(condition = input$showPoints, selector = "#landscape_output li a[data-value=landscape_site]")
  })

  #####################Simualtion output ########################################################
  output$sim_panel <- renderUI({
    if(input$project == 'demo'){
      sidebarLayout(position = "right",
                    sidebarPanel(style = "overflow-y:scroll; max-height: 600px",
                                 h4("Interested in more parameters in Bouts, Options and Ecology?"),
                                 actionButton("JumpToMore", label = "Check it now!")
                    ),
                    mainPanel(

                      plotOutput("demo_sim_histogram"),
                      plotOutput("demo_sim_chord")
                    ))
    }else{
      sidebarLayout(position = "right",
                    sidebarPanel(style = "overflow-y:scroll; max-height: 600px",
                                 h4("not for demo")
                    ),
                    mainPanel(
                      numericInput("N_female", "Number of Female Mosquitoes", value = 50, min = 0, max = NA, step = 1),
                      numericInput("N_male", "Number of Male Mosquitoes", value = 50, min = 0, max = NA, step = 1)
                    ))
    }
  })
  
  
  ###############################################################################
  # Histograms
  ###############################################################################
  
  # these functions take parameter 'data' which is raw JSON read back into R
  
  bionomics_lifespan <- function(data){
    lifespans = vapply(X = data,FUN = function(x){
      x$bionomics_lifespan[[1]]
    },FUN.VALUE = numeric(1),USE.NAMES = FALSE)
    return(lifespans)
  }
  
  bionomics_BMinterval <- function(data){
    BMintervals = vapply(X = data,FUN = function(x){
      if(x$bionomics_bmInt[[1]]>0){
        return(x$bionomics_bmInt[[1]])  
      } else {
        return(NaN) 
      }
    },FUN.VALUE = numeric(1),USE.NAMES = FALSE)
    BMintervals = Filter(Negate(is.nan),BMintervals)
    return(BMintervals)
  }
  
  bionomics_HumanBMinterval <- function(data){
    HumanBMintervals = vapply(X = data,FUN = function(x){
      if(x$bionomics_bmIntH[[1]]>0){
        return(x$bionomics_bmIntH[[1]])
      } else {
        return(NaN)
      }
    },FUN.VALUE = numeric(1),USE.NAMES = FALSE)
    HumanBMintervals = Filter(Negate(is.nan),HumanBMintervals)
    return(HumanBMintervals)
  }
  
  bionomics_HumanBM <- function(data){
    HumanBM = vapply(X = data,FUN = function(x){
      x$feedHumanH[[1]]
    },FUN.VALUE = numeric(1),USE.NAMES = FALSE)
    return(HumanBM)
  }
  
  bionomics_vc <- function(data, eip=10){
    vc = vapply(X = data,FUN = function(x,eip){
      feedT = unlist(x$feedAllT)
      if(length(feedT)<2 | is.null(feedT)){
        return(NaN)
      } else {
        if((diff(feedT)>eip)[1]){
          # sum all pairs of bites that are more than EIP days apart
          sum(apply(X = combn(feedT,2),MARGIN = 2,FUN = function(x){
            diff(x)>eip
          }))
        } else {
          return(NaN)
        }
      }
    },FUN.VALUE = numeric(1),eip = eip,USE.NAMES = FALSE)
    vc = Filter(Negate(is.nan),vc)
    return(vc)
  }
  
  # HMSC plotly histogram
  histogramPlotLyGenericBionomics=function(data,title,color){
    p=plot_ly(x=data,name=title,marker=list(color=color),type="histogram") %>%
      layout(
        barmode="overlay",
        legend=list(x=1.05,y=.5,bgcolor="#FFFFFF"),
        title=paste(title, "( Mean: ",signif(mean(data),3),")"),
        bargap=0.1
      )
    p
  }
  
  
  ###############################################################################
  # Chord diagram
  ###############################################################################
  
  
  # oneHistory: a single mosquito's JSON outfile
  transitionsInMosquitoStates <- function(oneHistory, stateSpace = c("D","M","F","B","R","L","O","S","E")){
    states = oneHistory$stateH
    createSequenceMatrix(stringchar = unlist(states[-1]),possibleStates = stateSpace)
  }
  
  transitionsInMosquitoPopulation <- function(popHistory, stateSpace = c("D","M","F","B","R","L","O","S","E")){
    transMatrices = lapply(X = popHistory,FUN = transitionsInMosquitoStates)
    transitions = Reduce(f = "+",x = transMatrices)
    transitions[stateSpace,stateSpace]
  }
  
  circlizeStatesTransitionMatrix <- function(history, stateSpace = c("D","M","F","B","R","L","O","S","E")){
    transitions=transitionsInMosquitoPopulation(history,stateSpace=stateSpace)
    colors=c("#555555","#95E455","pink","red","purple","cyan","blue","yellow","grey")
    chordDiagramFromMatrix(transitions,directional=1,grid.col=colors,direction.type="arrows",self.link=2)
  }
  
  
  ################################################################################################################
  # 
  # make plots
  # 
  ################################################################################################################
  
  
  # if you do not have chorddiag, use: devtools::install_github("mattflor/chorddiag")
  
  mosquito_dir = "demo_json/" # wherever the json files are
  
  # files from each simulation
  files_l = system(command = paste0("ls ",mosquito_dir),intern = TRUE)
  hist_l = files_l[grep(pattern = "History",x = files_l)] # individual json histories
  pop_l = files_l[grep(pattern = "Pop",x = files_l)] # population csv
  
  # output
  mHist = fromJSON(txt = paste0(mosquito_dir,hist_l),flatten = FALSE,simplifyVector=FALSE)
  mPop = read.table(file = paste0(mosquito_dir,pop_l),header = TRUE,sep = ",")
  
  nullIx = which(vapply(X = mHist,FUN = function(x){x$ID[[1]]},FUN.VALUE = character(1)) == "NULL")
  mHist = mHist[-nullIx]
  
  axisSize = 12
  titleSize = 14.5
  
  lifespans = bionomics_lifespan(mHist)
  lifespans_plot = ggplot(data = data.frame(lifespan=lifespans)) +
    geom_histogram(aes(lifespan),fill=rgb(0,.5,.5,.5)) +
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          axis.title=element_text(size=axisSize),
          plot.title = element_text(size=titleSize)) +
    guides(fill = FALSE) + 
    labs(x="Days",y="Frequency",title="Mosquito Lifespans")
  
  BMintervals = bionomics_BMinterval(mHist)
  BMintervals_plot = ggplot(data = data.frame(BMinterval=BMintervals)) +
    geom_histogram(aes(BMinterval), fill = rgb(0,.5,0,.5)) +
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          axis.title=element_text(size=axisSize),
          plot.title = element_text(size=titleSize)) +
    guides(fill = FALSE) + 
    labs(x="Days",y="Frequency",title="Bloodmeal Interval")
  
  vectorialCapacity = bionomics_vc(mHist,eip = 8)
  vectorialCapacity_plot = ggplot(data = data.frame(vectorialCapacity=vectorialCapacity)) +
    geom_histogram(aes(vectorialCapacity), fill = rgb(0,.5,0,.5),stat = "count") +
    scale_x_continuous(breaks=0:(max(vectorialCapacity)+2)) + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          axis.title=element_text(size=axisSize),
          plot.title = element_text(size=titleSize)) +
    guides(fill = FALSE) + 
    labs(x="Vectorial Capacity",y="Frequency",title="Individual Vectorial Capacity")
  
  HumanBMs = bionomics_HumanBM(mHist)
  HumanBMs_plot = ggplot(data = data.frame(HumanBM=HumanBMs)) +
    geom_histogram(aes(HumanBM), fill = rgb(1,0,0,0.5),stat = "count") +
    scale_x_continuous(breaks=0:(max(HumanBMs)+2)) + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          axis.title=element_text(size=axisSize),
          plot.title = element_text(size=titleSize)) +
    guides(fill = FALSE) + 
    labs(x="Count",y="Frequency",title="Human Bloodmeals")
  
  
  output$demo_sim_histogram <- renderPlot({
    grid.arrange(BMintervals_plot,HumanBMs_plot,lifespans_plot,vectorialCapacity_plot,nrow=2)
  })
  
  output$demo_sim_chord <- renderPlot({
    circlizeStatesTransitionMatrix(history = mHist)
  })
  
  ######################################################################################################################
  
  
  observe({
    if (input$project == 'demo' && input$createDemoFolder > 0) {
      updateTabsetPanel(session, "nav", selected = "landscape")
    }
  })
  
  
  
  observe({
    if (input$project == 'exist' && input$launchgo > 0) {
      session$sendCustomMessage('activeNavs', 'Bouts')
      session$sendCustomMessage('activeNavs', 'Landscape')
      session$sendCustomMessage('activeNavs', 'Options')
      session$sendCustomMessage('activeNavs', 'Simulation')
      session$sendCustomMessage('activeNavs', 'Bouts')
      session$sendCustomMessage('activeNavs', 'Ecology')
      session$sendCustomMessage('activeNavs', 'Pathogen')
    }
  })
  observe({
    if (input$project == 'exist' && input$launchgo > 0) {
      updateTabsetPanel(session, "nav", selected = "landscape")
    }
  })
  
  observe({
    if (input$project == 'demo' && input$createDemoFolder > 0) {
      session$sendCustomMessage('activeNavs', 'Landscape')
    }
  })
  observe({
    if (input$project == 'new' && input$createNewFolder > 0) {
      session$sendCustomMessage('activeNavs', 'Landscape')
      session$sendCustomMessage('activeNavs', 'Options')
      session$sendCustomMessage('activeNavs', 'Simulation')
      session$sendCustomMessage('activeNavs', 'Bouts')
      session$sendCustomMessage('activeNavs', 'Ecology')
      session$sendCustomMessage('activeNavs', 'Pathogen')
    }
  })
  
  observeEvent(input$JumpToSim,{
    session$sendCustomMessage('activeNavs', 'Simulation')
    updateTabsetPanel(session, "nav", selected = "simulation")
  })
  
  observeEvent(input$JumpToMore,{
    session$sendCustomMessage('activeNavs', 'Options')
    session$sendCustomMessage('activeNavs', 'Bouts')
    session$sendCustomMessage('activeNavs', 'Ecology')
    session$sendCustomMessage('activeNavs', 'Pathogen')
    updateTabsetPanel(session, "nav", selected = "options")
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
      updateTabsetPanel(session, "nav", selected = "landscape")
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
  
  observeEvent(input$save_demo_land, {
    js_string_6 <- 'alert("Selected Demo Sites have been saved in the folder: demo!");'
    session$sendCustomMessage(type='jsCode', list(value = js_string_6))
  })
  
  
  
  
  
  
  #######################################################################
  output$panel_f <- renderUI({
    if (input$showF)
      column(6,
             wellPanel(
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

               wellPanel(

                 sliderInput(inputId = "B_succeed", label ="Probability of Success",
                             value = 0.95, min = 0.8, max = 1, step = 0.01),                #
                 sliderInput(inputId = "B_surv", label ="Baseline Probability of Survival",
                             value = 0.99, min = 0.9, max = 1, step = 0.01),
 
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
                 hr(),
                 checkboxInput("showBloodMeal", "Blood Meal Size", FALSE),
                 conditionalPanel(condition = "input.showBloodMeal",
                                  helpText("Please set the parameters in Options - Blood Meal")

                 ),
                 hr(),
                 checkboxInput("overfeed", "Overfeed", FALSE),
                 conditionalPanel(condition = "input.overfeed",
                                  helpText("Please set the parameters in Options - Blood Meal")

                 ),
                 hr(),
                 sliderInput(inputId = "preGblood", label ="Amount of Energy a Blood Meal Contributes to Pre-gonotrophic Energy Requirement (%)",
                             value = 0, min = 0, max = 100, step = 1),
                 sliderInput(inputId = "Q", label ="Human Blood Index",
                             value = 0.9, min = 0, max = 1, step = 0.1)
               ))

      )
  })
  output$panel_r <- renderUI({
    if (input$showR)
      column(6,
             conditionalPanel(condition = "input.showR",

                              wellPanel(

                                sliderInput(inputId = "R_surv", label ="Baseline Probability of Survival",
                                            value = 0.99, min = 0.9, max = 1, step = 0.01),

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

             wellPanel(

               sliderInput(inputId = "L_succeed", label ="Probability of Success",
                           value = 0.98, min = 0.8, max = 1, step = 0.01),
               sliderInput(inputId = "L_surv", label ="Baseline Probability of Survival",
                           value = 0.99, min = 0.9, max = 1, step = 0.01)#,

             ))
  })
  output$panel_o <- renderUI({
    if (input$showO)
      column(6,

             wellPanel(

               sliderInput(inputId = "O_succeed", label ="Probability of Success",
                           value = 0.99, min = 0.9, max = 1, step = 0.01),
               sliderInput(inputId = "O_surv", label ="Baseline Probability of Survival",
                           value = 0.99, min = 0.9, max = 1, step = 0.01)#,

             ))
  })
  output$panel_m <- renderUI({
    if (input$showM)
      column(6,

             wellPanel(

               sliderInput(inputId = "M_succeed", label ="Probability of Success",
                           value = 0.95, min = 0.9, max = 1, step = 0.01),
               sliderInput(inputId = "M_surv", label ="Baseline Probability of Survival",
                           value = 0.99, min = 0.9, max = 1, step = 0.01)#,

             ))
  })
  output$panel_s <- renderUI({
    if (input$showS)
      column(6,
             wellPanel(
               helpText("Please set the parameters in Options - Sugar Feeding")
             ))
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
  
  output$panel_landscape <- renderUI({
    if(input$project == 'demo'){
      fluidPage(
        sidebarLayout(position = "right", 
                      sidebarPanel(style = "overflow-y:scroll; max-height: 600px",
                                   h4("Please add sites"),
                                   checkboxInput("demo_f", "Haunts: Blood Feeding Sites", TRUE),
                                   checkboxInput("demo_l", "Habitats: Egg Laying Sites", TRUE),
                                   checkboxInput("demo_s", "Sugar Feeding Sites", TRUE),
                                   checkboxInput("demo_m", "Mating Sites", TRUE),
                                   hr(),
                                   actionButton("save_demo_land", "Save Selected Demo Sites"),
                                   hr(),
                                   actionButton("JumpToSim", "Next Step: Simulation Initialization")
                      ),
                      mainPanel(
                        plotOutput("demo_landscape")
                      )
        ))
    }else{
      fluidPage(
        useShinyjs(),
        sidebarLayout(position = "right",
                      sidebarPanel(style = "overflow-y:scroll; max-height: 600px",
                                   helpText("Please set the parameters"),
                                   checkboxInput("showPoints", "Sites", FALSE),
                                   conditionalPanel(condition = "input.showPoints",
                                                    wellPanel(
                                                      helpText("space(x,y), a search weight (w)"),
                                                      checkboxInput("landscape_point_f", "{f}: Haunts, blood feeding sites", TRUE),
                                                      conditionalPanel(condition = "input.landscape_point_f",
                                                                       wellPanel(
                                                                         radioButtons(inputId = "landscape_f_input", "Provide the locations w/o weights:",
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
                                                                         )
                                                                         #,
                                                                         #uiOutput("landscape_f_file")
                                                                       )),
                                                      checkboxInput("landscape_point_l", "{l}: Habitats, egg laying sites", TRUE),
                                                      conditionalPanel(condition = "input.landscape_point_l",
                                                                       wellPanel(
                                                                         radioButtons(inputId = "landscape_l_input", "Provide the locations w/o weights:",
                                                                                      choices = c("Clusters" = "cluster",
                                                                                                  "Import x, y, w" = "imp_xyw",
                                                                                                  "Import x, y" = "imp_xy"
                                                                                      ),
                                                                                      selected = "cluster"),
                                                                         conditionalPanel(condition = "input.landscape_l_input != 'cluster'",
                                                                                          fileInput('filel', 'Choose CSV File',
                                                                                                    accept=c('text/csv',
                                                                                                             'text/comma-separated-values,text/plain',
                                                                                                             '.csv')),
                                                                                          wellPanel(checkboxInput('headerl', 'Header', TRUE),
                                                                                                    radioButtons('sepl', 'Separator',
                                                                                                                 c(Comma=',',
                                                                                                                   Semicolon=';',
                                                                                                                   Tab='\t'),
                                                                                                                 ','))
                                                                         )
                                                                         #uiOutput("landscape_l_file")
                                                                       )),
                                                      checkboxInput("landscape_point_s", "{s}: Sugar Feeding Sites", TRUE),
                                                      
                                                      checkboxInput("landscape_point_m", "{m}: Mating Sites", TRUE))
                                                    
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
                                   )),
                      mainPanel(
                        tabsetPanel(
                          id = "landscape_output",
                          tabPanel(
                            title = "Sites",
                            value = "landscape_site",
                            plotOutput("panel_landscape_out_site")

                          )
                        )
                      )
        ))
    }
  })
  
  output$demo_landscape <- renderPlot({
    f_xy = read.csv('demo_data/peridom.f116.xyw', header=T)/2
    plot(f_xy[,1], f_xy[,2], type = "n", pch = 3, col = "red", xlab = "East-West", ylab = "North-South")
    l_xy = read.csv('demo_data/peridom.l117.xyw', header=T)/2
    
    set.seed(21)
    m_x = runif(10, -10, 10)/2
    m_y = runif(10, -10, 10)/2
    m_xy = cbind(x=m_x, y=m_y)
    
    xx = unique(c(f_xy[,1], l_xy[,1]))
    yy = unique(c(f_xy[,2], l_xy[,2])) 
    
    lx = length(xx)
    ix = sample(1:lx, 80)
    s_x = c(xx[ix], runif(40, -10, 10)/2)
    s_y = c(yy[ix], runif(40, -10, 10)/2)
    s_xy = cbind(x=s_x, y=s_y)
    
    f_xy[,3] = rgamma(250, 1, 1)
    l_xy[,3] = rgamma(250,1,1)
    m_xy = cbind(m_xy, w=rgamma(10,1,1))
    s_xy = cbind(s_xy, w=rgamma(120,1,1))
    
    if(input$demo_m){
      points(m_xy, pch=15, col = "orange", cex = m_xy[,3])}
    if(input$demo_f){
      points(f_xy, pch = 21, bg = "red", cex = f_xy[,3])}
    if(input$demo_l){
      points(l_xy, pch = 4, col = "blue", cex = l_xy[,3])}
    if(input$demo_s){
      points(s_xy, pch=6, col=grey(0.5), cex=s_xy[,3])}
    
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
  
  observeEvent(input$showF, {
    if(input$showF){
      updateTabsetPanel(session, "boutbar",selected = "bout_f")
    }
  })
  observeEvent(input$showB, {
    if(input$showB){
      updateTabsetPanel(session, "boutbar",selected = "bout_b")
    }
  })
  observeEvent(input$showR, {
    if(input$showR){
      updateTabsetPanel(session, "boutbar",selected = "bout_r")
    }
  })
  observeEvent(input$showL, {
    if(input$showL){
      updateTabsetPanel(session, "boutbar",selected = "bout_l")
    }
  })
  observeEvent(input$showO, {
    if(input$showO){
      updateTabsetPanel(session, "boutbar",selected = "bout_o")
    }
  })
  observeEvent(input$showS, {
    if(input$showS){
      updateTabsetPanel(session, "boutbar",selected = "bout_s")
    }
  })
  observeEvent(input$showM, {
    if(input$showM){
      updateTabsetPanel(session, "boutbar",selected = "bout_m")
    }
  })
  observeEvent(input$showE, {
    if(input$showE){
      updateTabsetPanel(session, "boutbar",selected = "bout_e")
    }
  })
  observeEvent(input$showMale, {
    if(input$showMale){
      updateTabsetPanel(session, "boutbar",selected = "bout_male")
    }
  })
  
  
  ##################Sync inputs for multiple pages#########################################
  
  
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
  
  observeEvent(input$createDemoFolder, {
    toggle(selector = "#nav li a[data-value=start]")
  })
  #   observeEvent(input$createNewFolder, {
  #   toggle(selector = "#nav li a[data-value=start]")
  # })
  observeEvent(input$launchgo, {
    toggle(selector = "#nav li a[data-value=start]")
  })
  
  observeEvent(input$save_demo_land, {
    do.call(file.remove, list(list.files("demo/", full.names = TRUE)))
    if(input$demo_f){
      write.csv(input$f_xy, "demo/demo_f.csv")
    }
    if(input$demo_s){
      write.csv(input$s_xy, "demo/demo_s.csv")
    }
    if(input$demo_l){
      write.csv(input$l_xy, "demo/demo_l.csv")
    }
    if(input$demo_m){
      write.csv(input$m_xy, "demo/demo_m.csv")
    }
  })
  
  observeEvent(input$save_inputs_bout, {
    # Define inputs to save
    param_name <- c('N_female', 'N_male', 'gammaShape', 'SENESCE', 'sns_a', 'sns_b', 'TATTER', 'ttsz_p', 'ttr_a',
                    'ttr_b', 'S_u', 'S_a', 'S_b', 'S_sa', 'S_sb', 'bs_m', 'bs_v', 'maxBatch',
                    'emt_m', 'emt_v','eggT', 'eggP', 'energyPreG', 'PfEIP'
    )
    f_param_name <- c('F_succeed', 'F_surv', 'F_wts')
    b_param_name <- c('B_succeed', 'B_surv', 'B_wts',
                      'surviveH', 'probeH', 'surviveprobeH', 'feedH',
                      'surviveZ', 'feedZ', 'bm_a', 'bm_b', 'OVERFEED', 'of_a', 'of_b', 'preGblood',
                      'Q')
    r_param_name <- c('R_surv', 'REFEED', 'rf_a', 'rf_b', 'R_wts')
    l_param_name <- c('L_succeed','L_surv', 'L_wts')
    o_param_name <- c('O_succeed', 'O_surv', 'O_wts')
    m_param_name <- c('M_succeed', 'M_surv', 'M_wts')
    s_param_name <- c('S_succeed', 'S_surv', 'preGsugar', 'S_wts')
    # Declare inputs
    inputs_bout <- NULL
    inputs_name <- NULL
    stateSpace <- ""
    
    #Append all inputs before saving to folder
    if(input$showF){
      stateSpace <- paste(stateSpace, "F", seq = "")
      for(input.i in f_param_name){
        if(length(input[[input.i]]) != 0){
          inputs_name <- append(inputs_name,input.i)
          inputs_bout <- append(inputs_bout, input[[input.i]])
        }}}
    
    if(input$showB){
      stateSpace <- paste(stateSpace, "B", seq = "")
      inputs_name <- append(inputs_name, 'bm_a')
      inputs_bout <- append(inputs_bout, input$bm_mean * input$bm_v)
      inputs_name <- append(inputs_name, 'bm_b')
      inputs_bout <- append(inputs_bout, (1 - input$bm_mean) * input$bm_v)
      for(input.i in b_param_name){
        if(length(input[[input.i]]) != 0){
          inputs_name <- append(inputs_name,input.i)
          inputs_bout <- append(inputs_bout, input[[input.i]])
        }}}
    
    if(input$showR){
      stateSpace <- paste(stateSpace, "R", seq = "")
      for(input.i in r_param_name){
        if(length(input[[input.i]]) != 0){
          inputs_name <- append(inputs_name,input.i)
          inputs_bout <- append(inputs_bout, input[[input.i]])
        }}}
    
    if(input$showL){
      stateSpace <- paste(stateSpace, "L", seq = "")
      for(input.i in l_param_name){
        if(length(input[[input.i]]) != 0){
          inputs_name <- append(inputs_name,input.i)
          inputs_bout <- append(inputs_bout, input[[input.i]])
        }}}
    
    if(input$showO){
      stateSpace <- paste(stateSpace, "O", seq = "")
      for(input.i in o_param_name){
        if(length(input[[input.i]]) != 0){
          inputs_name <- append(inputs_name,input.i)
          inputs_bout <- append(inputs_bout, input[[input.i]])
        }}}
    
    if(input$showM){
      stateSpace <- paste(stateSpace, "M", seq = "")
      for(input.i in m_param_name){
        if(length(input[[input.i]]) != 0){
          inputs_name <- append(inputs_name,input.i)
          inputs_bout <- append(inputs_bout, input[[input.i]])
        }}}
    
    if(input$showS){
      stateSpace <- paste(stateSpace, "S", seq = "")
      inputs_name <- append(inputs_name, 'S_u')
      inputs_bout <- append(inputs_bout, 1/input$S_u_inv)
      for(input.i in m_param_name){
        if(length(input[[input.i]]) != 0){
          inputs_name <- append(inputs_name,input.i)
          inputs_bout <- append(inputs_bout, input[[input.i]])
        }}}
    
    for(input.i in param_name){
      if(length(input[[input.i]]) != 0){
        inputs_name <- append(inputs_name,input.i)
        inputs_bout <- append(inputs_bout, input[[input.i]])
      }}
    
    inputs_name <- append(inputs_name, 'ttsz_a')
    inputs_bout <- append(inputs_bout, input$ttsz_mean * input$ttsz_v)
    inputs_name <- append(inputs_name, 'ttsz_b')
    inputs_bout <- append(inputs_bout, (1 - input$ttsz_mean) * input$ttsz_v)
    
  
    inputs_name <- append(inputs_name, 'stateSpace')
    inputs_bout <- append(inputs_bout, stateSpace)

    # Inputs data.frame
    inputs_data_frame <- data.frame(inputId = inputs_name, value = inputs_bout)
    # Save Inputs
    jsonOut=prettify(toJSON(inputs_data_frame))
    write(jsonOut,paste0(DIR,"/Mosquito_par.json"))
  })
}