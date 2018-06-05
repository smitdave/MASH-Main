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

  #################### Loading parameter json file #####################################

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


  #################### Option Output #####################################################
  output$panel_initial <- renderUI({
                           fluidPage(
                             helpText("Welcome to MBITES! Please set parameters here:"),
                             navlistPanel(widths = c(2,10),
                                          
                                          #########################################################################
                                          tabPanel("Timing",
                                                   helpText("test Timing")
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


  
  
  

  #####################Simualtion output ########################################################
  output$sim_panel <- renderUI({
    if(input$project == 'demo'){
      sidebarLayout(position = "right",
                    sidebarPanel(style = "overflow-y:scroll; max-height: 600px",
                                 h4("Interested in more parameters in parameter tuning?"),
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




  

  
  ################################################################################################################
  # 
  # make plots
  # 
  ################################################################################################################
  
  
  observe({
    if (input$project == 'demo' && input$createDemoFolder > 0) {
      updateTabsetPanel(session, "nav", selected = "simulation")
    }
  })
  
  
  
  observe({
    if (input$project == 'exist' && input$launchgo > 0) {
      session$sendCustomMessage('activeNavs', 'Initialize')
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
      session$sendCustomMessage('activeNavs', 'Initialize')
      session$sendCustomMessage('activeNavs', 'Simulation')
      session$sendCustomMessage('activeNavs', 'Pathogen')
    }
  })
  
  observeEvent(input$JumpToSim,{
    session$sendCustomMessage('activeNavs', 'Simulation')
    updateTabsetPanel(session, "nav", selected = "simulation")
  })
  
  observeEvent(input$JumpToMore,{
    session$sendCustomMessage('activeNavs', 'Initialize')
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