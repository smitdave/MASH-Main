################################################################################################################
# MBITES-BRO GUI
# HMSC/Sean Wu
# NOTE: Set your directory to this source file's location!!!!!
# install.packages(c("shiny","shinythemes","shinyjs","shinydashboard","plotly","markdown","igraph","webshot","XML","circlize","markovchain"))
# devtools::install_github(repo = "smitdave/MASH",ref = "MASH-S3")
# devtools::install_github("mattflor/chorddiag")
################################################################################################################
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinydashboard)
library(plotly)
library(markdown)
library(igraph)
library(webshot)
library(compiler)
library(XML)
#library(chorddiag)
library(circlize)
library(markovchain)
library(MASH.MBPT)
library(parallel)
#plotLandscape(LANDSCAPE)
source("MBITES-BRO.R")
source("PLOTS_Cohort.R")
source("PLOTS_Bionomics.R")
PP<<-DHM.Basic.Pset()
dataIn<<-DHM.Basic.Cohort(PP)
LANDSCAPE<<-makeLandscape(nF=10,nA=8,nS=15,nM=6,aquaMod="el4p",pointGen=pointsPoisson,hhSize=10,hhMin=2,aquaSD=0.025)
source("PLOTS_LandscapePlotLy.R")
VAR_DESCR_COL_WIDTH=3
VAR_SLIDE_COL_WIDTH=4
################################################################################################################

################################################################################################################
shinyUI(fluidPage(theme = shinytheme("flatly"),
  # Application header ###################################################################################
  titlePanel(h1("Modular Analysis & Simulation for Health :: MBITES-BRO")),
  # Top-Level tabs #######################################################################################
  navbarPage("",id="nav",
    # Inputs tab #########################################################################################
    tabPanel("Inputs",
      sidebarLayout(
        sidebarPanel(
          includeMarkdown("instructions.md"),
          h1(""),
          actionButton(inputId="exportParameters", label="Export Parameters to CONFIG file",style='text-align:center; font-size:100%; width:100%; height:80%; position:center; margin:5 auto;display:block'),
          actionButton(inputId="importParameters", label="Import Parameters from CONFIG file",style='text-align:center; font-size:100%; width:100%; height:80%; position:center; margin:5 auto;display:block'),
          h1(),
          actionButton(inputId="runModel",label="Run Model", style='text-align:center; width:100%; height:100%; position:center; background-color:#428bca; margin:5 auto; display:block'),
          #includeMarkdown("libraries.md")
          h1(),
          img(src='boutFull.png',align="center",width="100%",style="border-radius: 25px")
        ),
        mainPanel(
          h2("Parameters Setup"),
          helpText("Adjust the landscape and mosquito biological parameters to setup the simulation."),
          h1(""),
          tabsetPanel(
            ########################################################
            tabPanel("Landscape",
              fluidRow(h1("")),
              fluidRow(
                actionButton(inputId="randomLandscape",label="Generate Random Landscape", style='text-align:center; width:100%; height:100%; position:center; margin:5 auto; display:block'),
                h1(""),
                column(4,
                  fluidRow(helpText("nF: Blood-feeding sites number.")),
                  fluidRow(sliderInput("nF",NULL,min=1,max=25,value=10,step=1,width="100%")),
                  fluidRow(helpText("nA: Aquatic habitats number.")),
                  fluidRow(sliderInput("nA",NULL,min=1,max=25,value=8,step=1,width="100%")),
                  fluidRow(helpText("nS: Sugar-feeding sites number.")),
                  fluidRow(sliderInput("nS",NULL,min=1,max=25,value=15,step=1,width="100%")),
                  fluidRow(helpText("nM: Mating sites number.")),
                  fluidRow(sliderInput("nM",NULL,min=1,max=25,value=6,step=1,width="100%")),
                  fluidRow(helpText("hhSize: Average number of hosts at feeding sites.")),
                  fluidRow(sliderInput("hhSize",NULL,min=1,max=25,value=6,step=1,width="100%")),
                  fluidRow(helpText("hhMin: Minimum number of hosts at feeding sites.")),
                  fluidRow(sliderInput("hhMin",NULL,min=1,max=25,value=6,step=1,width="100%")),
                  fluidRow(helpText("aquaMod: Aquatic model.")),
                  fluidRow(radioButtons("aquaMod",NULL,c("Emerge"="emerge","EL4P"="el4p"))),
                  fluidRow(helpText("pointGen: Point generation function.")),
                  fluidRow(radioButtons("pointGen",NULL,choiceNames=list("Poisson","Clustered","Lattice","Overdispersed"),choiceValues=list("poisson","clustered","lattice","overdispersed")))
                ),
                column(8,plotlyOutput("plotLandscape",height="750px"))
              )
            ),
            ########################################################
            tabPanel("Movement Kernels",
              fluidRow(h1("")),
              fluidRow(
                column(1,radioButtons("kernelSource",label="Source",choices = list("S"="1","M"="2","A"="3","F"="4"),selected="1")),
                column(1,radioButtons("kernelDestiny",label="Destiny",choices = list("S"="1","M"="2","A"="3","F"="4"),selected="2")),
                column(7,fluidRow(plotlyOutput("plotKernels")))
              )
            ),
            ########################################################
            tabPanel("Blood Feeding Attempt",
              fluidRow(h1("")),
              fluidRow(
                column(VAR_DESCR_COL_WIDTH,helpText("bfa.p: Probability of surviving a blood-feed attempt.")),
                column(VAR_SLIDE_COL_WIDTH,sliderInput("bfa.p",NULL,min=0,max=1,value=.9))
              ),
              fluidRow(
                column(VAR_DESCR_COL_WIDTH,helpText("bfa.s: Probability of success in a blood-feed attempt.")),
                column(VAR_SLIDE_COL_WIDTH,sliderInput("bfa.s",NULL,min=0,max=1,value=.3))
              ),
              fluidRow(
                column(VAR_DESCR_COL_WIDTH,helpText("bfa.t: Mean time between blood-feed attempts (days).")),
                column(VAR_SLIDE_COL_WIDTH,sliderInput("bfa.t",NULL,min=0,max=5,value=.65,step=.05))
              ),
              fluidRow(
                column(VAR_DESCR_COL_WIDTH,helpText("Q: Proportion of Bites on Humans.")),
                column(VAR_SLIDE_COL_WIDTH,sliderInput("Q",NULL,min=0,max=1,value=.9))
              ),
              h1(),h1(),
              img(src='boutF.png',align="center",width="50%")
            ),
            ########################################################
            tabPanel("Post Prandial Resting",
              fluidRow(h1("")),
              fluidRow(
                column(VAR_DESCR_COL_WIDTH,helpText("ppr.p: Probability of surviving a resting attempt.")),
                column(VAR_SLIDE_COL_WIDTH,sliderInput("ppr.p",NULL,min=0,max=1,value=.9))
              ),
              fluidRow(
                column(VAR_DESCR_COL_WIDTH,helpText("ppr.p: Mean resting time (days).")),
                column(VAR_SLIDE_COL_WIDTH,sliderInput("ppr.t",NULL,min=0,max=5,value=4/5,step=.05))
              ),
              fluidRow(
                column(VAR_DESCR_COL_WIDTH,helpText("reFeed: Re-feeding probability.")),
                column(VAR_SLIDE_COL_WIDTH,sliderInput("reFeed",NULL,min=0,max=1,value=.9))
              ),
              h1(),h1(),
              img(src='boutR.png',align="center",width="50%")
            ),
            ########################################################
            tabPanel("Egg Laying Attempt",
              fluidRow(h1("")),
              fluidRow(
                column(VAR_DESCR_COL_WIDTH,helpText("ela.p: Probability of surviving an egg-laying attempt.")),
                column(VAR_SLIDE_COL_WIDTH,sliderInput("ela.p",NULL,min=0,max=1,value=.95))
              ),
              fluidRow(
                column(VAR_DESCR_COL_WIDTH,helpText("ela.s: Probability of succeding an egg-laying attempt.")),
                column(VAR_SLIDE_COL_WIDTH,sliderInput("ela.s",NULL,min=0,max=1,value=.75))
              ),
              fluidRow(
                column(VAR_DESCR_COL_WIDTH,helpText("ela.t: Mean time elapsed in egg-laying attempt (days).")),
                column(VAR_SLIDE_COL_WIDTH,sliderInput("ela.t",NULL,min=0,max=5,value=1,step=.05))
              ),
              h1(),h1(),
              img(src='boutO.png',align="center",width="50%")
            )
          )
        )
      )
    ),
    # Ouptuts tab #########################################################################################
    tabPanel("Outputs",
      #plotOutput("distPlot")
      fluidRow(
        actionButton(inputId="exportPlots",label="Export Plots to Files", style='text-align:center; width:100%; height:100%; position:center; margin:5 auto; display:block'),
        h1(),
        h1()
      ),
      fluidRow(
        column(3,plotlyOutput("plotMosquitoLifespans")),
        column(3,plotlyOutput("plotBMInterval")),
        column(3,plotlyOutput("plotHumanBMInterval")),
        column(3,plotlyOutput("plotHumanBM"))
      )
    ),
    # Import JSON tab ###########################################################################################
    tabPanel("Explore History",
      sidebarLayout(
        sidebarPanel(
          includeMarkdown("json.md"),
          h1("")
        ),
        mainPanel(
          h2("Explore Data"),
          #fileInput('loadJSON',NULL,accept=c('.json')),
          actionButton(inputId="loadJSON",label="Load data stored in the 'History' folder", style='text-align:center; width:100%; height:100%; position:center; margin:5 auto; display:block'),
          h2(""),
          tabsetPanel(
            tabPanel("Dissect Mosquito",
              fluidRow(
                column(1,htmlOutput("dissectedMosquito")),#selectInput("dissectedMosquito", "Select Mosquito ID:",1:100)),
                column(8,plotlyOutput("dissectionPlot",height="500px")),
                column(3,
                  fluidRow(h2("Mosquito Dissection")),
                  fluidRow("This plot shows the transitions between states of an individual mosquito throughout its lifespan.")
                )
              )
            ),
            tabPanel("States Transitions Chord",
              #fluidRow(h2("Just a mockup for now.")),
              fluidRow(
                #column(9,chorddiagOutput("chordDiagram",height="1000px")),#,
                column(9,fluidRow(plotOutput("chordDiagram2",height="800px",width="800px"),plotOutput("chordDiagram3",height="800px",width="800px"))),
                #column(5,plotlyOutput("matrixPlotTransitions",height="500px",width="500px"))
                column(3,
                  fluidRow(h2("States Transitions Chord Diagram")),
                  fluidRow("This plot shows the transitions between states in the whole mosquito population under study.")
                )
              )
            ),
            tabPanel("States Transitions Matrix",
              #fluidRow(h2("Just a mockup for now.")),
              fluidRow(
                column(9,plotlyOutput("matrixPlot",height="700px",width="750px")),#,
                #column(5,plotlyOutput("matrixPlotTransitions",height="500px",width="500px"))
                column(3,
                  fluidRow(h2("States Transitions Matrix Plot")),
                  fluidRow("This plot shows the transitions between states in the whole mosquito population under study.")
                )
              )
            ),
            tabPanel("States Transitions Network",
              #fluidRow(h2("Just a mockup for now.")),
              fluidRow(
                column(9,plotOutput("statesNetwork",height="750px",width="800px")),#,
                #column(5,plotlyOutput("matrixPlotTransitions",height="500px",width="500px"))
                column(3,
                  fluidRow(h2("States Transitions Network")),
                  fluidRow("NEEDS SERIOUS EFFICIENCY UPDATE.")
                )
              )
            )
          )
        )
      )
    ),
    # Tutorial Videos tab #########################################################################################
    tabPanel("Tutorial Videos",
      sidebarLayout(
        sidebarPanel(
          includeMarkdown("tutorials.md"),
          h1("")
        ),
        mainPanel(
          fluidRow(
            h1()
          ),
          fluidRow(
            tabsetPanel(
              tabPanel("Basic",
                h1("Link to some Youtube tutorial videos here."),
                HTML('<iframe width="640" height="375" src="//www.youtube.com/embed/yVfb_uoU2vs?ecver=2" frameborder="0" allowfullscreen></iframe>')
              )
              # ),
              # tabPanel("Intermediate",
              #   h1("Link to some Youtube tutorial videos here."),
              #   h3("Quadcopter video used as a placeholder."),
              #   HTML('<iframe width="640" height="375" src="//www.youtube.com/embed/yVfb_uoU2vs?ecver=2" frameborder="0" allowfullscreen></iframe>')
              # ),
              # tabPanel("Advanced",
              #   h1("Link to some Youtube tutorial videos here."),
              #   h3("Quadcopter video used as a placeholder."),
              #   HTML('<iframe width="640" height="375" src="//www.youtube.com/embed/yVfb_uoU2vs?ecver=2" frameborder="0" allowfullscreen></iframe>')
              # )
            )
          )
        )
      )
    ),
    # About tab ###########################################################################################
    tabPanel("About",
        tabsetPanel(selected="The Project",
          tabPanel("The Model",includeMarkdown("model.md")),
          tabPanel("The Project",includeMarkdown("project.md")),
          tabPanel("People",includeMarkdown("team.md"))
        )
      )
    )
  )
)

