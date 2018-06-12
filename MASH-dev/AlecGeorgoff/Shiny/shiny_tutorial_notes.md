## Structure
Script `app.R` lives in a directory (`newdir/`)

App can be run with `runApp("newdir")`

`app.R` has three components:
* user interface object
* server function
* call to the `shinyApp` function

`ui` - layout and appearance

`server` - instructions that your computer needs to build the app

`shinyApp` - creates Shiny app objects from an explicit UI/server pair

`app.R` layout:
```R
library(shiny)
ui <- ...
server <- ...
shinyApp(ui = ui, server = server)
```

Each app should live in its own directory

#### Recap
To create Shiny app:
* Make a directory named `myapp/` for your app
* Save your `app.R` script inside that directory
* Launch the app with `runApp` or RStudio's keyboard shortcuts
* Exit the Shiny app by clicking escape

## Building a User Interface
Bare minimum to create Shiny app:
```R
library(shiny)

# Define UI ----
ui <- fluidPage(

)

# Define server logic ----
server <- function(input, output) {

}

# Run the app ----
shinyApp(ui = ui, server = server)
```

`fluidPage` adjusts to dimensions of user's browser window

_Example:_
```R
ui <- fluidPage(
  titlePanel("title panel"),

  sidebarLayout(
    sidebarPanel("sidebar panel"),
    mainPanel("main panel")
  )
)
```

![example](https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/images/sidebar-layout1.png)

`sidebarLayout` always takes two arguments:
* `sidebarPanel` function output
* `mainPanel` function output

More advanced layouts:
* `navbarPage`
* `fluidRow` & `column`
* [Layout Guide](https://shiny.rstudio.com/articles/layout-guide.html)

#### HTML Content
Shiny<br>Function | HTML5<br>Equivalent | Creates
:---: | :---: | :---:
`p`  |`<p>`   | A paragraph of text
`h1`  |`<h1>`   | A first level header
`h2`  |`<h2>`   | A second level header
`h3`  |`<h3>`   | A third level header
`h4`  |`<h4>`   | A fourth level header
`h5`  |`<h5>`   | A fifth level header
`h6`  |`<h6>`   | A sixth level header
`a`  |`<a>`   | A hyperlink
`br`  |`<br>`   | A line break<br>(e.g. a blank line)
`div`  |`<div>`   | A division of text<br>with a uniform style
`span`  |`<span>`   | An in-line division of text<br>with a uniform style
`pre`  |`<pre>`   | Text 'as is' in<br>a fixed width font
`code`  |`<code>`   | A formatted block of code
`img`  |`<img>`   | An image
`strong`  |`<strong>`   | Bold text
`em`  |`<em>`   | Italicized text
`HTML`  |   | Directly passes a character<br>string as HTML code

To create header element:
* select a header function (e.g., h1 or h5)
* give it the text you want to see in the header
* pass `h1("My title")` as an argument to `titlePanel`, `sidebarPanel`, or `mainPanel`

_Example:_
```R
ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      h1("First level title"),
      h2("Second level title"),
      h3("Third level title"),
      h4("Fourth level title"),
      h5("Fifth level title"),
      h6("Sixth level title")
    )
  )
)
```

![example](https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/images/headers.png)

#### Formatting Text
_Example:_
```R
ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      p("p creates a paragraph of text."),
      p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
      strong("strong() makes bold text."),
      em("em() creates italicized (i.e, emphasized) text."),
      br(),
      code("code displays your text similar to computer code"),
      div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
      br(),
      p("span does the same thing as div, but it works with",
        span("groups of words", style = "color:blue"),
        "that appear inside a paragraph.")
    )
  )
)
```

![example](https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/images/formatting.png)

#### Images
```R
img(src = "my_image.png", height = 72, width = 72)
```

Image file _must_ be in a folder named `www` in the same directory as the `app.R` script

_Example:_
```R
ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      img(src = "rstudio.png", height = 140, width = 400)
    )
  )
)
```

![example](https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/images/image-in-app.png)

#### Other Tags
* [Customize your UI with HTML](https://shiny.rstudio.com/articles/html-tags.html)
* [Shiny HTML Tags Glossary](https://shiny.rstudio.com/articles/tag-glossary.html)

#### Recap
You can:
* create a user interface with `fluidPage`, `titlePanel` and `sidebarLayout`
* create an HTML element with one of Shiny's tag functions
* set HTML tag attributes in the arguments of each tag function
* add an element to your web page by passing it to `titlePanel`, `sidebarPanel` or `mainPanel`
* add multiple elements to each panel by separating them with a comma
* add images by placing your image in a folder labeled `www` within your Shiny app directory and then calling the `img` function

## Widgets
#### Control Widgets
![control widgets](https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/images/basic-widgets.png)

The standard Shiny widgets are:

Function | Widget
:--- | :---
`actionButton`  |  Action Button
`checkboxGroupInput`  | A group of check boxes
`checkboxInput`  | A single check box
`dateInput`  | A calendar to aid date selection
`dateRangeInput`  | A pair of calendars for selecting a date range
`fileInput`  | A file upload control wizard
`helpText`  | Help text that can be added to an input form
`numericInput`  | A field to enter numbers
`radioButtons`  | A set of radio buttons
`selectInput`  | A box with choices to select from
`sliderInput`  | A slider bar
`submitButton`  | A submit button
`textInput`  | A field to enter text

To add a widget to your app, place a widget function in `sidebarPanel` or `mainPanel` in your `ui` object

Each function requires several arguments. The first two are:
* a **name for the widget**: The user will not see this name, but you can use it to access the widget's value. The name should be a character string
* a **label**: This label will appear with the widget in your app. It should be a character string, but it can be an empty string `""`

_Example:_ `actionButton("action", label = "Action")`

Remaining arguments vary from widget to widget

_Example:_
```R
library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Basic widgets"),

  fluidRow(

    column(3,
           h3("Buttons"),
           actionButton("action", "Action"),
           br(),
           br(),
           submitButton("Submit")),

    column(3,
           h3("Single checkbox"),
           checkboxInput("checkbox", "Choice A", value = TRUE)),

    column(3,
           checkboxGroupInput("checkGroup",
                              h3("Checkbox group"),
                              choices = list("Choice 1" = 1,
                                             "Choice 2" = 2,
                                             "Choice 3" = 3),
                              selected = 1)),

    column(3,
           dateInput("date",
                     h3("Date input"),
                     value = "2014-01-01"))
  ),

  fluidRow(

    column(3,
           dateRangeInput("dates", h3("Date range"))),

    column(3,
           fileInput("file", h3("File input"))),

    column(3,
           h3("Help text"),
           helpText("Note: help text isn't a true widget,",
                    "but it provides an easy way to add text to",
                    "accompany other widgets.")),

    column(3,
           numericInput("num",
                        h3("Numeric input"),
                        value = 1))
  ),

  fluidRow(

    column(3,
           radioButtons("radio", h3("Radio buttons"),
                        choices = list("Choice 1" = 1, "Choice 2" = 2,
                                       "Choice 3" = 3),selected = 1)),

    column(3,
           selectInput("select", h3("Select box"),
                       choices = list("Choice 1" = 1, "Choice 2" = 2,
                                      "Choice 3" = 3), selected = 1)),

    column(3,
           sliderInput("slider1", h3("Sliders"),
                       min = 0, max = 100, value = 50),
           sliderInput("slider2", "",
                       min = 0, max = 100, value = c(25, 75))
    ),

    column(3,
           textInput("text", h3("Text input"),
                     value = "Enter text..."))
  )

)

# Define server logic ----
server <- function(input, output) {

}

# Run the app ----
shinyApp(ui = ui, server = server)
```

#### Recap
It is easy to add fully functional widgets to your Shiny app.
* Shiny provides a family of functions to create these widgets
* Each function requires a name and a label
* Some widgets need specific instructions to do their jobs
* You add widgets to your Shiny app just like you added other types of HTML content

#### Go Further
The [Shiny Widgets Gallery](http://shiny.rstudio.com/gallery/widget-gallery.html) provides templates that you can use to quickly add widgets to your Shiny apps

## Display Reactive Output
### Two Steps
Two step process:
1. Add an R object to your user interface
2. Tell Shiny how to build the object in the server function. The object will be reactive if the code that builds it calls a widget value

#### Step 1: Add an R object to the UI
Shiny provides a family of functions that turn R objects into output for your user interface. Each function creates a specific type of output.

| Output Function | Creates |
| :---: | :---: |
|`dataTableOutput`   | DataTable  |
|`htmlOutput`   | raw HTML  |
|`imageOutput`   | image  |
|`plotOutput`   | plot  |
|`tableOutput`   | table  |
|`textOutput`   | text  |
|`uiOutput`   | raw HTML  |
|`verbatimTextOutput`   | text  |

_Example:_
```R
ui <- fluidPage(
  titlePanel("censusVis"),

  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with
               information from the 2010 US Census."),

      selectInput("var",
                  label = "Choose a variable to display",
                  choices = c("Percent White",
                              "Percent Black",
                              "Percent Hispanic",
                              "Percent Asian"),
                  selected = "Percent White"),

      sliderInput("range",
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100))
    ),

    mainPanel(
      textOutput("selected_var")
    )
  )
)
```

Notice that `textOutput` takes an argument, the character string `"selected_var"`. Each of the `*Output` functions require a single argument: a character string that Shiny will use as the name of your reactive element. Your users will not see this name, but you will use it later.
