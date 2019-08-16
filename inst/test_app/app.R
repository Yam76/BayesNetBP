library(shiny)
library(cyjShiny)
library(BayesNetBP)

data("toytree")

tree <- toytree@graph$dag

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("BayesNetBP"),

  fluidRow(
    column(4,
           h4("Load Model"),
           # actionButton("compile", label = "Compile"),
           fileInput("file1", "Choose your file"),
           actionButton("update", label = "Update"),

           hr(),
           h4("Subgraph"),
           actionButton("sg_exp", label = "Expand"),
           actionButton("sg_add", label = "Add to list"),
           actionButton("sg_sub", label = "Subset"),
           actionButton("sg_reset", label = "Reset"),
           radioButtons("direct","Expand direction",
                        c("Up" = "in", "Down" = "out", "Both"="all"),
                        inline=TRUE),
           #verbatimTextOutput("added"),

           selectInput("layout",
                       label = "Choose graph layout",
                       choices = c("dagre", "cose")),

           helpText("Selected Node"),
           verbatimTextOutput("clickedNode")


    ),

    column(4,
           h4("Fixed Evidence"),
           selectInput("var",
                       label = "Choose variables to set evidence",
                       choices = c("DUMMY_VAR_1", "DUMMYN") ),

           selectInput("getvalue",
                       label = "Select value for discrete variable",
                       choices = c() ),

           textInput("evidence", "Enter value for continuous variable",
                     value = "", width = NULL, placeholder = NULL),

           actionButton("add", label = "Add evidence"),
           actionButton("clear", label = "Clear"),
           helpText("Evidence to be absorbed:"),
           verbatimTextOutput("added"),

           hr(),
           actionButton("absorb", label = "Absorb"),
           actionButton("reset", label = "Reset"),
           actionButton("marg", label="Plot Marginals"),
           actionButton("post", label = "Shift in marginals")

    ),

    column(4,
           h4("Effects of a spectrum of evidence"),

           selectInput("abvar",
                       label = "Choose the observed variable",
                       choices = c("DUMMY_NODE_CLASS_VAR_1", "DUMMY_NODE_CLASS_VAR_2")),

           #selectInput("var2",
           #            label = "Choose variables to plot",
           #            choices = tree.init.p$nodes),

           actionButton("addplot", label = "Add to plot list"),
           actionButton("addall", label = "Add all"),
           actionButton("clearpvar", label = "Clear"),
           actionButton("plotkld", label = "Plot"),

           helpText("Variables to plot:"),
           verbatimTextOutput("addplot"),

           sliderInput("range", "Range:",
                       min = -10, max = 10, value = c(-3,3)),
           sliderInput("increment", "Step:",
                       min = 0, max = 1, value = 0.5, step= 0.1)

    )
  ),




  # Sidebar layout with input and output definitions ----
  ######### Sidebar panel for inputs ----#######################
  # sidebarLayout(
  #
  #   sidebarPanel(
  #     # Input: Slider for the number of bins ----
  #     sliderInput(inputId = "bins",
  #                 label = "Number of bins:",
  #                 min = 1,
  #                 max = 50,
  #                 value = 30),
  #   ),
  #
  # )
  ####################################################

  ################ Main panel for displaying outputs ----######################
  mainPanel(
    textOutput("test_text"), #================================= test =======================

    textOutput("test_text_2"),

    # Output: Histogram ----
    # plotOutput(outputId = "distPlot"),

    cyjShinyOutput('cyjShiny', width="800px", height="800px")
  )
  ################################################################################


)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  # output$distPlot <- renderPlot({
  #
  #   x    <- faithful$waiting
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #
  #   hist(x, breaks = bins, col = "#75AADB", border = "white",
  #        xlab = "Waiting time to next eruption (in mins)",
  #        main = "Histogram of waiting times")
  # })
  output$test_text <- renderText({ #================================= test =======================
    paste(input$range[1])
  })

  output$test_text_2 <- renderText({ #================================= test =======================
    paste(0)
  })

  output$cyjShiny <- renderCyjShiny( cyjShiny(graphNELtoJSON(tree), "dagre") )

  observeEvent(input$update, {
    print("hello")
  })

  # https://stackoverflow.com/questions/33519816/shiny-what-is-the-difference-between-observeevent-and-eventreactive


}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
