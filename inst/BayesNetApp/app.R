library(shiny)
library(cyjShiny)
library(BayesNetBP)

data("toytree")
# `tree` and `subtree` are objects of class ClusterTree
tree <- toytree
subtree <- tree




#************************************************************
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

  # graph panels, main graph and subgraph
  fluidRow(
    column(6,
        cyjShinyOutput('cyjShiny')
    ),

    column(6,
        cyjShinyOutput("cyjShiny_subtree")
    )
  ),

  #----- this is a small hack that allows us to put some white space at the bottom
  mainPanel(
    h3(textOutput("whitespace"))
  )

)

#************************************************************
server <- function(input, output, session) {
  # https://stackoverflow.com/questions/33519816/shiny-what-is-the-difference-between-observeevent-and-eventreactive


  # initialization


  output$cyjShiny <- renderCyjShiny( cyjShiny(graphNELtoJSON(tree@graph$dag), "dagre") )

  output$cyjShiny_subtree <- renderCyjShiny( cyjShiny(graphNELtoJSON(subtree@graph$dag), "dagre") )

  loadStyleFile(system.file(package = "BayesNetBP", "data", "style.js"))


  # event observation

  observeEvent(input$update, {
    if(!is.null(input$file1) & class(toytree) == "ClusterTree"){
      tree <- get(load(input$file1$datapath))
      subtree <- tree

      output$cyjShiny <- renderCyjShiny( cyjShiny(graphNELtoJSON(tree@graph$dag), "dagre") )
      output$cyjShiny_subtree <- renderCyjShiny( cyjShiny(graphNELtoJSON(subtree@graph$dag), "dagre") )
    }

    # probably remove/change this later
    else{
      print("Invalid file")
    }
  })



}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
