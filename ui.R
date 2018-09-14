fluidPage(
  titlePanel("Stephen's taxonomy barplots"),
  fluidRow(
    column(3,
           actionButton("use.sample.data","Use sample data", width=170),
           h3("Upload Data"),
           div(style='height:45px;',fileInput(inputId = "file1", 
                     label=NULL,
                buttonLabel = "Upload")),
           checkboxInput(inputId = "SAMPLES.IN.ROWS", label= "Samples in rows", value=FALSE
                    ),
      br(),
      h5("Download condensed table"),
      downloadButton(outputId= "Download", label="Download", width=50),
      checkboxInput(inputId = "DOWNLOAD.MELTED", label= "Melted", value=FALSE)),
    column(3,h3("Options"),
      selectInput(inputId = "TaxLev",
                  label = "Taxonomic Level",
                  choices = c("L1","L2","L3","L4","L5","L6","L7"),
                  selected = "L2",
                  width=170),
      numericInput(inputId= "num.taxa", label = "Number of taxa to plot", value = 4,min = 1, max=11, width=170),
      actionButton("graph.button",tags$strong("Plot"), width=170, style='padding:20px')
    ),
    column(6,h3("Taxa to plot"),
           tableOutput(outputId = "top.taxa.table"))
  ),
  fluidRow(
    column(12,plotOutput(outputId = "graph")
    )
  ),
  fluidRow(
    column(4,
    numericInput(inputId= "WIDTH", label = "Plot width", value = 800,min =200, max=1400, width=170),
    numericInput(inputId= "X.size", label = "Size of X-axis font", value = 15,min = 2, max=50, width=170),
    numericInput(inputId= "LEGEND.SIZE", label = "Size of legend font", value = 15,min = 2, max=50, width=170)
    )
  )
)