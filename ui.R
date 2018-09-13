fluidPage(
  titlePanel("Stephen's taxonomy barplots"),
  sidebarLayout(
    sidebarPanel(h2("Input options"),
      fileInput(inputId = "file1", 
                label = "Upload OTU table", 
                buttonLabel = "Upload"),
      checkboxInput(inputId = "SAMPLES.IN.ROWS", label= "Samples in rows", value=TRUE),
      selectInput(inputId = "TaxLev",
                  label = "Taxonomic Level",
                  choices = c("L1","L2","L3","L4","L5","L6","L7"),
                  selected = "L2"),
      actionButton("graph.button","Plot"),
      numericInput(inputId= "num.taxa", label = "Number of taxa to plot", value = 4,min = 1, max=12),
      numericInput(inputId= "X.size", label = "Size of X-axis font", value = 10,min = 2, max=50),
      downloadButton(outputId= "Download", label="Download .tsv")
    ),
    mainPanel(h2("Taxa to plot"),
        tableOutput(outputId = "top.taxa.table"),
        plotOutput(outputId = "graph"), width=35)
  )
)