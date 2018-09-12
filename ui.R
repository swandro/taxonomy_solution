fluidPage(
  titlePanel("Taxonomy Solution"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file1", 
                label = "Upload OTU table", 
                buttonLabel = "Upload")
    ),
    mainPanel(
      plotOutput(outputId = "graph"), width = 8)
  )
)