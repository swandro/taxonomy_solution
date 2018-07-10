fluidPage(
  titlePanel("Taxonomy Solution"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "FILE", label = "Upload OTU table", buttonLabel = "Upload")
    ),
    mainPanel(
      tableOutput(outputId = "view"), width = 8)
  )
)