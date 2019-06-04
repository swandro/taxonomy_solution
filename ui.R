fluidPage(
  titlePanel("Stephen's taxonomy barplots"),br(),
  fluidRow(
    column(8,
      fluidRow(
        column(6,
           h3("Upload Data"),
           actionButton("use.sample.data","Use sample data", width=170),
           checkboxInput(inputId = "SAMPLES.IN.ROWS", label= "Upload samples in rows", value=FALSE
           ),
           div(style='height:30px;margin-top:-10px',fileInput(inputId = "file1", 
                     label=NULL,
                buttonLabel = "Upload")),
           
            hr(),
            downloadButton(outputId= "Download", label="Download data", width=50, style='padding:50;margin-bottom:-15px'),
            radioButtons(inputId = "DOWNLOAD.OPTIONS", label= "", 
                         choiceNames=list("Condensed table","Melted, with all taxa"), 
                         choiceValues = list("Cond","Melt")),
            hr()),
        column(6,h3("Taxonomy Options"),
          selectInput(inputId = "TaxLev",
                      label = "Taxonomic Level",
                      choices = c("L1","L2","L3","L4","L5","L6","L7"),
                      selected = "L1",
                      width=170),
          numericInput(inputId= "num.taxa", label = "Number of taxa to plot", value = 3,min = 1, max=11, width=170),
          actionButton("graph.button",tags$strong("Plot"), width=170, style='padding:20px'),
          checkboxInput(inputId = "NORMALIZE", label= "Relative abundance", value=FALSE)
          )
        ),
  fluidRow(h3("Plot options"),
    column(3,
    numericInput(inputId= "WIDTH", label = "Plot width", value = 800,min =200, max=1400, width=170),
    numericInput(inputId= "HEIGHT", label = "Plot height", value = 400,min =200, max=1000, width=170)
    ),
    column(3,
    numericInput(inputId= "X.size", label = "X-axis font", value = 15,min = 2, max=50, width=170),
    numericInput(inputId= "LEGEND.SIZE", label = "Legend font", value = 15,min = 2, max=50, width=170)
    ),
    column(3,
           numericInput(inputId= "Y.size", label = "Y-axis font", value = 15,min = 2, max=50, width=170),
           numericInput(inputId= "Y.title.size", label = "Y-axis title", value = 20,min = 2, max=50, width=170)
    ))
  ),
  column(4,h3("Taxa to plot"),
         tableOutput(outputId = "top.taxa.table"))
    ),
  fluidRow(
    column(12,plotOutput(outputId = "graph")
    )
  )
)