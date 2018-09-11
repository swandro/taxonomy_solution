library(reshape2)
library(dplyr)

function(input, output){
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$OTU
    
    if (is.null(inFile))
      return(NULL)
    
    output$dat <- read.delim(file = inFile$datapath, header = T, sep = '\t', check.names = F)
  })
  
  output$view <- renderTable({
    head(datasetInput(), n = isolate(input$obs))
  })
  
}