library(reshape2)
library(ggplot2)
library(shiny)


#####Data importing and formatting functions###################################################################

determine.delimiter <- function(string){
  #Determines what character separates the different levels of taxonomy
  string <- as.character(string)
  char.list <- unlist(strsplit(x= string, split = ''))   #Get a vector of all characters
  if ("|"%in%char.list){
    return("\\|")
  }
  for (delim in c(":", ";","_")){
    if (delim%in%char.list){
      return(delim)
    }
  }
  stop("Could not determine delimeter")
}

invert <- function(x){
  #inverts matrix so that samples are the columns
  inverted <- t(x)
  colnames(inverted) <- inverted[1,]
  inverted <- inverted[-1,]
  class(inverted) <- "numeric"
  inverted <- data.frame(inverted, check.names = F)
  return(inverted)
}

format.samples.in.columns <- function(x){
  rownames(x) <- x[,1]
  temp <- x
  x <- x[,-1]
  x <- data.frame(x)
  colnames(x) <- colnames(temp)[-1]
  rownames(x) <- rownames(temp)
  return(x)
}

format.taxonomy <- function(unformatted.string, delimiter){
  #Takes in an unformatted taxonomy string and the delimiter and produces a vector of the taxonomies without the titles
  #Assumes the taxonomy is separated by underscores ex. "s__Enterococcus;g__faecium"
  
  #Split by delimiter
  tax.split <- unlist(strsplit(x = as.character(unformatted.string), split = delimiter))
  taxonomies <- c()
  
  for (level in tax.split){
    #Split by the underscore and store the last value as the taxonomy name
    level.split <- unlist(strsplit(x=level, split = '_'))
    taxonomies <- c(taxonomies, rev(level.split)[1])
    #get the level of the taxonomy by taking everything else that isn't an underscore
    
  }
  
  return(taxonomies)
}

shorten.levels <- function(x, DELIMITER){
  #Determines if taxonomy is redundant and only returns the deepest level
  last <- rownames(x)[nrow(x)]
  number <- length(unlist(strsplit(last, split = DELIMITER)))
  result <- NULL
  rnames <- c()
  for (i in seq(nrow(x))){
    if (length(unlist(strsplit(rownames(x)[i], split = DELIMITER))) == number){
      result <- rbind(result,x[i,])
      rnames <- c(rnames,rownames(x)[i])
    }
  }
  result <- data.frame(result, check.names = F)
  rownames(result) <- rnames
  return(result)
}


########################################################################################

###Formatting output functions#########################

##MELTED RELATIVE ABUNDANCE
relative.abundance <- function(DF){
  #Takes in the melted data frame and makes it relative abundance
  sums <- summarize(group_by(dat.melt, variable), SUM=sum(value))
  result <- apply(dat.melt, 1, function(x){
    return(as.numeric(x["value"])/subset(sums, variable==x["variable"])$SUM)
  })
}

collapse.to.level <- function(DF, LEVEL){
  ##Collapse a data frame to a level
  return(summarize(group_by(DF, LEVEL, variable), value = sum(value)))
}

make.other.category <- function(DF, LEVEL, NUMBER){
  #Makes the other category for a melted data frame
  #Decides top microbes by greatest sum in all samples
  temp <- summarize(group_by_(DF,LEVEL),value=sum(value))
  temp <- temp[order(temp$value, decreasing = T),]
  top.taxa <- data.frame(temp[1:NUMBER,1])[,1]
  #Add in "Other" only if it doesn't already exist
  if(!"Other"%in%top.taxa){
    top.taxa <- c(top.taxa, "Other")
  } else{
    top.taxa <- c(top.taxa,data.frame(temp[NUMBER+1,1])[,1])
  }
  
  #Create other category and condenst into melted dataframe
  temp2 <- DF
  #Make a factor with the levels as the top microbes. All non top microbes will be NA
  temp2[[LEVEL]] <- factor(temp2[[LEVEL]], levels=top.taxa)
  #Change all NA to "Other"
  temp2[[LEVEL]][which(is.na(temp2[[LEVEL]]))] <- "Other"
  temp2 <- summarize(group_by_(temp2, "variable", LEVEL),value=sum(value))
  colnames(temp2) <- c("variable","taxonomy","value")
  
  #Add back in metadata
  
  #Return the data frame
  return(temp2)
}
########################################################################################


##Sample data######
sample.data <- read.delim(file = "sample_data.tsv", header = T, sep = '\t', check.names = F)
sample.data <- invert(sample.data)
###########


###Colors######
genus.colors <- c("#a6cee3", "#1f78b4", "#b2df8a","#33a02c","#fb9a99",
                  "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6","#6a3d9a",
                  "#ffff99", "#b15928")
################




function(input, output, session){

  #####Workflow###########################################################################
  
  SWITCH <- reactiveValues(sample.data=FALSE, any.data=FALSE)
  NUMBER.SAMPLES <- reactiveValues(num=8)
  X.INPUT.CHANGED <- reactiveValues(q=FALSE)
  
  observeEvent(input$use.sample.data,{
    SWITCH$sample.data <<- TRUE
    SWITCH$any.data <<- TRUE
    X.INPUT.CHANGED$q <<- FALSE
    })
  observeEvent(input$WIDTH!=800,{
    X.INPUT.CHANGED$q <<- TRUE
  })
  
  observeEvent(input$file1,{
    X.INPUT.CHANGED$q <<- FALSE
    uploaded.dat()
  })
  
  # output$testing <- renderText({
  #   print("hello")
  #   SWITCH$sample.dat <<- TRUE
  # })

  #Load in the data
  uploaded.dat <- reactive ({
    #req(input$file1)
    SWITCH$sample.data <<- FALSE
    SWITCH$any.data <<- TRUE
    dat <- read.delim(file = input$file1$datapath, header = T, sep = '\t', check.names = F)
    #Deal with docs with double header lines
    if (colnames(dat)[1]=="ID" & dat[1,1]=="#SampleID"){
      dat <- dat[-1,]
    }
    #Get samples in columns and bacteria in rows
    if (input$SAMPLES.IN.ROWS){
      dat <- invert(dat)
    } else{
      dat <- format.samples.in.columns(dat)
    }
    return(dat)
  })

  
  melted.raw.dat <- reactive({
    req(SWITCH$any.data)
    if (SWITCH$sample.data){
      dat <- sample.data
    } else {
    dat <- uploaded.dat()
    } 
    ####Format taxonomy
    #Get vector of all taxonomies
    taxonomy.list <- rownames(dat)
    #if the last row is unclassified, remove the row
    if (taxonomy.list[length(taxonomy.list)]=="unclassified"){
      dat <- dat[-nrow(dat),]
      taxonomy.list <- rownames(dat)
    }
    #Get the delimiter
    DELIMITER <- determine.delimiter(taxonomy.list[length(taxonomy.list)])
    
    #If the taxa is redundant because there are multiple levels, remove redundant info
    dat <- shorten.levels(dat, DELIMITER)
    
    #normalize data
    if (input$NORMALIZE){
      sums <- apply(dat, 2, sum)
      sums[which(sums==0)] <- 1
      for (i in seq(ncol(dat))){
        dat[,i] <- dat[,i]/sums[i]
      }
    }
    #Get the number of taxonomy levels
    TAX.COUNT <- lengths(regmatches(DELIMITER, gregexpr(DELIMITER, taxonomy.list[length(taxonomy.list)]))) + 1 #taxonomy fields is the number of delimiters + 1
    
    #Create new columns for each taxonomy level
    lev.list <- c("L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9", "L10")[1:TAX.COUNT]
    for (L in lev.list){
      dat[[L]] <- NA
    }
  
    #Add taxonomy columns to data frame
    for (i in seq(nrow(dat))){
      taxonomies <- format.taxonomy(rownames(dat)[i], DELIMITER)
      for (j in seq(length(taxonomies))){
        dat[i,lev.list[j]] <- taxonomies[j]
        dat[i,lev.list[j]] <- taxonomies[j]
      }
    }
    
    #Melt data
    dat.melt <- melt(dat, id.vars = lev.list)
    dat.melt$value <- as.numeric(dat.melt$value)
    if ("L7"%in%colnames(dat.melt)){
      temp <- apply(dat.melt, 1, function(x){return(paste(as.character(x["L6"]), as.character(x["L7"])))})
    dat.melt$L7 <- temp
    }
    #Get number of samples
    NUMBER.SAMPLES$num <<- length(levels(factor(dat.melt$variable)))
    
    #Return the melted raw dat
    return(dat.melt)
    })
  
  melted.other.dat <- reactive({
  #Make other category
  raw.data <- melted.raw.dat()
  dat.melt.other <- make.other.category(DF= raw.data, LEVEL=as.character(input$TaxLev),NUMBER= as.numeric(input$num.taxa))
  return(dat.melt.other)
  })

  output$top.taxa.table <- renderTable({
    data <- melted.other.dat()
    data.condensed <- as.data.frame(summarize(group_by(data ,taxonomy),abundance = round(sum(value), 1)))
    tot <- sum(data.condensed[,2])
    data.condensed[,2] <- data.condensed[,2]/tot
    return(data.condensed)
  })
  
  y.size <- reactive({
    input$graph.button
    return(isolate(as.numeric(input$HEIGHT)))
  })
  
  x.size <- reactive({
    input$graph.button
    if (isolate(X.INPUT.CHANGED$q)){
      # if (as.numeric(input$WIDTH) > 1400){
      #   return(1400)
      # } else{
     return(as.numeric(isolate(input$WIDTH)))
    } else{
      val <- 250 + (as.numeric(isolate(NUMBER.SAMPLES$num))-1)*20
      updateNumericInput(session, "WIDTH", value=val )
      return(val)
    }
  })
  
  output$graph <- renderPlot({
    input$graph.button
    isolate({
    fig.data <- melted.other.dat()
    color <- c(genus.colors[1:length(levels(factor(fig.data$taxonomy)))-1],"grey")
      ggplot(data = fig.data ,aes(x=variable,y=value,fill=taxonomy)) +
      geom_bar(stat="identity", width =.9) +
      scale_y_continuous(expand = c(0.002,0.002)) +
      labs(title=" ",x="",y="Relative Abundance", fill= "") +
      theme(plot.title=element_text(size=12,hjust= 0.5),
            axis.title=element_text(size=as.numeric(input$Y.title.size)),
            axis.text=element_text(size=10),
            axis.text.x = element_text(angle=-90,vjust=.5, hjust=0,color="black", size=as.numeric(input$X.size)),
            axis.text.y = element_text(size= as.numeric(input$Y.size), color="black"),
            strip.text=element_text(size=15),
            legend.text=element_text(size=as.numeric(input$LEGEND.SIZE)),
            legend.title=element_text(size=20),
            strip.background=element_rect(color="black", fill=NA,size=.5),
            panel.background=element_rect(fill=NA, color="black",size=.5),
            panel.grid=element_blank()) +
      scale_fill_manual(values = color)
    })
  }, width = x.size, height = y.size)
  
  output$Download <- downloadHandler(
    filename= function(){
      paste("Condensed_otu_table.tsv")
    },
    content= function(file){
      data <- melted.raw.dat()
      if(input$DOWNLOAD.OPTIONS=="Cond") {
        data <- melted.other.dat()
        data <- dcast(data = data, formula = variable~taxonomy, fill="value")
      } 
      write.table(data, file, sep = '\t',row.names=FALSE,quote = F)
    }
  )
  
}