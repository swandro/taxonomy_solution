<<<<<<< HEAD
###Prototype
library(reshape2)
library(dplyr)

DELIMITER <- ";"
ORIENTATION <- 1  #0 if samples in columns, 1 if samples in rows

test.file <- read.delim(file = "test.txt", header = T, sep = '\t', check.names = F)
test.inv <- read.delim(file = "test_inv.txt", header = T, sep = '\t', check.names = F)

#####Data importing and formatting functions###################################################################
determine.delimiter <- function(string){
  #Determines what character separates the different levels of taxonomy
  string <- as.character(string)
  char.list <- unlist(strsplit(x= string, split = ''))   #Get a vector of all characters
  for (delim in c(":", ";", "|")){
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
}

format.taxonomy <- function(unformatted.string, delimiter=DELIMITER){
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


########################################################################################

###Formatting output functions

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
  temp <- DF %>% group_by("LEVEL") %>% summarize(value=sum(value))
  temp <- temp[order(temp$value, decreasing = T),]
  top.taxa <- data.frame(temp[1:NUMBER,1]) 
  #Add in "Other" only if it doesn't already exist
  if(!"Other"%in%top.taxa){
  top.taxa <- c(top.taxa, "Other")
  }
  
  #Create other category and condenst into melted dataframe
  temp2 <- DF
  #Make a factor with the levels as the top microbes. All non top microbes will be NA
  temp2[[LEVEL]] <- factor(temp2$L2, levels=top.taxa)
  #Change all NA to "Other"
  temp2[[LEVEL]][which(is.na(temp2[[LEVEL]]))] <- "Other"
  temp2 <- temp2 %>% group_by(variable, "LEVEL") %>% summarize(value=sum(value))
  colnames(temp2) <- c("variable","taxonomy","value")
  
  #Add back in metadata
  
  #Return the data frame
  return(temp2)
}

#temp <- dcast(dat.melt, formula = variable~L2,value.var = "value",  fun.aggregate = sum )
test <- make.other.category(DF=dat.melt, LEVEL="L3",NUMBER= 4)
########################################################################################

#####Workflow###########################################################################

#Load in the data
dat <- test.file
#Get samples in columns and bacteria in rows
if (ORIENTATION){
  dat <- invert(test.file)
  class(dat) <- "numeric"
  dat <- data.frame(dat, check.names = F)
}

#Get relative abundance
##Needs to be data frame with first column is sample names
sums <- apply(dat, 2, sum)
#NORMALIZE
dat.relative <- NULL
for (i in seq(ncol(dat))){
  dat.relative <- cbind(dat.relative, dat[,i]/sums[i])
}
colnames(dat.relative) <- colnames(dat)
rownames(dat.relative) <- rownames(dat)
dat.relative <- data.frame(dat.relative, check.names = F)


####Format taxonomy
#Get vector of all taxonomies
taxonomy.list <- rownames(dat)
#Get the delimiter
DELIMITER <- determine.delimiter(taxonomy.list[1])
#Get the number of taxonomy levels
TAX.COUNT <- lengths(regmatches(DELIMITER, gregexpr(DELIMITER, taxonomy.list[1]))) + 1 #taxonomy fields is the number of delimiters + 1
#Create new columns for each taxonomy level
lev.list <- c("L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9", "L10")[1:TAX.COUNT]
for (L in lev.list){
  dat[[L]] <- NA
  dat.relative[[L]] <- NA
}

#Add taxonomy columns to data frame
for (i in seq(nrow(dat))){
  taxonomies <- format.taxonomy(rownames(dat)[i])
  for (j in seq(length(taxonomies))){
    dat[i,lev.list[j]] <- taxonomies[j]
    dat[i,lev.list[j]] <- taxonomies[j]
  }
}

#Melt data
dat.melt <- melt(dat, id.vars = lev.list)
dat.relative.melt <- melt(dat.relative, id.vars = lev.list)

########################################################################################





=======
###Prototype
library(reshape2)
library(dplyr)

DELIMITER <- ";"
ORIENTATION <- 1  #0 if samples in columns, 1 if samples in rows

test.file <- read.delim(file = "test.txt", header = T, sep = '\t', check.names = F)
test.inv <- read.delim(file = "test_inv.txt", header = T, sep = '\t', check.names = F)

#####Data importing and formatting functions###################################################################
determine.delimiter <- function(string){
  #Determines what character separates the different levels of taxonomy
  string <- as.character(string)
  char.list <- unlist(strsplit(x= string, split = ''))   #Get a vector of all characters
  for (delim in c(":", ";", "|")){
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
}

format.taxonomy <- function(unformatted.string, delimiter=DELIMITER){
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

format.taxonomy.titles <- function(unformatted.string, delimiter=DELIMITER){
  #Takes in the unformatted taonomy string and produces a vector of the taxonomy titles
  
  #Split by delimiter
  tax.split <- unlist(strsplit(x = as.character(unformatted.string), split = delimiter))
  titles <- c()
  
  for (level in tax.split){
    level.split <- unlist(strsplit(x=level, split = '_'))
    level.format <- paste(x = head(x = level.split, n =  -1), collapse = '')
    titles <- c(titles, level.format)
  }
  
  return(titles)
}

apply.format.taxonomy <- function(row){
  #Function for apply that takes a row and adds in the taxonomy of all the present levels
  #Assumes that the unformatted taxonomy is in a column calles "unformatted.taxonomy
  
  tax.vector <- format.taxonomy(row["unformatted.taxonomy"], delimiter = DELIMITER)
  
  
}

########################################################################################

###Formatting output functions

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
  return(summarize(group_by_(DF, "variable", LEVEL), value = sum(value)))
}


detach(dat.melt)

make.other.category <- function(DF, LEVEL, NUMBER){
  #Makes the other category for a melted data frame
  #Uer specifies a number of categories
  
  #Get the names of the taxa to keep
  temp <- summarize(group_by_(DF,LEVEL), value = sum(value))
  taxa <- pull(temp[order(temp$value, decreasing = T),][LEVEL][1:NUMBER,1])
  
  ##Make the final data frame
  result <- NULL
  for (SAMPLE in levels(factor(DF$variable))){
    temp <- data.frame(summarize(group_by_(subset(DF, variable%in%SAMPLE), LEVEL), value = sum(value)))
    other.sum <- sum(temp[which(!temp[[LEVEL]]%in%taxa),]$value)
    to.add <- temp[which(temp[[LEVEL]]%in%taxa),]
    other.row <- c("Other", other.sum)
    to.add <- rbind(to.add, other.row)
    to.add$variable <- SAMPLE
    result <- rbind(result, to.add)
  }
  return(result)
}


make.other.category.cutoff <- function(DF, LEVEL, NUMBER){
  #Makes the other category for a melted data frame
  #Uer specifies a percent cutoff. Requires a relative abundance data frame
}

unmelt <- function(DF, LEVEL){
  #Unmelts data frame
  temp <- dcast(DF, variable~ eval(LEVEL), value.var = "value")
  return(temp)
}


########################################################################################

#####Workflow###########################################################################

#Load in the data
dat <- test.file
if (ORIENTATION){
  dat <- invert(test.file)
  class(dat) <- "numeric"
  dat <- data.frame(dat, check.names = F)
}
#Get relative abundance
##Needs to be data frame with first column is sample names
sums <- apply(dat, 2, sum)
#NORMALIZE
dat.relative <- NULL
for (i in seq(ncol(dat))){
  dat.relative <- cbind(dat.relative, dat[,i]/sums[i])
}
colnames(dat.relative) <- colnames(dat)
rownames(dat.relative) <- rownames(dat)
dat.relative <- data.frame(dat.relative, check.names = F)


####Format taxonomy
#Get vector of all taxonomies
taxonomy.list <- rownames(dat)
#Get the delimiter
DELIMITER <- determine.delimiter(taxonomy.list[1])
#Get the number of taxonomy levels
TAX.COUNT <- lengths(regmatches(taxonomy.list[1], gregexpr(DELIMITER, taxonomy.list[1]))) + 1 #taxonomy fields is the number of delimiters + 1
#Create new columns for each taxonomy level
lev.list <- c("L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9", "L10")[1:TAX.COUNT]
for (L in lev.list){
  dat[[L]] <- NA
  dat.relative[[L]] <- NA
}

#Add taxonomy columns to data frame
for (i in seq(nrow(dat))){
  taxonomies <- format.taxonomy(rownames(dat[i,]))
  for (j in seq(length(taxonomies))){
    dat[i,lev.list[j]] <- taxonomies[j]
    dat.relative[i,lev.list[j]] <- taxonomies[j]
  }
}

#Melt data
dat.melt <- melt(dat, id.vars = lev.list)
dat.relative.melt <- melt(dat.relative, id.vars = lev.list)

########################################################################################
>>>>>>> 77a1a358ebb4ae1991d050c337dc617e1dae9979
