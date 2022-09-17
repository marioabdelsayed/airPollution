#takes a directory containing data files, the pollutant to 
#calculate the mean for and the numeric id of the file(s)
#containing the pollution information
pollutantmean <- function (directory, pollutant, id= 1:332){
  #set the working directory to the files
  setwd(directory)
  #vector to store pollutants
  pollVector <- vector()
  #determine file name
  for (value in id){
    if(value < 10){
     fileName <- paste0('00',value,'.csv')
    }
    else if (10<=value && value<100){
      fileName <- paste0('0',value,'.csv')
    }
    else {
      fileName <- paste0(value,'.csv')
    }
    file <- read.csv(fileName)
    pollVector <- append(pollVector,file[[pollutant]])
  }
  #remove na from vector
  cleanVector <- pollVector[!is.na(pollVector)]
  #calculate the mean
  meanPollutant <- mean(cleanVector)
  meanPollutant
}
#returns the number of complete cases (rows not containig emptry or na)
complete <- function(directory, id=1:332){
  #set working directory
  setwd(directory)
  #data frame to store the files and their case numbers
  fileList <- data.frame()
  #fullcases count
  fullCases <- 0
  #get file name
  for (value in id){
    if(value < 10){
      fileName <- paste0('00',value,'.csv')
    }
    else if (10<=value && value<100){
      fileName <- paste0('0',value,'.csv')
    }
    else {
      fileName <- paste0(value,'.csv')
    }
    file <- read.csv(fileName)
    #check if rows in file have na values, if not, increase case count
    for (rowNum in 1:nrow(file)){
      if(!any(is.na(file[rowNum,]))){
        fullCases <- fullCases + 1
      }
    }
    #add the file and its case count to the file list
    fileList <- rbind(fileList,c(value,fullCases))
    fullCases <- 0
  }
  #appropriately name the list 
  names(fileList) <- c("id","nobs")
  fileList
}

corr <- function(directory, threshold = 0){
  qualifiedFiles <- vector()
  corrResults <- vector()
  nitrate <- vector()
  sulfate <- vector()
  fullCases <- complete(directory)
  #determine the the files that meet the threshold
  for (row in 1: nrow(fullCases)){
    if (fullCases[row,2] >= threshold) {
      qualifiedFiles <- append(qualifiedFiles,fullCases[row,1])
    }
  }
  #get the pollutants from the qualified files
  if (length(qualifiedFiles) == 0){
    print('no files meet the threshold')
    return()
  }
  #get the file names and set them appropriately
  for(file in 1:length(qualifiedFiles)){
    if(qualifiedFiles[file] < 10){
      fileName <- paste0('00',qualifiedFiles[file],'.csv')
    }
    else if (10<=qualifiedFiles[file] && qualifiedFiles[file]<100){
      fileName <- paste0('0',qualifiedFiles[file],'.csv')
    }
    else{
      fileName <- paste0(qualifiedFiles[file],'.csv')
    }
    #read the file
    fileContent <- read.csv(fileName)
    #for every row, get the nitrate and sulfat values 
    for (rowNumber in 1:nrow(fileContent)){
      if(!any(is.na(fileContent[rowNumber,]))){
        nitrate <- append(nitrate,fileContent[rowNumber,][["nitrate"]])
        sulfate <- append(sulfate,fileContent[rowNumber,][["sulfate"]])
      }
    }
    #append the cor of the nitrate and sulate values to the list 
    corrResults <- append(corrResults,cor(nitrate,sulfate))
    #reset the pollutant vectors for the next file
    nitrate <- vector()
    sulfate <- vector()
  }
  corrResults
}

