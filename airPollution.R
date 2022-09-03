pollutantmean <- function (directory, pollutant, id= 1:332){
  setwd(directory)
  pollVector <- vector()
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
  cleanVector <- pollVector[!is.na(pollVector)]
  print(cleanVector)
  meanPollutant <- mean(cleanVector)
  meanPollutant
}

pollutantmean('C:/Users/ACER/OneDrive/Desktop/projects/Project 1 files/specdata/'
,'nitrate', 23) 

complete <- function(directory, id=1:332){
  setwd(directory)
  fileList <- data.frame()
  fullCases <- 0
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
    for (rowNum in 1:nrow(file)){
      if(!any(is.na(file[rowNum,]))){
        fullCases <- fullCases + 1
      }
    }
    fileList <- rbind(fileList,c(value,fullCases))
    fullCases <- 0
  }
  names(fileList) <- c("id","nobs")
  print(fileList)
  }
complete('C:/Users/ACER/OneDrive/Desktop/projects/Project 1 files/specdata/', 1)