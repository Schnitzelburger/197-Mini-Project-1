pollutantMean <- function(directory, pollutant, id = 1:332){
  fileList <- list.files(directory, full.names = TRUE)[id]
  dFrame <- data.frame()
  dFrame <- do.call(rbind, lapply(fileList, read.csv))
  return(mean(dFrame[, pollutant], na.rm = TRUE))
}

complete <- function(directory, id = 1:332){
  fileList <- list.files(directory, full.names = TRUE)[id]
  dFrame <- data.frame()
  dFrame <- do.call(rbind, lapply(fileList, read.csv))
  dFrame <- subset(dFrame, !is.na(sulfate) & !is.na(nitrate))
  count <- c()
  for(i in id){
    count <- c(count, nrow(subset(dFrame, ID == i)))
  }
  result <- data.frame(id = id, nobs = count)
  return(result)
}

# pollutantMean("specdata", "sulfate", 1:10)
# complete("specdata", 3)