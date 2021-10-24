# Problem 1 - Pollutant Mean
# -- Example --
# Note: the function RETURNS the value, you'll need to print() if necessary
# Relative path: pollutantMean("specdata", "sulfate", 1:10)
# Using absolute path: pollutantMean("C:/Users/jaspe/Desktop/Code/CMSC 197 DATASCI/Mini Project 1/specdata", "sulfate", 1:10)
# Example to display on console: print(pollutantMean("specdata", "sulfate", 1:10))
pollutantMean <- function(directory, pollutant, id = 1:332){
  fileList <- list.files(directory, full.names = TRUE)[id] # Select files with IDs passed
  dFrame <- data.frame()
  dFrame <- do.call(rbind, lapply(fileList, read.csv)) # Fill a dataframe with rows from all selected IDs
  return(mean(dFrame[, pollutant], na.rm = TRUE))
}

# Problem 2 - Complete Cases
# -- Example --
# Note: the function RETURNS the value, you'll need to print() if necessary
# Relative path: complete("specdata", c(2,4,8,10,12))
# Using absolute path: complete("C:/Users/jaspe/Desktop/Code/CMSC 197 DATASCI/Mini Project 1/specdata", c(2,4,8,10,12))
# Example to display on console: print(complete("specdata", c(2,4,8,10,12)))
complete <- function(directory, id = 1:332){
  fileList <- list.files(directory, full.names = TRUE)[id]
  dFrame <- data.frame()
  dFrame <- do.call(rbind, lapply(fileList, read.csv))
  dFrame <- subset(dFrame, !is.na(sulfate) & !is.na(nitrate)) # Create a subset of the dataframe with all complete rows
  count <- c()
  for(i in id){
    count <- c(count, nrow(subset(dFrame, ID == i))) # Count rows with their respective IDs
  }
  result <- data.frame(id = id, nobs = count)
  return(result)
}

# Problem 3 - Correlations
# -- Example --
# Note: the function RETURNS the value, you'll need to print(), head(), summary(), or length() if necessary
# Relative path: corr("specdata", 150)
# Using absolute path: corr("C:/Users/jaspe/Desktop/Code/CMSC 197 DATASCI/Mini Project 1/specdata", 150)
# Example to display on console: print(head(corr("specdata")))
corr <- function(directory, threshold = 0){
  fileList <- list.files(directory, full.names = TRUE)
  dFrame <- data.frame()
  dFrame <- do.call(rbind, lapply(fileList, read.csv))
  dFrame <- subset(dFrame, !is.na(sulfate) & !is.na(nitrate)) # Same steps as function complete()
  corrGroup <- c()
  for(id in unique(dFrame$ID)){ # Select unique IDs
    currentGroup <- subset(dFrame, ID == id) # Create a subset for each unique ID
    if(nrow(currentGroup) > threshold){ # If number of rows in one specified ID is greater than the given threshold, use it
      corrGroup <- c(corrGroup, cor(currentGroup$sulfate, currentGroup$nitrate))
    }
  }
  return(corrGroup)
}

# Problem 4 - Histogram
# Note: I turned this into a function to keep things clean
# -- Example --
# Relative path: createHist("rprog_data_HospData/outcome-of-care-measures.csv")
# Using absolute path: createHist("C:/Users/jaspe/Desktop/Code/CMSC 197 DATASCI/Mini Project 1/rprog_data_HospData/outcome-of-care-measures.csv")
createHist <- function(path){
  outcome <- read.csv(path, colClasses = "character")
  outcome[, 11] <- as.numeric(outcome[, 11]) # The 11th column contains data on Hospital 30-Day Death (Mortality) Rates from Heart Attack
  # #ADD8E6 is a hex value representing the color of the bars
  # main is top text
  # xlab is the label on the x-axis
  hist(outcome[, 11], col = "#ADD8E6", main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack", xlab = "Deaths")
}

# To quickly check out some runs, you can uncomment these (make sure working directory is set up properly)
# print(pollutantMean("specdata", "sulfate", 1:10))
# print(complete("specdata", c(2,4,8,10,12)))
# print(summary(corr("specdata")))
# createHist("rprog_data_HospData/outcome-of-care-measures.csv")