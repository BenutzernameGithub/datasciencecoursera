#Write a function that takes a directory of data files and 
#a threshold for complete cases and calculates the correlation 
#between sulfate and nitrate for monitor locations where the 
#number of completely observed cases (on all variables) is 
#greater than the threshold. 
#The function should return a vector of correlations for the monitors 
#that meet the threshold requirement. If no monitors meet the 
#threshold requirement, then the function should return a numeric 
#vector of length 0.

corr <- function(directory, threshold = 0) {
  files_list <- list.files('specdata', full.names = TRUE) #list of files
  result <- NULL
  for (i in 1:332) {
    data <- read.csv(files_list[i])
    new_data <- data[complete.cases(data),]
  if (nrow(new_data) > threshold) {
    result <- c(result, cor(new_data[,'sulfate'], new_data[,'nitrate']))
  }}
  return (result)
}

