#Write a function that reads a directory full of files 
#and reports the number of completely observed cases in each data file. 
#The function should return a data frame where the first column 
#is the name of the file and the second column is the number of 
#complete cases. 

complete <- function(directory, id = 1:332) {
  files_list <- list.files('specdata', full.names = TRUE) #list of files
  dat <- data.frame() #empty df as a shallow, which will be appended later
    for (i in id) {
      data <- read.csv(files_list[i]) #reading the df
      nobs <- sum(complete.cases(data)) #summing the complete cases
      dat_new <- data.frame("id" = i,'nobs' = nobs) #create a df with the id and nobs information
      dat <- rbind(dat, dat_new) #binding the dfs together
      }
return(dat)
}

#playing around with one observation
data <- read.csv('specdata/001.csv')
x <- sum(complete.cases(data))

df <- data.frame("id", x)
df
