##Reading the complete data
data <- 
  read.csv('rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv',
           na.strings = 'Not Available', stringsAsFactors = F)
##Only working with the needed columns 
needed_data <- data[, c(2,7,11,17,23)]
##Rewording the columns
names(needed_data) <-c('Hospital', 'State', 'Heart Attack', 'Heart Failure',
                       'Pneumonia')
##Creating outcome sets being ordered by State and given outcome
outcome_ha <- needed_data[order(needed_data$State, 
                                   needed_data$`Heart Attack`), 1:3]
outcome_hf <- needed_data[order(needed_data$State, 
                                needed_data$`Heart Failure`), c(1,2,4)]
outcome_p <- needed_data[order(needed_data$State, 
                              needed_data$`Pneumonia`), c(1,2,5)]
#Loading dplyr library
library(dplyr)

test <- as_tibble(outcome_hf) %>% group_by(State) %>%
  summarize(across(c('Hospital', 'Heart Failure'), ~ nth(., 10))) %>%
  select('Hospital', 'State')
test

#Creating new column order
col_order <- c('State', 'Hospital', 'Heart Attack')

new_result <- test[, col_order]
#Testing seems to work. Now writing the actual function. 

rankall <- function(outcome, number) {
  data <- 
    read.csv('rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv',
             na.strings = 'Not Available', stringsAsFactors = F)
  
  #if (!(state %in% data$State)) {
  #  stop("invalid state") 
  #} 
  if (outcome %in% c('“heart attack”, “heart failure”, “pneumonia"')) {
    stop('invalid outcome')
  }  
  
  ##Only working with the needed columns 
  needed_data <- data[, c(2,7,11,17,23)]
  ##Rewording the columns
  names(needed_data) <-c('Hospital', 'State', 'Heart Attack', 'Heart Failure',
                         'Pneumonia')
  ##Creating outcome sets being ordered by State and given outcome
  outcome_ha <- needed_data[order(needed_data$State, 
                                  needed_data$`Heart Attack`), 1:3]
  outcome_hf <- needed_data[order(needed_data$State, 
                                  needed_data$`Heart Failure`), c(1,2,4)]
  outcome_p <- needed_data[order(needed_data$State, 
                                 needed_data$`Pneumonia`), c(1,2,5)]
  #Loading dplyr library
  library(dplyr)
  
  if (outcome == 'heart attack') {
    as_tibble(outcome_ha) %>% group_by(State) %>%
    summarize(across(c('Hospital', 'Heart Attack'), ~ nth(., number))) %>%
    select('Hospital', 'State') %>% print(n=Inf)
    }
  
  if (outcome == 'heart failure') {
    as_tibble(outcome_hf) %>% group_by(State) %>%
    summarize(across(c('Hospital', 'Heart Failure'), ~ nth(., number))) %>%
    select('Hospital', 'State') %>% print(n=Inf)
    }

  if (outcome == 'pneumonia') {
    as_tibble(outcome_p) %>% group_by(State) %>%
    summarize(across(c('Hospital', 'Pneumonia'), ~ nth(., number))) %>%
    select('Hospital', 'State') %>% print(n=Inf)
    }
}


rankall('heart attack', 4)
rankall('pneumonia', 66)
rankall("heart failure", 11)
View(result)
