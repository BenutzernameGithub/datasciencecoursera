rankhospital <- function(state, outcome, rank) {
  #Load dplyr library
  library(dplyr)
  outcome_data <- 
    read.csv('rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv',
             na.strings = 'Not Available', stringsAsFactors = F)
  
  needed_data <- outcome_data[, c(2,7,11,17,23)]
  ##Rewording the columns
  names(needed_data) <-c('Hospital', 'State', 'Heart Attack', 'Heart Failure',
                         'Pneumonia')
  ##Creating outcome sets being ordered by State and given outcome
  outcome_ha <- needed_data[order(needed_data$State, 
                                  needed_data$`Heart Attack`,
                                  needed_data$Hospital), 1:3]
  outcome_hf <- needed_data[order(needed_data$State, 
                                  needed_data$`Heart Failure`, 
                                  needed_data$Hospital), c(1,2,4)]
  outcome_p <- needed_data[order(needed_data$State,
                                 needed_data$`Pneumonia`,
                                 needed_data$Hospital), c(1,2,5)]
  
  if (!(state %in% outcome_data$State)) {
    stop("invalid state") 
  } 
  if (outcome %in% c('“heart attack”, “heart failure”, “pneumonia"')) {
    stop('invalid outcome')
  }
  
  
  if (outcome == 'heart attack') {
    as_tibble(outcome_ha) %>% filter(outcome_ha$State == state) %>%
      na.omit() %>% summarize(across(c('Hospital', 'Heart Attack'), 
                                     ~ nth(., rank))) %>%
      select('Hospital') %>% print()
  }
  
  if (outcome == 'heart failure') {
    as_tibble(outcome_hf) %>% filter(outcome_hf$State == state) %>%
      na.omit() %>% summarize(across(c('Hospital', 'Heart Failure'), 
                                     ~ nth(., rank))) %>%
      select('Hospital') %>% print()
  }
  
  if (outcome == 'pneumonia') {
    as_tibble(outcome_p) %>% filter(outcome_p$State == state) %>%
      na.omit() %>% summarize(across(c('Hospital', 'Pneumonia'), 
                                     ~ nth(., rank))) %>%
      select('Hospital') %>% print()
  }
}

rankhospital("NC", 'heart attack', 83)
rankhospital('WA', 'heart attack', 7)
rankhospital('TX', 'pneumonia', 10)
rankhospital('NY', 'heart attack', 7)
