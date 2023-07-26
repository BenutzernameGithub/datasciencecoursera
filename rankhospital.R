§

head(data)

rankhospital <- function(state, outcome, rank) {
  library(dplyr)
  outcome_data <- 
    read.csv('rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv')
  
  if (!(state %in% outcome_data$State)) {
    stop("invalid state") 
  } 
  if (outcome %in% c('“heart attack”, “heart failure”, “pneumonia"')) {
    stop('invalid outcome')
  }
  
  if (outcome == 'heart attack') {
    outcome_data %>% filter(State == state) %>% 
      arrange(as.numeric
              (Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)) %>%
      nth(n=rank) %>% select(Hospital.Name) %>% print()
  }
  
  if (outcome == 'heart failure') {
    outcome_data %>% filter(State == state) %>% 
      arrange(as.numeric
              (Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)) %>%
      nth(n=rank) %>% print()
  }
  
  if (outcome == 'pneumonia') {
    outcome_data %>% filter(State == state) %>% 
      arrange(as.numeric
              (Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)) %>%
      nth(n=rank) %>% select(Hospital.Name) %>% print()
  }
}

rankhospital("TX", 'heart failure', 4)



library(dplyr)


