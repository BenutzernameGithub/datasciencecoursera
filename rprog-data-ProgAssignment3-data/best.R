library(dplyr)

best <- function(state, outcome) {
  library(dplyr)
  outcome_data <- read.csv(
    'rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv'
  )
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
      slice_head(n = 1) %>% select(Hospital.Name) %>% print()
  }
  
  if (outcome == 'heart failure') {
    outcome_data %>% filter(State == state) %>% 
      arrange(as.numeric
        (Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)) %>%
      slice_head(n = 1) %>% select(Hospital.Name) %>% print()
  }
  
  if (outcome == 'pneumonia') {
    outcome_data %>% filter(State == state) %>% 
      arrange(
        as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)) %>%
      slice_head(n = 1) %>% select(Hospital.Name) %>% print()
  }
  
}
