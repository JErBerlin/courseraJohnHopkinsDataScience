# load lib
library(hash) ##to use dictionary
library(dplyr)

# 2 Finding the best hospital in a state
best <- function(state, outcome) { 
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  
  ## Check that state and outcome are valid
  ### possible outcomes
  outcomesH <- hash()
  outcomesH[["heart attack"]] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  outcomesH[["heart failure"]] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  outcomesH[["pneumonia"]] <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  
  ### Check: if state or outcome not valid, stop with error message
  if(!(state %in% df$State)) stop("invalid state")
  if(!(outcome %in% names(outcomesH))) stop("invalid output")
  else {
    ind <- outcomesH[[outcome]]
  }

  ## filter by state
  bestH <- df %>% filter(State == state)
    
  ## Preprocess. Type conversion to numeric of col of interest (ind)
  bestH <- bestH[!is.na(as.numeric(bestH[,ind])),]
  bestH[,ind] <- as.numeric(bestH[,ind])

  ## select top of outcome (ind)
  bestH <- bestH %>% top_n(-1, bestH[,ind])
       
  ## Return hospital name in that state with lowest 30-day death ## rate
  ### if there is a tie, return first in alphabetical order
  ( top_n(bestH, -1, Hospital.Name) )$Hospital.Name
}
