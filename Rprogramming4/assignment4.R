# load lib
library(hash) ##to use dictionary
library(dplyr)

# readfile
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 

# 1 Plot the 30-day mortality rates for heart attack
##  col 11: 30-day death rates from heart attack 
outcome[, 11] <- as.numeric(outcome[, 11]) 
hist(outcome[, 11])

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
  
  ## Preprocess. Type conversion to numeric of col of interest (ind)
  bestH <- df[!is.na(as.numeric(df[,ind])),]
  bestH[,ind] <- as.numeric(bestH[,ind])

  ## filter and select
  bestH <- df %>% filter(State == state) 
  bestH <- bestH %>% top_n(-1, bestH[,ind])
       
  ## Return hospital name in that state with lowest 30-day death ## rate
  ### if there is a tie, return first in alphabetical order
  ( top_n(bestH, 1, Hospital.Name) )$Hospital.Name
}
