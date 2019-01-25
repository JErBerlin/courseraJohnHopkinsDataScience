rankhospital <- function(state, outcome, num = "best") { 

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
  if(!(outcome %in% names(outcomesH)))
    stop("invalid output")
  else 
    ind <- outcomesH[[outcome]]
  
  ## filter by state
  bestH <- df %>% filter(State == state)
  
  ## Preprocess. Type conversion to numeric of col of interest (ind)
  bestH <- bestH[!is.na(as.numeric(bestH[,ind])),]
  bestH[,ind] <- as.numeric(bestH[,ind])
  
  ## Order by outcome and then by name
  bestH <- bestH[order(bestH[,ind], bestH[,"Hospital.Name"]),]
  
  ## Return hospital name in that state with the given rank ## 30-day death rate
  nrows <- nrow(bestH)
  if(num == 'best') 
    return(bestH[1,"Hospital.Name"])
  else if(num=='worst')
    return(bestH[nrows,"Hospital.Name"])
  else if(num > nrows)
    return(NA)
  else
    return(bestH[num,"Hospital.Name"])
}
