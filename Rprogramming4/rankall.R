rankall <- function(outcome, num = "best") { 
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  
  ## Check that state and outcome are valid
  ### possible outcomes
  outcomesH <- hash()
  outcomesH[["heart attack"]] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  outcomesH[["heart failure"]] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  outcomesH[["pneumonia"]] <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  
  ### Check: if outcome not valid, stop with error message
  if(!(outcome %in% names(outcomesH)))
    stop("invalid output")
  else 
    ind <- outcomesH[[outcome]]
  
  ## For each state, find the hospital of the given rank
  bestH <- data.frame(matrix(ncol = 2, nrow = 0))
  names(bestH) <- c('hospital', 'state')
  ### retrieve and order states list
  States <- unique(df$State)
  States <- States[order(States)]
  
  for(state in States) {
    ## filter by state
    bestH1 <- df %>% filter(State == state)
    
    ## Preprocess. Type conversion to numeric of col of interest (ind)
    bestH1 <- bestH1[!is.na(as.numeric(bestH1[,ind])),]
    bestH1[,ind] <- as.numeric(bestH1[,ind])
    
    ## Order by outcome and then by name
    bestH1 <- bestH1[order(bestH1[,ind], bestH1[,"Hospital.Name"]),]
    
    ## Return hospital name in that state with the given rank ## 30-day death rate
    nrows <- nrow(bestH1)
    if(num == 'best') {
      bestH[nrow(bestH)+1,] <- c(bestH1[1,"Hospital.Name"], state)
    } else if(num=='worst') {
      bestH[nrow(bestH)+1,] <- c(bestH1[nrows,"Hospital.Name"], state)
    } else if(num > nrows) {
      bestH[nrow(bestH)+1,] <- c(NA, state)
    } else {
      bestH[nrow(bestH)+1,] <- c(bestH1[num,"Hospital.Name"], state)
    }
  }
  
  ## Return a data frame with the hospital names and the ## (abbreviated) state name
  return(bestH)
}