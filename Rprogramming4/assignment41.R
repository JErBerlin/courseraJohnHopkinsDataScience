# readfile
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 

# 1 Plot the 30-day mortality rates for heart attack
##  col 11: 30-day death rates from heart attack 
outcome[, 11] <- as.numeric(outcome[, 11]) 
hist(outcome[, 11])