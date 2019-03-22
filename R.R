## This data used in this program comes from http://hospitalcompare.hhs.gov, and 
## contains information about the quality of care of over 4000 Medicare-certified
## hospitals in the US. Quality of care of each hospital will be assessed here by 
## the outcome "30 Day death rate from heart attack/ pneumonia/ heart failure".

## This program will take in 3 arguements. First, a 2 letter abbreviation for a state
## in the US. Second, the outcome being measure (ie. 30 Day death rate from heart attack
## or pneumonia or heart failure). Third, a ranking that specifies a hospital of what rank
## should be returned. 

## For example, rankhospital("TX", "heart failure", 4) will return a hospital in the state of
## Texas that is ranked 4th in terms of lowest 30 day mortality due to heart failure.

#This sets the working directory
setwd("C:/Users/Ben/Desktop/codes")

rankhospital <- function(state, outcomes, num = "best") {
  
  data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors = F)
  
## Following code looks at which outcome we are interested in and will get the required column
## for that outcome.
  
  if (outcomes == "heart attack") {
    outcomerow <- 11 
  } else if (outcomes == "heart failure") {
    outcomerow <- 17
  } else if (outcomes == "pneumonia") {
    outcomerow <- 23
  } else {
    return ("invalid outcome")
  }

## Following code checks whether the state entered as an arguement exists in the data or not  

  subset <- data[, "State", drop = T]
  subset <- unique(subset)
  if (! state %in% subset) {
    return ("invalid state")
  }
  
  statesubset <- data[data$State == state, ]  ##Gets a subset of the dataframe that contains the required state only
  columns <- c(2,7,outcomerow)
  statesubset <- statesubset[ , columns]  ##Further subsets it so that only the required columns remain
  good <- complete.cases(statesubset)  
  statesubset <- statesubset[good, ]  ##Any row with na values are omitted.
  ordered <- order(statesubset[,3], statesubset[,"Hospital.Name"])
  statesubset <- statesubset[ordered, ]  ##Order dataframe by mortality outcome, break ties using Hospital name
  
##If the ranking inputted is not a number, but "best" or "worst", the following code will
##return the hospital with the lowest and highest 30 day mortality respectively.
  
  if (num == "worst") {
    return(tail(statesubset,1)[[1]])
  } else if (num == "best") {
    return(head(statesubset,1)[[1]])
  } else {return(statesubset[num,1])}
}


rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)

