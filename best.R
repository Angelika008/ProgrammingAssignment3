best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state_name <- levels(as.factor(data[,7]))
  ctgry <- c("heart attack", "heart failure", "pneumonia") 
  
  ## Check that state and outcome are valid
  if(!(state %in% state_name)) stop("invalid state")
  if(!(outcome %in% ctgry )) stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death
  col <- if(outcome == ctgry[1]){11}
          else if(outcome == ctgry[2]){17}
          else{23}
   
  
  data[,col] <- suppressWarnings(as.numeric(data[,col]))
  statedata <- data[grep(state, data$State), ]
  orderdata <- statedata[order(statedata[, col], statedata[, 2], na.last = NA), ]
  orderdata[1,2]
}