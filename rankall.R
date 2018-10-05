rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state_name <- levels(as.factor(data[,7]))
  ctgry <- c("heart attack", "heart failure", "pneumonia") 
  
  ## Check that state and outcome are valid
  if(!(outcome %in% ctgry )) stop("invalid outcome")
  ## For each state, find the hospital of the given rank
  col <- if(outcome == ctgry[1]){11}
  else if(outcome == ctgry[2]){17}
  else{23}
  
  data[,col] <- suppressWarnings(as.numeric(data[,col]))


  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name

  
  states.df <- subset(data, select = c(col, 2, 7))
  
  split.df <- split(states.df, states.df[3])
  
  staterank <- function(states.df) {
    #Make list of positions
    rank.list <- order(states.df[,1],states.df[,2], na.last = NA)
    
    #Check validity of num argument and assign numeric value
    if (num == "best")
      num <- 1
    else if (num == "worst")
      num <- length(rank.list)
    else if (!is.numeric(num))
      stop("Unrecognised num argument")
    
    states.df[rank.list[num],2]
  }
  
  ranked.states <- data.frame(sapply(split.df, staterank))
  ranked.states <- data.frame(ranked.states, row.names(ranked.states))
  names(ranked.states) <- c("hospital", "state")
  ranked.states
}
