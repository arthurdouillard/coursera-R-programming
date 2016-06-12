rankall <- function(outcome, num = "best") {
  NAME      <- 2
  STATE     <- 7
  ATTACK    <- 11
  FAILURE   <- 17
  PNEUMONIA <- 23
  
  outcome_list <- read.csv("data/outcome-of-care-measures.csv",
                      colClasses = "character")
  
  if (outcome != "heart attack"
      && outcome != "heart failure"
      && outcome != "pneumonia")
    stop("Invalid outcome.")
  
  if (outcome == "heart attack")
    outcome = ATTACK
  else if (coutcome == "heart failure")
    outcome = FAILURE
  else
    outcome = PNEUMONIA
  
  ans <- data.frame(hospital=character(0), state=character(0))
  
  outcome_list[, outcome] <- as.numeric(outcome_list[, outcome])
  for (state in unique(outcome_list$State)) {
      df <- outcome_list[outcome_list$State == state, ]
      df <- df[order(df[, outcome], df[, NAME], na.last = NA),]
      
      if (num == "worst")
        num = length(df2[, outcome])
      else if (num == "best")
        num = 1
      
      row <- data.frame(hospital=df[num, NAME], state=state)
      ans <- rbind(ans, row)
  }
  
  ans <- ans[order(ans[, 2]),]
  ans
}