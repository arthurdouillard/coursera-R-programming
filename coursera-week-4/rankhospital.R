rankhospital <- function(state, outcome, num = "best") {
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
  else if (!(state %in% outcome_list$State))
    stop("Invalid state.")
  
  df <- outcome_list[outcome_list$State == state,]
  if (num != "best" && num != "worst" && num > length(df[, NAME]))
    stop("Invalid num.")
  
  if (outcome == "heart attack")
    outcome = ATTACK
  else if (coutcome == "heart failure")
    outcome = FAILURE
  else
    outcome = PNEUMONIA
  
  df[, outcome] <- as.numeric(df[, outcome])
  df2 <- df[order(df[, outcome], df[, NAME], na.last = NA),]
  
  if (num == "worst")
      num = length(df2[, outcome])
  else if (num == "best")
    num = 1
  
  df2[num, NAME]
  
}