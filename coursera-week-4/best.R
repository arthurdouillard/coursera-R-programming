# col 2 -> Hospital name
# col 7 -> State
# col 11 -> heart attack
# col 17 -> heart failure
# col 23 -> pneumonia
best <- function(state, outcome) {
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
  else
    message(paste("Looking for the best hospital in ", state, " for ",
                  outcome, "..."))
  
  df <- outcome_list[outcome_list$State == state,]
  if (outcome == "heart attack")
    df[which.min(df[, ATTACK]), NAME]
  else if (outcome == "heart failure")
    df[which.min(df[, FAILURE]), NAME]
  else
    df[which.min(df[, PNEUMONIA]), NAME]
}