complete <- function(directory, id=1:332) {
  output <- data.frame(id=integer(), nobs=integer())
  it = 1
  
  for (i in id) {
    current_csv <- frame_retriever(directory, i)
    output[it,] <- c(i, sum(complete.cases(current_csv)))
    
    it = it + 1
  }
  
  output
}

frame_retriever <- function(directory, i) {
  name <- paste(formatC(i, width=3, flag="0"), ".csv", sep="")
  name <- paste(directory, name, sep="/")
  
  read.csv(name)
}