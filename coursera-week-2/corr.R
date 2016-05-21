corr <- function(directory, threshold=0) {
  files = list.files(directory)
  
  ans = numeric()
  for (file in files) {
    file = paste(directory, file, sep="/")
   
    df = read.csv(file)
    if (sum(complete.cases(df)) > threshold) {
      ans <- c(ans, cor(df$nitrate, df$sulfate, use="pairwise.complete.obs"))
    }
  }
  
  ans
}