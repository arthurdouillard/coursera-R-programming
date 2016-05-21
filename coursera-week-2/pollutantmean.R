pollutantmean <- function(directory, pollutant, id=1:332) {
  myfiles = aggregator(directory, id)
  
  if (pollutant == "nitrate") {
    mean(myfiles$nitrate, na.rm=TRUE)
  } else {
    mean(myfiles$sulfate, na.rm=TRUE)
  }
}

aggregator <- function(dir_path, id) {
  # Read all csv correspond to id.
  myfiles = NA
  
  for (i in id) {
    format <- paste(formatC(i, width=3, flag="0"), ".csv", sep="")
    format <- paste(dir_path, format, sep="/")
    
    if (length(myfiles) <= 1 && is.na(myfiles)) {
      myfiles = read.csv(format)
    } else {
      myfiles = rbind(myfiles, read.csv(format))
    }
  }
  
  myfiles
}