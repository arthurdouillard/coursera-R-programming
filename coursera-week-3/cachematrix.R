makeCachedMatrix <- function(x = Matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(z) inv <<- z
  getInv <- function() inv
  
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}

cachedInverse <- function(x) {
  inv <- x$getInv()
  if (!is.null(inv)) {
    # Already computed
    message("Getting cached data...")
    return(inv)
  }
  message("Computing inverse...")  
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}