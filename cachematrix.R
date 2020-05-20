##These functions cache the inverse of a matrix.

##This function creates a special matrix that can cache it's own inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {
    x
  }
  setmean <- function(inverse) {
    i <<- inverse
  }
  getmean <- function() {
    i
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cachemean <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}