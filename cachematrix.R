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
  setinverse <- function(inverse) {
    i <<- inverse
  }
  getinverse <- function() {
    i
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##This function calculates the inverse of the special matrix returned by the above function.
##If the inverse has already been caluculated or the matrix hasn't changed, then the function should retrieve the matrix from the cache.
cacheSolve <- function(x, ...) {
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