## the makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse.
## matrix is saved to x and inverse is saved to s
## you get back a list that includes methods:
## set: sets matrix and resets cached inverse
## get: returns matrix
## setSolve: saves solve value
## getSolve: returns cached inverse value

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() {
    x
  }
  setSolve <- function(solve) {
    s <<- solve
  }
  getSolve <- function() {
    s
  }
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Function to get the inversed matrix 
## from a special object created by makeCacheMatrix.
## If the inverse has already been calculated
## and the matrix has not changed
## then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}
