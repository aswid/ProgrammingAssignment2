## In this file there is a pair of functions that cache the inverse of a matrix.
## First of functions makeCacheMatrix creates a special "matrix" object that can
## cache its inverse.The cacheSolve function computes the inverse of the special
## "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has 
## not changed), then cacheSolve will retrieve the inverse from the cache.


## Function return a special "matrix" object - 'x' that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {       ## set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x        ## get the value of the matrix
  setsolve <- function(solve) m <<- solve    ## set the value of the inverse
  getsolve <- function() m                   ## get the value of the inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  m <- x$getsolve()      ## gets the value of the inverse from the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)         ## sets the value of the inverse in the cache
  m
  
}
