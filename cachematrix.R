## These functions are described below.
## makeCacheMatrix: creates a custom matrix object, and is used on cacheSolve in order to cache its inverse
## cacheSolve: creates and caches the inverse of the custom matrix object provided.

## creates a custom matrix object; it can store its own value and its inverse altogether
## the inverse of this matrix must be calculated with cacheSolve in order to improve
## the calculation of the inverse (it is cached)
## after that, you can call getInverse to see the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Calculates and caches the inverse of the custom matrix object provided
## If the inverse was previously calculated, the value is returned from cache (it is not calculed over and over)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m  
}
