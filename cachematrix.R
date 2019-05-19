## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that creates a list of four functions
## One to set an object to be the supplied matrix
## Two to get the matrix obcect
## Three to set the inverse of the matrix
## Four to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function that solves for the inverse of a matrix contained 
## in a makeCacheMatrix list or returns the cached inverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
