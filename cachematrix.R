## Create a special object that stores a matrix, and caches
## its inverse.

## This function creates an object that provides functions for storing
## and retrieving a matrix (set, get), as well as storing and retrieving
## the matrix's inverse (setinverse, getinverse).
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL ## reset the inverse to null
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function return a matrix that is the inverse of the matrix 
## stored in x. 
## - If there is a cached value, then that value is returned.
## - If a cached value does not exist, then the function calculates
## the inverse, caches it, and returns that value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
