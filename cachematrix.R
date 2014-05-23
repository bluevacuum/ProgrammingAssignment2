## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a matrix and inserts it into a list with various functions that
##can be called to get and set the inverse of the matrix so that the inverse of the matrix
##only need be computed once

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <-- y
    m <-- NULL
  }
 get <- function() x
 setinverse <- function(inverse) m <<- inverse
 getinverse <- function() m
 list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function checks to see if the inverse of the matrix has been computed
## and returnes the cached inverse if found, otherwise it computes the inverse
## of the matrix and stores it for later retrieval
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data  ## compute inverse
  x$setinverse(m)            ##store inverse for later retrieval
  m
  
}

