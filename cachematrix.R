## Functions are meant to cache the inverse of a matrix so that there is no need
## to recalculate it each time it is needed

## This function creates a "matrix" object that can cache its inverse y setting 
## the value of the matrix, getting the value of the matrix, setting the value 
## the inverse matrix, and getting the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of the previously created matrix using
## the makeCacheMatrix function. It checks if the value of the matrix has 
## already been calculated and retrieves the value if so. If it hasn't, it then 
## calculates the inverse using the solve function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}