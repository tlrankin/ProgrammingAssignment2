# Programming Assignment 2:  makeCacheMatrix and cacheSolve
# First, need to write a script to create a matrix object that can cache its
# inverse

makeCacheMatrix <- function (x = matrix()) {
      inv <- NULL # establishes the value of the inverse to "null"
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }             # this setter function grabs the value of x from the parent environment and resets inverse to "null"
      get <- function() x  # this getter line grabs the new matrix from the parent environment
      setinverse <- function(inverse) inv <<- inverse # assigns input argument from parent environment
      getinverse <- function() inv # gets the inverse from the parent environment
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}  # brings all together in a list

# Next need to develop the cacheSolve script to compute the inverse of the matrix
# returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
inv <- x$getinverse() # retrives value if cached in the parent environment
if(!is.null(inv)) {
  message("getting cached data")
  return(inv)
} # if a value for an inverse is cached, returns that value
data <- x$get() # if no value is cached, calculates a new value from input matrix
inv <- solve(data, ...)
x$setinverse(inv)
inv
}

