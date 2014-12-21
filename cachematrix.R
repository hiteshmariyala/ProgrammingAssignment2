## below are the two functions to cache inverse of a matrix and calculate inverse of matrix or fetch it from cache.
##

## below function takes a matrix as input, and sets it inverse value to null for the first time, and creates setters, getters for matrix and its inverse and returns all of them as a list.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseofmatrix) inverse <<- inverseofmatrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## below function makes use of the list returned by the makeCacheMatrix function and takes inverse if its already available or calculates and stores back in the list.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
