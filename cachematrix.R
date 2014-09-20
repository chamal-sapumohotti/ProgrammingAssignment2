## These functions are designed to reduce the computation time
## by caching the matrix inverse calculation of a matrix.


## makeCacheMatrix function returns a list of 4 functions that
## are inside a closure which includes the original matrix and 
## a variable i which stores the inverse matrix.
## The 4 functions are as follows.
## set : set/reset the matrix object.
## get : return the matrix.
## setInverse : set the inverse value.
## getInverse : get the inverse value.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) i <<- inv
  
  getInverse <- function() i

  list(set= set, get= get,
        setInverse = setInverse, getInverse = getInverse)  

}


## cacheSolve function checks if the makeCacheMatrix object
## already has the inverse computed. If so, returns the inverse.
## else it will compute and cache the inverse before returning.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data, ...)  
  x$setInverse(inv)
  inv
}
