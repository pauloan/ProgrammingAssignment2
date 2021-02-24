## With these pair of functions I am trying to store the time consuming 
## operation of inverting a matrix in a cache. By programming these functions,
## I will be able to recall the inverse of matrix X promptly.

## makeCacheMatrix is designed to create a parent environment where a matrix
## and its inverse are set/get and output a list of with this information.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inv <<- solve
      getinverse<- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve is designed to extract the inverse of a matrix stored 
## specifically by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
