## With these pair of functions I am trying to store the time consuming 
## operation of inverting a matrix in a cache. By programming these functions,
## I will be able to recall the inverse of matrix X promptly.

## makeCacheMatrix is designed to create a parent environment where a matrix
## and its inverse are stored and output a list of with this information.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse<- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve is designed to extract the inverse of a matrix stored 
## specifically by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
