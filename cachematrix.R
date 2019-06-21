## Put comments here that give an overall description of what your
## functions do
## These functions allow the caching of the inverse of a square matrix

## Write a short comment describing this function
## This function takes a matrix and returns a special list containing the matrix, its inverse,
## and functions to get and set the matrix and inverse.  The inverse is set to null if the matrix
## is set to a new value
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(inv) m <<- inv
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Write a short comment describing this function
## This function takes a list containing a square matrix created by makeCacheMatrix.  
## It will return the inverse of the matrix.  It first checks if the inverse is already cached
## in the list.  If it is cached, the cached value is returned.  Otherwse, the inverse is cacluated
## and set in the list.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
