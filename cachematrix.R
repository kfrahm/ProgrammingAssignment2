## Coursera - R Programming
## Assignment 2: Functions to cache the inverse of a matrix
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
     iv <- NULL
     set <- function(y){ ## Create a new matrix and reset the inverse
          x <<- y
          iv <<- NULL
     }
     get <- function() x ## Return the matrix
     setinverse <- function(inverse) iv <<- inverse
     getinverse <- function() iv
     list(set = set, get = get,
          setinv = setinverse,
          getinv = getinverse)
}


## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     iv <- x$getinv()
     if(!is.null(iv)){ ## Return the cached inverse if available
          message("getting cached inverse")
          return(iv)
     }
     data <- x$get()
     iv <- solve(data, ...) ##Compute the inverse of the matrix
     x$setinv(iv)
     iv
}
