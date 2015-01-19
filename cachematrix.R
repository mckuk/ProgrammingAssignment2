## The following functions allow one to create a "matrix" object, with cached inverse of the
## stored matrix, and to get the value of the stored matrix and of its inverse. Using such
## "matrix" objects allows one to speed-up program calculations, since the
## inverse does not need to be computed repeatedly - the program may use the cached value.

## makeCacheMatrix returns a "matrix" object with cached inverse, which is a list of methods that:
## set(u) - set the value of the matrix to u; 
## get() - return the value of the matrix;
## setInverse(u) - set the cached value of the inverse of the matrix to u;
## getInverse() - return the cached value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
     xInv <- NULL
     
     set <- function(u) {
          x<<- u
          xInv<-- NULL
     }
     
     get <- function() { x }
     
     setInverse <- function(u) { xInv <<- u }
     
     getInverse <- function() { xInv }
     
     return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}


## cacheSolve returns the inverse of matrix contained in the "matrix" object created by 
## makeCacheMatrix. If the inverse is already cached, it returns the cached value. Otherwise it
## calculates the inverse, caches it in the "matrix" object and returns the calculated value.

cacheSolve <- function(x, ...) {
     inv <- x$getInverse()
     
     if(!is.null(inv)){
          message("getting cached data")
          return(inv)
     }
     
     data <- x$get()
     inv <- solve(data, ...)
     x$setInverse(inv)
     
     return(inv)
}
