## This file contains two functions working together to only calculate
##   the inverse of the matrix when it hasn't been calculated before.
## makeCacheMatrix() takes a matrix as an argument and returns 
##   a new object of it - cacheMatrix.
## cacheSolve() takes a cacheMatrix object as an argument and returnes 
##   inversed matrix for the one stored in the argument.

## Argument: x - a matrix.
## Returns: a vector of functions to get/set initial matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
       x <<- y
       inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Argument: x - cacheMatrix returned by makeCacheMatrix
## Returns: inverse of the matrix upon which cacheMatrix was built
## It is assumed that matrix is inversible.
## When the inverse is calculated for the first time, it is cached,
##   and on the next call for the same object the inverse is returned from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
  
}
