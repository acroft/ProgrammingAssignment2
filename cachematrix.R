## A set of functions to implement a cacheable matrix inversion. Useful when inverting large matrices that commonly
## need to be inverted and the inversion takes a long time.

## This function is used to create a matrix with helper functions to get or set a cached inverse.

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


## This function returns the inverse of an input matrix which was made by makeCacheMatrix().

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
