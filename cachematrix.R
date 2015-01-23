## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix makes a version of the matrix that is optimized by
## storing the very first time it is computed. Modifications to the
## matrix will result in resetting the cache of inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    # Getter and setter for the main function
    set <- function(y) {
        x <<- y
        # Null the inverse we cached if we receive a new value of x
        inv <<- NULL
    }

    get <- function() x

    # Getter and setter for inverse
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list()
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x@getinverse()
    if(!is.null(inv)) {
        return(inv)
    }
    inv <- solve(x$get(), ...)
    x$setinverse( inv )
    inv
}
