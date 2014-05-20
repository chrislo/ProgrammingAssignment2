## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
    # Creates a data structure which caches the inverse of the matrix
    # x
    #
    # Args:
    #   x: the initial value of the matrix. No type checking is performed on the
    #   input. Defaults to an empty matrix.
    #
    # Returns:
    #   a vector of 4 functions
    #     set(a) - sets a new value of x
    #     get()  - returns the stored matrix
    #     setinverse(i) - sets the value of the cached inverse of x to the inverse i
    #     getinverse()  - returns the value of the cached inverse of x

    xinverse <- NULL
    set <- function(y) {
        x <<- y
        xinverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) xinverse <<- inverse
    getinverse <- function() xinverse
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    xinverse <- x$getinverse()
    if(!is.null(xinverse)) {
        message("getting cached data")
        return(xinverse)
    }
    data <- x$get()
    xinverse <- solve(data)
    x$setinverse(xinverse)
    xinverse
}
