# Calculating the inverse of a matrix can be computationally
# costly. The function makeCacheMatrix is a special data structure
# which stores a matrix x and its inverse. cacheSolve calculates the
# inverse of the matrix stored in this data structure, but returns the
# value in the cache if the inverse has already been calculated.

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

cacheSolve <- function(x) {
    # return the inverse of x, where x is a cachable data structure
    # created with makeCacheMatrix. If the matrix hasn't changed the
    # result is returned from the cache
    #
    # Args:
    #   x: an instance of makeCacheMatrix containing the matrix to be
    #   inverted.
    #
    # Returns:
    #   The inverse of the matrix (calculated using solve())

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
