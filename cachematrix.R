## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
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
