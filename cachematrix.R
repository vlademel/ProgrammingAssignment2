## These functions attempt to reduce processing time by creating a cache of the inverse of a matrix.
## Once the value has been cahce'd then instead of computing a new inverse everytime, the old one is returned.


## This function creates a list of functions that are to be used in cacheSolve
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve essentially checks to see whether or not an inverse matrix has been previously computed with the same 
## specified matrix
cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
} 