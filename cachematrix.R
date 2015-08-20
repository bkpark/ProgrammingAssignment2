## A set of functions for a special "matrix" that can cache its own inverse.

## makeCacheMatrix is constructor for cacheMatrix that can cache
## its own inverse; returns a list containing following four functions:
## set, get, setInverse, getInverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    return(list(set=set, get=get,
                setInverse=setInverse, getInverse=getInverse))
}

## cacheSolve checks the cache for inverse; if cache not found
## computes inverse, caches it, and returns the inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    y <- x$get()
    inv <- solve(y)
    x$setInverse(inv)
    return(inv)
}

## makeCacheMatrix2 builds cacheSolve into getInverse, so that user
## does not need to be aware that the solution is being cached,
## aside from "cacheSolve"'s message, and since "setInverse" is no
## longer necessary it is hidden from user interface
## (unless directly accessed through "orig.matrix" element)
makeCacheMatrix2 <- function(x = matrix()) {
    y <- makeCacheMatrix(x)
    getInverse <- function() cacheSolve(y)
    return(list(set=y$set, get=y$get,
                getInverse=getInverse, orig.matrix=y))
}
