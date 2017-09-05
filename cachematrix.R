## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL ## Initialize/declare cachedInverse value
        set <- function(y) { ## set the value
                x <<- y
                cachedInverse <<- NULL
        }
        get <- function() x  ## get the value
        setInverse <- function(inverse) cachedInverse <<- inverse ## set the value of inverse
        getInverse <- function() cachedInverse ## get the value of inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Check if cachedInverse has already been calculated and return if so
        cachedInverse <- x$getInverse()
        if(!is.null(cachedInverse)) {
                message("getting cached data")
                return(cachedInverse)
        }
        
        ## Calculate the inverse of the matrix and set in the cache
        mat <- x$get()
        cachedInverse <- solve(mat, ...)
        x$setInverse(cachedInverse)
        cachedInverse
}