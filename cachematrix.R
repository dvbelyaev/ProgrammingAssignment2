## Here are two functions for caching the inverse of a matrix:
##
##   - makeCacheMatrix creates a special "matrix" object that can cache
##     its inverse.
##
##   - cacheSolve computes the inverse of the special "matrix" returned
##     by makeCacheMatrix above. If the inverse has already been
##     calculated (and the matrix has not changed), then cacheSolve should
##     retrieve the inverse from the cache.
##
## Important assumption: the matrix supplied is always invertible.

## Generating a special "matrix" object that can cache its inverse

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


## Calculating the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

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
