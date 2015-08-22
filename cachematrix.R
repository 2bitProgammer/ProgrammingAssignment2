## The following functions can be used to compute and cache the inverse matrix
## of a martix.  This is useful for speeding up computations, particularly in
## loops where the inverse might already have been computed

## makeCacheMatrix creates a special "vector" of functions that can be used to
## cache an inverted matrix
makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL

    # Set the matrix value for which the cached inverse matrix will be computed on
    set <- function(m) {
        x <<- m
        cachedInverse <<- NULL # A new matrix has been set so invalidate old cached value
    }

    # Returns the matrix that is currently being cached
    get <- function() x

    # For setting the computed inverse matrix
    setInverse<- function(inv) cachedInverse <<- inv

    # For retrieving the cached inverse matrix
    getInverse <- function() cachedInverse

    # Return the functions as a list for use by cacheSolve
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve computes the inverse matrix of a matrix.  It checks the cache
## first to see if the inverse has already been computed, and if so returns
## the value.  If not, the inverse matrix is computed and stored in the cache
## as well as returned from the function
cacheSolve <- function(x, ...) {
    # Check to see if the inverse has already been computed and if so use it
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    # Otherwise, get the matrix that we need to compute against and find the inverse
    data <- x$get()
    m <- solve(data, ...)

    # Cache the result for future queries
    x$setInverse(m)

    # Return the computed result
    m
}
