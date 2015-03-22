## This package contains functions that will calculate the inverse of a matrix
## and cache the result.

## This function creates a special matrix to cache the inverse of the provided
## matrix.

makeCacheMatrix <- function(x = matrix()) {
    # This variable holds the cached inverse of the provided matrix
    i <- NULL
    
    # Define the four functions that support the cache for this matrix
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) i <<- inverse
    
    getInverse <- function() i
    
    # Return a list of the cache functions
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function returns the inverse of the provided matrix.  If the result has
## already been calculated, the function returns the cached copy.  Otherwise, 
## the inverse is calculated, cached, and returned.

cacheSolve <- function(x, ...) {
    # Look up the cached inverse for the provided matrix
    i <- x$getInverse()
    
    if(!is.null(i)) {
        # Found a cached inverse
        message("using cached inverse")
        return(i)
    }
    
    # No inverse found in cache
    i <- solve(x$get(), ...)    # Calculate the inverse
    x$setInverse(i)             # Cache the inverse

    i
}
