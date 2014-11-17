## This file contains two methods to support caching of the computation of a
## inverse of a matrix. The first method returns a list of closures used to
## support caching of the calculation. And the second does the calculation
## and uses the list returned from the first function to set and retrieve the
## cached value.

## This function returns a list of closures used to cache the result
## of the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse to NULL.
    inv <- NULL
    
    # Closure for setting the matrix data.
    set <- function(y) {
        # Assign the new matrix data here.
        x <<- y
        
        # If new data is assigned, ensure that the inverse is recomputed by
        # un-caching it here.
        inv <<- NULL
    }
    
    # Closure for getting the matrix data.
    get <- function() {
        x
    }
    
    # Closure for setting the inverse data.
    setinv <- function(i) {
        inv <<- i
    }
    
    # Closure for getting the inverse data.
    getinv <- function() {
        inv
    }
    
    # Return a list of the supporting closures.
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This method attempts to use the cached value of the computed inverse value.
## If the value has not yet been calculated and cached, then it will compute
## the value of the inverse, cache it, and return it using the supplied list
## of closures.

cacheSolve <- function(x, ...) {
    # Attempt to retrieve a cached value for this cache object.
    inv <- x$getinv()
    if (is.null(inv)) {
        # We only get here if there isn't a cached value, so perform the
        # calculation, cache it, and assign it to the return value.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
    } else {
        # Provide some feedback for users if the cached inverse is used.
        message("getting cached data")
    }
    inv
}
