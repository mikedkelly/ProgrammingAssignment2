## function: cachematrix.R
## owner: Mike Kelly 25-Jan-2015
## description: These functions return the inverse of a given matrix.
## To improve performance, it caches the matrix and the inverse result in the 
## global environment. If a subsequent request calls the function and the
## input matrix is in the cache, the result is returned from cache instead
## of re-calculated.

## makeCacheMatrix takes a square matrix as input and returns a list of functions
## that cacheSolve() will call to decide whether to invert the input matrix
## or return the inverse value stored in cache.
makeCacheMatrix <- function(x = matrix()) {
        
        # initially set the cached matrix object to NULL
        m <- NULL
        
        # create a function called called set() that scopes two objects into
        # the global environment: x, which stores the input matrix y, and m,
        # an empty value, which will eventually hold the cached inverse of 
        # the matrix.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # create a function called get() that returns the stored matrix from
        # the global environment.
        get <- function() {
                x
        }
        
        # create a function called setInverse() that calculates the inverse
        # of the cached matrix m using the built-in solve function.
        setInverse <- function(solve) {
                m <<- solve
        }
        
        # create a function called getInverse() that returns the value of the
        # inverted matrix.
        getInverse <- function() {
                m
        }
        
        # Return the above four functions as a list that can be called by cacheSolve()
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve takes an input matrix x and compares it to what is in cache.
## If it finds the matrix in cache, it returns the subsequent cached inverse.
## Otherwise it calculates the inverse and stores the new value in cache.
cacheSolve <- function(x, ...) {
        
        ## Assign the current value of the cache to m  
        m <- x$getInverse()
        
        ## If the cache was storing a value, return it and let the
        ## user know it is returning a cached value.
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        
        ## If the cache is empty, get the input matrix, calculate its inverse,
        ## and store the value in cache so it is available for reuse. Finally,
        ## return the calculated inverse m to the caller.
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
