## makeCacheMatrix creates a special matrix object with functions to set, get, set the inverse, and get the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    invr <- NULL
    
    ## The set function sets the matrix to a new value 'y'
    ## and resets the stored inverse to NULL.
    set <- function(y) {
        x <<- y
        invr <<- NULL
    }
    
    ## The get function returns the current matrix.
    get <- function() x
    
    ## The setinverse function allows setting the inverse matrix directly.
    setinverse <- function(inverse) invr <<- inverse
    
    ## The getinverse function retrieves the currently stored inverse matrix.
    getinverse <- function() invr
    
    ## Return a list containing the set, get, setinverse, and getinverse functions.
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of a matrix using caching; caching the result for future use to avoid unnecessary recalculations.
cacheSolve <- function(x, ...) {
    ## Get the currently stored inverse matrix.
    invr <- x$getinverse()
    
    ## If the inverse matrix is already cached, print a message and return it.
    if (!is.null(invr)) {
        message("Getting Cached Data:-")
        return(invr)
    }
    
    ## If the inverse matrix is not cached, calculate it using the solve function.
    matrx <- x$get()
    invr <- solve(matrx, ...)
    
    ## Cache the calculated inverse for future use.
    x$setinverse(invr)
    
    ## Return the calculated inverse.
    invr
}

