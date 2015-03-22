## This file contains two functions that implement a "special matrix" that 
## can store its inverse, in order of save computation time.


## This function creates one "special matrix" which can store its inverse.
## This function only creates the special matrix (x) itself, while the 
## calculation for its inverse is implemented in function cacheSolve()
makeCacheMatrix <- function(x = matrix()) {

    ## initialization for m, which stores the inverse matrix of x
    inverse <- NULL

    ## set function, sets the matrix itself (set matrix x by input argument y)
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## get function, get the matrix itself (matrix x)
    get <- function() x

    ## function to set the inverse matrix
    setinverse <- function(calculatedInverse) inverse <<- calculatedInverse
    ## function to get the inverse matrix
    getinverse <- function() m
    
    ## declare
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns a matrix that is the inverse of 'x'.
## And it uses cached value when applicable.
cacheSolve <- function(x, ...) {

    ## Call get inverse method.
    inverse <- x$getinverse()

    ## Check whether inverse matrix is cached. If yes, return the cache.
    if(!is.null(inverse)) {
        message("getting cached inverse matrix")
        return(inverse)
    }
    
    ## In case of no cache available, calculate inverse, set cache and return.
    originMatrix <- x$get()
    inverse <- solve(originMatrix, ...) ## calculate inverse
    x$setinverse(inverse)               ## set cache
    inverse
}
