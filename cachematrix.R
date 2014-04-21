## This function is the constructor for a "special" matrix that is able
## to save its inverse in the cache to avoid unnecessary recalculations.
## It provides four methods for getting and setting the matrix and its inverse.
## - get() method will return the matrix to be inverted.
## - set(x) method will set a new matrix to be inverted and clear the cache.
## - getinv() method will return the inverse matrix from the cache
## - setinv(inv) method will save the inverse matrix in the cache
##
## Once an object is created by this constructor, you may change the underlying
## matrix to be inverted by the set() method. This will clear the cache, 
## and a new inverse matrix must be computed the next time via cacheSolve().


makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    
    get <- function() {
        x
    }
    
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    
    getinv <- function() {
        xinv
    }
    
    setinv <- function(inv) {
        xinv <<- inv
    }
    
    list(get = get, set = set, getinv = getinv, setinv = setinv)
}


## This function returns the inverse matrix of the "special" matrix
## constructed by the above makeCacheMatrix() constructor.
## The special matrix is passed as the argument x.
## If the inverse matrix is available in the cache, a message will
## be printed. 
## Otherwise the inverse matrix will be computed and saved in the cache.
## It is assumed that the matrix is always invertible

cacheSolve <- function(x, ...) {
    ## Try to retrieve the inverse from cache 
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("Getting cache data")
    }
    else {
        ## inverse matrix is not available yet
        ## retrieve matrix, compute its inverse, and save it in cache
        matrix <- x$get()
        inv <- solve(matrix)
        x$setinv(inv)
    }
    inv
}
