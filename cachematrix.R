## The objective of these functions is to create a special "matrix" object that
## can cache its inverse. The inverse of the special "matrix" is computed. If 
## the matrix has not changed, then the inverse previously computed is retrieved
## from the cache.

## This function creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special matrix returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## remains unchanged), then this function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
