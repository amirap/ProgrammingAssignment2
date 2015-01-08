## This R file contains a pair of functions 
## that cache the inverse of a matrix.

## makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse.cacheSolve. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse
## has already been calculated,then the cachesolve will
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cachaed data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
