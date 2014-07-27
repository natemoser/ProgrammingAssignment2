## cachematrix.R
## Nathan Moser (nate@rezonate.org)
##
## Implements an object can be used to calculate the inverse of a supplied matrix, 
## and caches the result in the object. If the object is used again to find
## the inverted matrix, the cached result will be returned, rather than the
## inverse being calculated again.

## makeCacheMatrix: accepts a matrix as input, and returns
## an object with functions for getting and setting the cached matrix, as
## well as getting and setting the inverted matrix.

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


## cacheSolve: accepts an object that was created via makeCacheMatrix, and 
## if the inverse of the matrix has been previously calculated, returns that
## cached result. Otherwise, the inverse of the matrix is calcuated, stored
## in the object passed in to cacheSolve, and then the inverse matrix is 
## returned. It is assumed that the matrix supplied is always invertable.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
