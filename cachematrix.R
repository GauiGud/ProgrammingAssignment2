## These functions calculate the inverse of a matrix and stores
## it in cache.  Therefore it does not need to re-calculate
## the next time you need the inverse.

## This function, makeCacheMatrix, has four functions:
##     1: set - sets the matrix
##     2: get - returns the matrix
##     3: setinverse - calculates the inverse of the matrix
##     4: getinverse - returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
}


## This function, cacheSolve, uses the other function
## makeCacheMatrix to calculate the inverse of a matrix
## or returns the already calculated inverse

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
