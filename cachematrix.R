## This pair of functions enable caching the inverse of matrix. Indeed, it can be a really time-consuming operation.
## If the contents of a matrix are not changing, it may make sense to cache the value of the inverse so that 
## when we need it again, it can be looked up in the cache rather than recomputed.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    set <- function(y) {
        x <<- y
        invs <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invs <<- inverse
    getinverse <- function() invs
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    invs <- x$getinverse()
    if(!is.null(invs)) {
        message("getting cached data")
        return(invs)
    }
    data <- x$get()
    invs <- solve(data, ...)
    x$setinverse(invs)
    invs
}
