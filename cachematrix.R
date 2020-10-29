## These two functions allow one to get the inverse of a given matrix. If calculated, the inverse matrix is stored in the cache memory, and future calculations
## of the same matrix are avoided by obtaining the result directly from the cache.

## This function sets the environment for a given matrix, defining key functions to use in the cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function() i <<- solve(x)
        getinverse <- function() i
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function uses the parameters defined in the makeCacheMatrix() function to first, check if there is an inverse value in the cache and retrieve it,
## and second, if no value was found, calculate the inverse of the given matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setinverse()
        i
}
