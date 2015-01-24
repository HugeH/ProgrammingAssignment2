##     makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##     cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        matrix_check <- NULL
        set <- function(y) {
                x <<- y
                matrix_check <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) matrix_check <<- solve
        getSolve <- function() matrix_check
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}
 

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix only if the inverse has not already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_check <- x$getSolve()
        if(!is.null(matrix_check)) {
                message("getting cached data")
                return(matrix_check)
        }
        data <- x$get()
        matrix_check <- solve(data, ...)
        x$setSolve(matrix_check)
        matrix_check
}
