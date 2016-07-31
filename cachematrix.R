##  Program caches the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set_inv <- function(solve) i <<- solve
        get_inv <- function() i
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}


## cache if null inverse of x
cacheSolve <- function(x, ...) {
        i <- x$get_inv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$set_inv(i)
        i
}
