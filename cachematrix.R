## The function makeCacheMatrix stores the inverse of the input matrix, the function cacheSolve
## returns the inverse

## The function caches the inverse of a square matrix and returns a message if the input matrix 
## is not square  

makeCacheMatrix <- function(x = matrix()) {
    if (identical(nrow(x), ncol(x)) == TRUE) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        set_inv <- function(solve) inv <<- solve
        get_inv <- function() inv
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv) 
    } else {
        message("Input matrix needs to be square, solve function will not work with non-square matrices")
    }
}


## Returns the inverse of the matrix cached by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
    inv <- x$get_inv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inv(inv)
    inv
}
