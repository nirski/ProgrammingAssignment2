# A pair of functions that cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse.
# Example use:
# n <- 10
# x <- matrix(sample(1:n^2), nrow = n)
# y <- makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
    
    # initialize the inverse
    m <- NULL
    
    # method to set the original
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # method to get the original
    get <- function() {
        x
    }
    
    # method to set the inverse
    setsolve <- function(solve) {
        m <<- solve
    }
    
    # method to get the inverse
    getsolve <- function() {
        m
    }
    
    # return a list with the methods
    list(
        set = set,
        get = get,
        setsolve = setsolve,
        getsolve = getsolve
    )
    
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve retrieves the inverse from the cache. It is assumed that the matrix supplied is always invertible.
# Example use:
# s <- cacheSolve(y) # first time run
# s <- cacheSolve(y) # getting cached data

cacheSolve <- function(x, ...) {
    
    # use the method to get the inverse from the special "matrix" object
    m <- x$getsolve()
    
    # if the inverse is already set, return the cached inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # if not, get the original
    data <- x$get()
    
    # compute the inverse
    m <- solve(data, ...)
    
    # cache the inverse to the special "matrix" object
    x$setsolve(m)
    
    # and return the inverse
    m
    
}
