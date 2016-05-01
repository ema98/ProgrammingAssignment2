## Matrix inversion is a costly computation. Caching the inverse of a matrix rather than computing it repeatedly
## can help save some time.

## makeCacheMatrix creates a special "matrix" object that caches its inverse with a list containing following functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inversion
## 4. get the value of the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
    ## x matrix
    ## Return the special "matrix" - cache of x
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve calculates the inverse of the special "matrix" created with makeCacheMatrix. It first checks to see
## if the inverse has been already calculated. If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the inverse of the value of the matrix in the special "matrix" and sets the value of the inverse
## of the matrix in the special "matrix" via the setsolve function. 

cacheSolve <- function(x, ...) {
    ## x matrix
    ## Return the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s    
}
