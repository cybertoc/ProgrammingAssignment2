## cachematrix.R
## -------------
## Caching the inverse of a matrix
## The following two functions will allow caching of matrix inverse

## makeCacheMatrix: creates a special "matrix" object that caches its inverse
## arguments: x - input matrix
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    
    ## Return a list of elements to be used by the cacheSolve method
    list(set = set, get = get, 
         setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve: computes the inverse of the special matrix returned by makeCacheMatrix
## if the inverse has already been computes, returns it from the cache
cacheSolve <- function(x, ...) {
    
    ## Check if the inverse of x has already been computed
    ## if yes, return it
    s <- x$getsolve()
    if (!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    ## otherwise, compute the inverse of x using the solve method
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
