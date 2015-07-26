## Coursera R Programming Assignment 2
## 

## Use lexical scoping and <<- operator to create a set of functions
## that convert a matrix to a special matrix which caches the iverse 
## after it is solved the first time so that performance is improved if 
## the inverse is required multiple times


## makeCacheMatrix accepts a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        message("in makeCacheMatrix$set")
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(m) inv <<- m
    getInverse <- function() inv
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
    
}


## cacheSolve returns the invers of a matrix created with makeCacheMatrix
## first time it is called with the matrix, it caculates it and saves it before 
## returning the result.  Subsequent calls use the saved results
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (!is.null(i)){
        message("using saved inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
    
}
