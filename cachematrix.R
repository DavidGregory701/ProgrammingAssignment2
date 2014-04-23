## This file contains procedures for creating and manipulating a CacheMatrix object
## CacheMatrix stores a standard matrix along with it's inverse (if computed)
## and has the following fields:
##   m: the matrix
##   i: the inverse of the matrix (NULL if not yet computed)


## Create and initialize the CacheMatrix object, define set and get methods
makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(y) {
        m <<- y
        i <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Returns the inverse of x.  Uses cached answer if already computed
cacheSolve <- function(x, ...) {

    i <- x$getinverse()  ## get cached answer
    
    if (is.null(i)) {    ## if cache is empty solve assuming that m is invertable
        i <- solve(x$get(), ...)
        x$setinverse(i)  ## and cache the answer
    }
    
    ## return the solution
    i
}
