## Programming assignment 2

## This function creates a special matrix which has the ability to store its inverse
## in the cache using the << operator

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function (y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function () {x}
    
    setinverse <- function(inverseMatrix) { i <<- inverseMatrix }
    
    getinverse <- function() { i }
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function checks to see if the inverse of this matrix is already cached
## and if so returns the cached value, if not it calculates the inverse, then
## caches it and finally returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    
    i <- solve(data, ...)
    
    x$setinverse(i)
    
    i
}
