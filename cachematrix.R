## Programming assignment 2

## This function creates a special matrix which has the ability to store its inverse
## in the cache using the << operator

makeCacheMatrix <- function(x = matrix()) {
    
    ## use i to store the cached inverse
    i <- NULL
    
    ## function to set the value of makeCacheMatrix
    set <- function (y) {
        x <<- y
        i <<- NULL
    }
    
    ## function to get the value of makeCacheMatrix
    get <- function () {x}
    
    ## function to set the inverse of makeCacheMatrix in the cache
    setinverse <- function(inverseMatrix) { i <<- inverseMatrix }
    
    ## function to retrieve the inverse of makeCacheMatrix from the cache
    getinverse <- function() { i }
    
    ## return a list of the makeCacheMatrix functions to access in the cache
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function checks to see if the inverse of this matrix is already cached
## and if so returns the cached value, if not it calculates the inverse, then
## caches it and finally returns it

cacheSolve <- function(x, ...) {
    ## attempt to pull the inverse from the cache
    i <- x$getinverse()
    
    ## check to see if the inverse has been successfully retrieved from the cache
    ## and if so, return the cached inverse
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## calculate the inverse of the matrix
    data <- x$get()
    
    i <- solve(data, ...)
    
    ## cache the calculated inverse of 'x'
    x$setinverse(i)
    
    ## Return the calculated inverse of 'x'
    i
}
