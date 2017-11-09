# This program takes a matrix as input and caches its inverse

# makeCacheMatrix stores the matrix as x and its inverse as inverse

# The get and set functions return and set the value of x, respectively
# getInverse and setInverse do the same for the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# cacheSolve checks to see if the inverse of x has been cached
# If so, it returns the cached inverse.
# Otherwise, it calculates the inverse, caches it with the
# setInverse function, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)){
            message("getting cached data")
            return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setInverse(inverse)
        inverse        
}
