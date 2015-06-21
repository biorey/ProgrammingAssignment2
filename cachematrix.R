# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# - setmatrix      set the value of a matrix
# - getmatrix      get the value of a matrix
# - setinverse     set the value of the inverse matrix
# - getinverse     get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        setmatrix <- function(y) {
                x <<- y
                cache <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inv) cache <<- inv
        getinverse <- function() cache
        list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, 
             getinverse = getinverse)
}

## This funtion calculate the inverse of the special matrix created with the above
## function, reusing cached result if it is available


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$getmatrix()
        inv <- solve(data, ...)
        # return the inverse
        x$setinverse(inv)
        inv
}



