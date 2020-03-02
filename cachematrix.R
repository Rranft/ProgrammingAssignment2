## These are two functions that creates an object that stores 
## a matrix and cache's its inverse.

## First function: Creates a matrix and is able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)

}


## Second function: Calculates the inverse of the matrix returned
## by the function above.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv))
                return(inv)
        
        inv_data <- x$get()
        inv <- solve(inv_data, ...)
        x$setinverse(inv)
        inv
}
