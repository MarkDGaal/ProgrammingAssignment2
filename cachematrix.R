## Sets or gets the values in of a matrix and is able to store values for 
## that matrix outside of the current environment in what is called the execution environment.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
                x <<- y
                m <<- NULL
        }
        get <-function() x
        setinverse <- function(solve) m <<- solve ##why would you set the inverse, thats the cacheSolve's job
        getinverse <- function() m
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}

## Computes, caches, and returns inverse of matrix. If the inverse of the matrix is already stored 
## in the execution environment it returns that stored value.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("data is cached, retriving...")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}