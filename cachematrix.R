## Caches value of the inverse of a given matrix

## contains functions to set and get matrix, set and get value of inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function()x
    setinverse <- function(inverse)m <<- inverse
    getinverse <- function()m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Checks to see if the inverse matrix is cached and calculates it if it hasn't been

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
            message("Getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data,...)%*% data
        x$setinverse(m)
        m
}
