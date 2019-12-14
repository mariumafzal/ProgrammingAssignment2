
## Put comments here that give an overall description of what your
## functions do

## Generates a "cacheable" matrix with functions for getting and setting the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) {
                m <<- inverse      
        } 
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns the inverse of x, either by calculating it directly or from cache if the matrix has not been changed since last calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

## For testing the code
# set.seed(1)
# testmat <- matrix(runif(9), 3, 3)
# testcachemat <- makeCacheMatrix(testmat)
# cacheSolve(testcachemat)
# testcachemat$set(matrix(rnorm(9), 3, 3))
# cacheSolve(testcachemat)
