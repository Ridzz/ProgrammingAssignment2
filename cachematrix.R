## Following R functions are able to cache matrix inversions 
## Matrix inversions can be costly computations and it helps to reduce the cost 
## by caching the matrix inverse than computing it repeatedly. 

## This function creates a special "matrix" object that can cache its inverse
## It is assumed that the matrix supplied is always invertible
## Returns a list containing 4 functions to set : 
## matrix value, get matrix value, set matrix inverse, and get matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- matrix()
        setmatrix <- function(y) {
                x <<- y
                m <<- matrix()
        }
        getmatrix <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.na(m[1,1])) {
                message("getting cached data")
                return(m)
        }
        matrixdata <- x$getmatrix()
        m <- solve(matrixdata)
        x$setinverse(m)
        m
}
