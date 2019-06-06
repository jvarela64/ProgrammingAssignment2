## These two functions will create an object with a Matrix and functions 
## which can calculate the Inverse and cache its result 

## makeCacheMatrix will take a matrix and create an object to store it, 
## adds functions to the object to be able to get, set, and calculate both 
## the original matrix and its Inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseOfX) m <<- inverseOfX
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve takes the object created by makeCacheMatrix and outputs
## its Inverse, caching it inside the object

cacheSolve <- function(cacheMatrix, ...) {
        ## Return a matrix that is the inverse of 'cacheMatrix'
    m <- cacheMatrix$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    matrixData <- cacheMatrix$get()
    m <- solve(matrixData)
    cacheMatrix$setInverse(m)
    m
}
