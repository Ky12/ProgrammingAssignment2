## R function is able to cache potentially time-consuming computations.
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y){
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) invMatrix <<- inverse
  getInverse <- function() invMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)){
    message("getting cached data")
    return(invMatrix)
  }
  matrixData <- x$getMatrix()
  invMatrix <- inverse(matrixData,...)
  x$setInverse(invMatrix)
  invMatrix
}
