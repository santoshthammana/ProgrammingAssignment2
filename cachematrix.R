# Matrix inversion can be time intensive for a large matrix.  The functions below allow the results of matrix inversion to be cached for quick retrieval.

## makeCacheMatrix takes in originalMatrix as input and returns a "cache matrix", which is a list of functions allowing for a matrix inverse to be cached.

makeCacheMatrix <- function(originalMatrix = matrix()) {

  # set inverseMatrix to NULL, because we haven't calculated the inverse yet
  inverseMatrix <- NULL

  # setMatrix sets the matrix to be inverted
  setMatrix <- function(mat) {
    originalMatrix <<- mat
    # inverseMatrix is set to NULL because the inverse of this new matrix needs to be computed.
    # This will remove results from any inverse calculations of a previous matrix.
    inverseMatrix <<- NULL
  }

  # getMatrix returns the matrix to be inverted
  getMatrix <- function() originalMatrix

  # setInverse sets the matrix inverse
  setInverse <- function(inv) inverseMatrix <<- inv

  # getInverse returns the inverse set by setInverse
  getInverse <- function() inverseMatrix

  # return the cache matrix, which is a list of the above functions.
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve returns the inverse of the matrix stored in cacheMatrix.  If cached, the inverse is simply retrieved.  Otherwise, the inverse is calculated and cached.
## Additional parameters to the solve function can be passed here.

cacheSolve <- function(cacheMatrix, ...) {

  ## Get the inverse currently stored in cacheMatrix.
  inv <- cacheMatrix$getInverse()

  ## If the inverse isn't NULL, then it's already been computed and can returned here.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  ## The inverse needs to be computed.  First retrieve the original matrix.
  data <- cacheMatrix$getMatrix()

  ## Compute the matrix inverse.
  inv <- solve(data, ...)

  ## Cache the matrix inverse in cacheMatrix.  If the original matrix remains the same, then the next time this function is called, the inverse will be retrieved from the cache.
  cacheMatrix$setInverse(inv)
  inv
}
