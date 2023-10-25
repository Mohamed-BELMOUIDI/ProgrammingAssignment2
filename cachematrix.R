## Creates a special matrix object that can cache its inverse.
## Initializes the object with the provided matrix 'x' (default is an empty matrix).
makeCacheMatrix <- function(x = matrix()) {
  data <- x
  cache <- NULL
  
  ## Sets the matrix data to a new value.
  set <- function(newmat) {
    data <<- newmat
    cache <<- NULL  # Clear the cache when the matrix changes
  }
  
  ## Retrieves the current matrix data.
  get <- function() data
  
  ## Calculates and caches the inverse of the matrix.
  setInverse <- function(inverse) cache <<- inverse
  
  ## Retrieves the cached inverse, or computes it if needed.
  getInverse <- function() {
    if (!is.null(cache)) {
      message("Getting cached inverse")
      return(cache)
    }
    
    message("Calculating and caching the inverse")
    inv <- solve(data)
    cache <<- inv
    inv
  }
  
  ## Returns a list of functions for matrix operations.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Computes and caches the inverse of the matrix object created with 'makeCacheMatrix'.
## If the inverse has already been cached (and the matrix has not changed), retrieves it from the cache.
## If the matrix has changed since the last computation, it recalculates the inverse and updates the cache.
## Returns the inverse matrix.
cacheSolve <- function(cachedMatrix, ...) {
  mat <- cachedMatrix$get()
  
  ## Check if the inverse is already cached.
  inv <- cachedMatrix$getInverse()
  
  return(inv)
}
