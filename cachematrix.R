
#R Assignment - Coursera
#Computing the inverse of a matrix is a computationally intensive task. Therefore, we build a function that creates a "matrix"
#object and calculate the inverse using caching.
  
# makeCacheMatrix function uses a matrix object for caching the inverse
  
  makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }


# cacheSolve function extracts the inverse matrix from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Retrive data from the cache")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
