## These functions implement a caching mechanism for matrix inversion.
## makeCacheMatrix creates a special matrix object that can store a matrix
## and cache its inverse. cacheSolve computes the inverse of the matrix,
## retrieves it from the cache if available, or computes and stores it if not.

## This function computes the inverse of a matrix and caches the result.
## If the inverse has already been calculated, it retrieves it from the cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) {
    inv <<- inverse
  }
  
  getinv <- function() inv
  
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  
  inv
}
