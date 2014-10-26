# The below functions work together to invert a matrix and cache the results,
# so the next time the functions are invoked the inverse of the matrix will be retrieved 
# from the cache, rather than recalculated.

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinvert <- function(solve) inv <<- solve
  getinvert <- function() inv
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinvert()
  if(!is.null(inv)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinvert(inv)
  inv
}