# The below functions work together to invert a matrix and cache the results,
# so the next time the functions are invoked the inverse of the matrix will be retrieved 
# from the cache, rather than recalculated.
# The main feature of these functions is caching. To be able to verify this feature 
# see the comments at the end of the source file.


## The function makeCacheMatrix returns a matrix object with a "method" to invert it.
# This function assumes that the matrix passed to it as an argument can be inverted, 
# there is no validation on the matrix whatsoever.
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


## The cacheSolve function checks if a matrix (passed as an argument) has already been 
# inverted. If an inverse matrix is already available the function will return the 
# cached matrix. If the matrix has not been inverted  or it has changed since then, 
# it will perform the actual inversion.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinvert()
  if(!is.null(inv)) {
    return(m) # Change this variable to a non-existing one to test the caching feature
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinvert(inv)
  inv
}


# Instructions to test the caching functionality of the above functions
# 1. Create an invertible matrix:
#   origMatrix <- matrix(c(2,3,2,2), nrow=2, ncol=2)
# 2. Change the name of the variable retrieved by the cacheSolve function
#   as indicated in line 34
# 3. Save and source() the cachematrix.R file
# 4. Call the functions:
#   myMatrix <- makeCacheMatrix(origMatrix)
#   cacheSolve(myMatrix)
# 6. Note the values returned by the function
# 7. Call the cacheSolve() fuction again with the same argument:
#   cacheSolve(myMatrix)
# 8. Change the name of the variable retrieved by the cacheSolve function back to 'inv'
# 9. Repeat steps 1 through 8



