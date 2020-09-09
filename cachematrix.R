## create a special object that stores a matrix and cache's its inverse.

## creates a special "matrix", which is a list containing a function to
##   - set(): set the value of the matrix
##   - get(): get the value of the matrix
##   - setinv(): set the value of the inverse
##   - getinv(): get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## calculates the inverse of the special "matrix" created with the above function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
