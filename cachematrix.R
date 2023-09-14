## Put comments here that give an overall description of what your
## functions do

## Function where it creates a matrix object and caches it so we can find its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # initial inverse matrix
  inverse <- NULL
  # set the matrix and reset inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # get the matrix
  get <- function() x
  # set the inverse of the matrix
  set_inverse <- function(inv) inverse <<- inv
  # get the inverse of the matrix
  get_inverse <- function() inverse
  
  # Return a list of the 4 functions
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Find the invsere of the cached matrix from makeCacheMatrix()

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}

