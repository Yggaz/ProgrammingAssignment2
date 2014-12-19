## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse (inv)
makeCacheMatrix <- function(x = matrix()) {
  ## initialize
  inv <- NULL
  setmatrix <- function(y) {
    x <<- y
    ## setting new matrix - MUST clear the saved inverse!
    inv <<- NULL
  }
  ## getting a matrix
  getmatrix <- function() {
    x
  }
  ## writing a matrix inverse into cash
  writeinverse <- function(cached) {
    inv <<- cached
  }
  ## reading a matrix inverse from cashe, if possible
  cachedinverse <- function() {
    inv
  }
  list(set = setmatrix, get = getmatrix,
       savetocache = writeinverse,
       readfromcache = cachedinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the cashed inverse is valid (it's been calculated and the matrix itself has not changed)
## then the cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  inverse <- x$readfromcache()
  if(!is.null(inverse)) {
    message("cached value found!")
    ## we have a valid inverse - return it!
    return(inverse)
  }
  ## not so lucky - we have to solve matrix...
  data <- x$get()
  inverse <- solve(data)
  ## ...and to cache solved matrix
  x$savetocache(inverse)
  inverse
}
