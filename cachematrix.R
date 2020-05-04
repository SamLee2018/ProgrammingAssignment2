### Assignment: Caching the Inverse of a Matrixless 
#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
#rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here).
#Your assignment is to write a pair of functions that cache the inverse of a matrix.

## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setsolve <- function(solve) {
    
    m <<- solve
  }
  
  getsolve <- function() m
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.


cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  
  if(!is.null(m)) {
    
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)

  ## Return a matrix that is the inverse of 'x'
    m
}
