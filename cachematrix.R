## These functions calculate the inverse of the matrix and cached


## makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setsolve <- function(inv) mat <<- inv
  getsolve <- function() mat
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## `cacheSolve`: This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    mat <- x$getsolve()
    if(!is.null(mat)) {
      message("getting cached data")
      return(mat)
    }
    data <- x$get()
    mat <- solve(data, ...)
    x$setsolve(mat)
    mat
}