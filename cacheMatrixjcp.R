makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setmatrix <- function(y) {
    x <<- y = matrix()
    inverse <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(s) inverse <<- s
  getinverse <- function() inverse
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  mat <- x$getmatrix()
  s <- solve(mat, ...)
  x$setinverse(s)
  s
}
