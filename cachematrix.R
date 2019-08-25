
# makeCacheMatrix creates a list containing a function to
# 1-set the value of the matrix
# 2-get the value of the matrix
# 3-set the value of inverse of matrix
# 4-get the value of inverse of matrix
makeCacheMatrix <- function(y = matrix()) {
  mat <- NULL
  setmat <- function(x) {
    y <<- x
    mat <<- NULL
  }
  get <- function() y
  setinv <- function(inverse) mat <<- inverse
  getinv <- function() mat
  list(setmat=setmat, get=get, setinv=setinv, getinv=getinv)
}


# The cacheSolve fn returns the inverse of the matrix.

# we assume that matrix is always invertible.
cacheSolve <- function(y, ...) {
  mat <- y$getinv()
  if(!is.null(mat)) {
    message("Getting cached data")
    return(mat)
  }
  data <- y$get()
  mat <- solve(data)
  y$setinv(mat)
  mat
}