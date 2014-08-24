## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setinvMatrix <- function(inv) invMatrix <<- inv
  getinvMatrix <- function() invMatrix
  list(set = set, get = get,
       setinvMatrix = setinvMatrix,
       getinvMatrix = getinvMatrix)
}
## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cachesolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # get the cached inverse
  invMatrix <- x$getinvMatrix()
  if(!is.null(invMatrix)) {
    # if the inverse if actually cached, just return it
    message("getting cached inverse")
    return(invMatrix)
  }
  # otherwise, calculate the inverse and cache it
  mtx <- x$get()
  invMatrix <- solve(mtx, ...)
  x$setinvMatrix(invMatrix)
  return(invMatrix)
}
