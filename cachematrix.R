## Functions:
##
## makeCacheMatrix - creates the special object, which represents the "matrix" and returns the list, containing set of functions: 
## 1) get - get the value of the matrix
## 2) set - set the value of the matrix
## 3) getinv - get the value of the inverse matrix
## 4) setinv - set the value of the inverse matrix
##
## cacheSolve - returns the inverse matrix, using cache if it is available
##
## Example:
## m = makeCacheMatrix(matrix(sample(16),4,4))
## i = cacheSolve(m)
##

## Creates the special object which represents the "matrix" 
## and can contain a cache of the inverse matrix if it has been calculated
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setinv <- function(inv) cache <<- inv
  getinv <- function() cache
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}


## Returns the inverse matrix from the special object created by the "makeCacheMatrix" function before
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cache <- x$getinv()
  if(!is.null(cache)) {
    return(cache)
  }
  m <- x$get()
  if(det(m) != 0) {
    rs <- solve(m, ...)
  } else {
    stop("You couldn't get inverse matrix: det(x) == 0")
  }
  x$setinv(rs)
  rs
}
