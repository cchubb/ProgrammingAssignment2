## Contains a set of functions that will calculate the inverse of a matrix and cache the results, for reuse
##Usage:
##  mm <- matrix(c(2,3,2,2), 2, 2, byrow = TRUE)
##  xx <- makeCacheMatrix(mm)
##  cacheSolve(xx)

# makeCacheMatrix() Returns a list of access methods for managing the matrix x and it's inverse s
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s  
}
