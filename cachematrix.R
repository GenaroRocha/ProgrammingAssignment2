## With this functions we'll be able to cache
## potentially time-consuming computations

## With our first function, we create a special
## matrix, which is really a list containing a 
## function to set the value of the matrix, get
## the value of the matrix, set the value of the
## inverse matrix, and get the value of the
## inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## With our second function, we calculate the 
## inverse of the matrix created above, however,
## it first checks to see if the inverse has
## already been calculated; if so, it gets the 
## mean from the cache and skips the computation
## Otherwise, it calculates the inverse matrix
## and sets its value in the cache via the
## solve function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
