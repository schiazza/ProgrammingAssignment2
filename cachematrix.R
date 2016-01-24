## Two functions aiming to store in the cache the inverse of a matrix

## Will take 'x' as input (assumed squared and invertible matrix).
## Includes a set of four functions. Set/Get to initialize/get its
## value. Setinverse/getinverse to intialize/get its inverse from cahce.

makeCacheMatrix <- function(x = matrix()) {
  minverse <- NULL
  set <- function(y) {
    x <<- y
    minverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_matrix) minverse <<- inverse_matrix
  getinverse <- function() minverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'ç
  minverse <- x$getinverse()
  if(!is.null(minverse)) {
    message("getting cached data")
    return(minverse)
  }
  data <- x$get()
  minverse <- solve(data, ...)
  x$setinverse(minverse)
  minverse
}
