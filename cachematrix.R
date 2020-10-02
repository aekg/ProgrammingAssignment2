## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){ # set the value of the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x # get the value of the matrix
  setinv <- function(solve) inv <<- solve # set the value of the inverse
  getinv <- function() inv # get the value of the inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv() # get the value of the inverse matrix
  if(!is.null(m)) { # if the value of the inverse matrix is not null it recovers it from the cache
    message("getting cached data")
    return(m) # returns the inverse
  }
  data <- x$get()
  m <- solve(data, ...) # if the value of the inverse matrix is null it calculates it
  x$setinv(m)
  m # returns the inverse
}
