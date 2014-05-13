## Put comments here that give an overall description of what your
## makeCacheMatrix create a special matrix that keeps its cache
## cacheSolve knows to check cache first, otherwise do the expensive
## computation

## makeCacheMatrix has the following functions
## get: returns matrix
## set: sets the matrix
## setinv: sets the inverse
## getinv: returns the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve has the following logic
## It first checks if the matrix has a precomputed cache
## if so, this is returned. If not, the "expensive" 
## solve() function is called. The inversed matrix
## is cached and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
