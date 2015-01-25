## In ths session we defined two functions makeCacheMatrix() and cacheSolve()
## to calculate the inverse of a matrix and store it in the cache.

## makeCacheMatrix() get in and store a matrix which can be inversed and 
## calculate the inverse of this matrix and store it in cache. The output is in
## list format.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve(x)
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve() get in the list formatted output from makeCacheMatrix() and
## read out the cached inverse of the matrix. If there is the inverse has not
## been calculated, it will calculate the inverse and set it in the cache, 
## otherwise the cached result will be read out without computation.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}
