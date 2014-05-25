## The two functions calculate inverse of a matrix
##, and if an inverse is previously calculated, it would be returned from cached data.


## The "makeCacheMatrix" function generates a list 
## based on the matrix input.
## The list contains four elements that deal with the matrix and its inverse
## , and would later work as an input for the "cacheSolve" function 

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


## The "cacheSolve" function returns the inverse of the matrix
##, uses the list as input. If the inverse has been previously 
## calculated, the calculation step would be skipped,
## and result would be returned from cached data.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
