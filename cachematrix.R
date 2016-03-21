## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinver = function(inverse) inver <<- inverse 
  getinver = function() inver
  list(set=set, get=get, setinver=setinver, getinver=getinver)
}

  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'

  inver = x$getinv()
  
  # check to see if inverse already calculated and the matrix has not changed
  if (!is.null(inver)&& is.matrix(inver)){
    message("Retrieving cached data")
    return(inver)
  }
  
  # if previous false, begins to calculate the inverse
  matterns.data = x$get()
  inver = solve(matterns.data, ...)
  
  # sets the value of the inverse in the cache via the setinver function.
  x$setinver(inver)
  
  return(inver)
}