## Programming Assignment of R week 2
## Caching the Inverse of a Matrix

## Create a matrix of x

makeCacheMatrix <- function(x = matrix()) {
  #set the matrix
  myMatrix <- NULL
  set <- function(y) {
    x <<- y
    myMatrix <<- NULL
  }
  get <- function() x
  
  # set and get the inverse of the matrix
  setInverse <- function(inverse) myMatrix <<- inverse
  getInverse <- function() myMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


##  retrieve the inverse of x from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invertedMatrix <- x$getInverse()
  if(!is.null(invertedMatrix)) {
    message("getting cached data")
    return(invertedMatrix)
  }
  data <- x$get()
    invertedMatrix <- solve(data, ...)
  x$setInverse(invertedMatrix)
  invertedMatrix
}
