## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  myMatrix <- NULL
  set <- function(y) {
    x <<- y
    myMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) myMatrix <<- inverse
  getInverse <- function() myMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

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
