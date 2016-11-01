## makeCacheMatrix creates a cached element
## functions do

## makeCacheMatrix makes a list to store existing inverse calcs. Copied largely from vector example as functionality is largely the same

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Cache Solve solves the inverse of x, recalling older call if availabe. 
## Functionality again largely same as example but with different types

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## Some testing of functionality

testMat <- makeCacheMatrix(matrix(c(3,5,2,7,4,9,2,7,2), ncol = 3, nrow = 3))
testMat$get()
## Following should return NULL
testMat$getinverse()
cacheSolve(testMat)
cacheSolve(testMat)
## Following should now return inverse of testMat
testMat$getinverse()
