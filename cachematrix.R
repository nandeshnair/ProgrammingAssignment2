## There are two functions in this file 
## 1. makeCacheMatrix
## 2. cacheSolve
## These functions help in computing the inverse of a matrix.
## The inverted matrix is stored in the cache and subsequent requests for the
## inverse for the same initial matrix are responded with the cached value.


## The function makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverted matrix
## 4. get the value of the inverted matrix

makeCacheMatrix <- function(originalMatrix = matrix()) {
  matrixInverse <- NULL
  set <- function(inputMatrix) {
    originalMatrix <<- inputMatrix
    matrixInverse <<- NULL
  }
  get <- function() originalMatrix
  setInverse <- function(inputInverse) matrixInverse <<- inputInverse
  getInverse <- function() matrixInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The function cacheSolve calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been computed. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverted matrix (inputInverse parameter) 
## in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  originalMatrix <- x$get()
  i <- solve(originalMatrix,...)
  x$setInverse(i)
  i
}
