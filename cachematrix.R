## Input x as a matrix
## and then set "s" value as null
## then I changed reference from "mean" to "solve"

makeCacheMatrix <- function(x = matrix(sample(1:50,9),3,3)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
## Similarly, changed "mean" to "solve".
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("Inversed Matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
## Our aim is to write a pair of functions, namely, "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## makeCacheMatrix is a function which creates a special "matrix" object that can cache its inverse for the input.
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## CacheSolve is a function which computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
m <- matrix(rnorm(16),4,4)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)
## Caching the Inverse of a Matrix: Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that stores a matrix and caches its inverse. This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## This function computes the inverse of the special "matrix" created by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
# Testing
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))

my_matrix$get()

my_matrix$getInverse()

cacheSolve(my_matrix)

my_matrix$getInverse()

my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))

my_matrix$get()

my_matrix$getInverse()

cacheSolve(my_matrix)

my_matrix$getInverse()

