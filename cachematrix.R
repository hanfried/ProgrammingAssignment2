## Programming Assignment 2 in Course R-Programming https://class.coursera.org/rprog-035
## writing a caching version of inverting a matrix
##
## Usage:
##   # without this script:
##   n <- 2500                              # size of large matrix
##   x <- matrix(rnorm(n^2),nrow=n,ncol=n)  # already takes a longer time
##   y <- solve(x)                          # computation takes long
##   z <- solve(x)                          # again takes long time
##
##   # with the cache script:
##   source("cachematrix.R")
##   n <- 2500
##   x <- makeCacheMatrix(matrix(rnorm(n^2),nrow=n,ncol=n))
##   y <- cacheSolve(x)                    # computation again takes long
##   z <- cacheSolve(x)                    # now quick due to caching (incl. a message of caching)


## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##  set: set the value of the matrix
##  get: get the value of the matrix
##  setinverse: set the value of the inverse of the matrix
##  getinverse: get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of the special "matrix" created with the makeCacheMatrix function. 
##   It first checks to see if the inverse of the matrix has already been calculated. 
##   If so, it gets the inverse from the cache and skips the computation. 
##   Otherwise, it calculates the inverse of the data and 
##   sets the value of he matrix in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setinverse(m)
  m
}
