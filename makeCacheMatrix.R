makeCacheMatrix <- function(x = matrix()) {
  #x is a square invertible matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
}

#To run:
#Create a matrix x
#m = makeCacheMatrix(x)
#cacheSolve(m)