# makeCacheMatrix is a function that creates a matrix object that can cache its inverse. This is my submission to the Coursera R Programming assignment for week 2. Some of the text and code is taken from the assignment page for this course.
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

#cacheSolve computes the inverse of the matrix returned by makeCacheMatrix. If the inverse has already been calculated and the matrix has not changed, then the cachesolve should retrieve the inverse from the cache.
cacheSolve <-function(x, ...) {
    #x is from makeCacheMatrix
    inv <- x$getinverse()
    #if cached, get cached inverse
    if(!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
    }
    #if not cached, calculate inverse of matrix x:
    data = x$get()
    inv = solve(data)
    x$setinverse(inv)
    #want to get inverse of original matrix x:
    return(inv)
}

#To run:
#cacheSolve(m)