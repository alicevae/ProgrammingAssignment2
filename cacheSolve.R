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