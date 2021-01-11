# makeCacheMatrix() will generate the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  
  # this function should set and get both the matrix and its inverse 
  # this is essentially the same as provided in the example, only with inverse instead of mean
  
  # inverse flag starts empty
  inv = NULL
  
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get is a function that simply returns the matrix
  get = function() x
  
  # setinverse is a function that will set the inverse of the matrix
  setinv = function(inverse) inv <<- inverse 
  
  # getinv is a function that simply returns the inverted matrix
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  # goal is to return inverse of the original matrix
  
  inv = x$getinv()
  
  # if inverse object already exists 
  if (!is.null(inv)){
    # get it 
    message("getting cached data")
    return(inv)
  }
  
  # else, calculate it
  mtr_data = x$get()
  inv = solve(mtr_data, ...)
  
  # set the value of the inverse in the cache using setinv().
  x$setinv(inv)
  
  return(inv)
}


