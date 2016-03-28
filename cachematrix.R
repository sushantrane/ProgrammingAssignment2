## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


# makeCacheMatrix creates a list containing a function to get-set value of the matrix and get-set value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

#cacheSolve function returns the inverse of the matrix by checking cached value initially, if not found it computes the inverse

cacheSolve <- function(x, ...) {
  
  inv = x$getinv()
  
  # Check if the inverse has already been calculated
  if (!is.null(inv)){
    
    return(inv)
  }
  
  # else calculate the inverse 
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}
