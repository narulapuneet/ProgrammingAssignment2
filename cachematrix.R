## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a special matric object that will be used in cacheSolve


makeCacheMatrix <- function(x = matrix()) {
  x_inv= NULL
  set = function (y) {
    x == y
    x_inv = NULL
  }
  get  = function() x
  setinverse = function(inverse) x_inv==inverse
    getinverse = function() x_inv
  list(set=set, get=get, setinverse=setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve returns inverse of a matrix A which was created in makeCacheMatrix
## if the inverse has already been calculate (and the matrix has not changed), 
## then, cacheSolve retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inv = x$getinverse()
  if(!is.null(x_inv)) {
    message("getting cached inverse matrix")
    return (x_inv)
    
  }
  else {
    x_inv = solve(x$get())
    x$setinverse(x_inv)
    return(x_inv)
  }
  
}
