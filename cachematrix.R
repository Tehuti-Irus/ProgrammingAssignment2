## makecachematrix function would return the special matrix object that can cache 
## its inverse. Generally solve() funciton is used to return the inverse of the matrix
## and cachesolve() is superior as it finds it from cache [if available] saving  
## computational time. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  cmat <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL}
  
  ## getinverse() returns the inverse from cache
  getinverse <- function() cmat
  
  ## setinverse() returns the inverse of the matrix object parameter
  setinverse <- function(m)
  {
   cmat <- m
  }
 
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Before computing the inverse of the matrix, find if it is in the cache
  ## using the function, getinverse() to retrieve it 
  imat <- x$getinverse()
  ## return the matrix object from cache without going in to computation
  if (!is.null(imat)) {message("getting matrix inverse from cache")
    return(imat)
  }
  ## compute as matrix inverse is not available in cache
  mat <- x$get()
  ## computing the inverse using the solve() function
  imat <- solve(mat, ...)
  ## setting the value in cache for future use
  x$setinverse(imat)
  ## return the inverse matrix object
  imat
  
  
}
