# The functions help to create a matrix object and corresponding
# getter and setter functions that can cache its inverse.
# That means the inverse of the matrix only has to be computed once.


#' Function to create matrix object which caches its inverse
#' 
#' @param x a numeric matrix. Default to 1x1 matrix with NA
#' 
#' @return a list with the functions set(y), get(), setinverse(inverse) and getinverse()
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


#' Function to calculate the inverse for a matrix with cached inverse.
#' 
#' If the inverse is calculated the first time the R standard function \link{\code{solve}} is used and the
#' result is cached in the matrix object created by \link{\code{makeCacheMatrix}}. If the function is called 
#' on again on the same object the inverse is not calculated but the cached inverse matrix is returned.
#' 
#' @param x matrix object create with \link{\code{makeCacheMatrix}}
#' @param ... further arguments to \link{\code{solve}}
#' 
#' @return the inverse of x
#' 
#' @example 
#' testmat <- makeCacheMatrix(diag(10))
#' cacheSolve(testmat)
#' cacheSolve(testmat)
cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if (!is.null(inv))  {
    message("Getting cached inverse")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)

  inv  
  
}
