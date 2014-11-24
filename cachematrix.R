## makeCacheMatrix - builds special matrix object, containing functions to get and set
##    the original, non-inverted matrix, and functions to get and set the inverse matrix
## cacheSolve - if inverse matrix is already solved and cached, function returns the cached
##    value; otherwise, function computes, caches, and returns the inverse matrix

## builds special matrix object from input x to allow caching
makeCacheMatrix <- function(x = matrix()) {
    n <- NULL
    set <- function(y){
      x <<- y
      n <<- NULL
    }
    get <- function() { x }
    set_inverse <- function(inv){
      n <<- inv
    }
    get_inverse <- function() { n }
    
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## returns the inverse of input matrix x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  n <- x$get_inverse()
  if(!is.null(n)){
    return(n)
  }
  
  data <- x$get()
  n <- solve(data,...)
  x$set_inverse(n)
  n
}
