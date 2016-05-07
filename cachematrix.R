## Caching Inverse of a Matrix
## Matrix inversion is costly computation and caching might help
## The two matrix below create a special object that stores the matrix and cache## the inverse

## This function creates a special matrix object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() X
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The below function cumputes the inverse of the special matrix created by     ## makeCacheMatrix. If the inverse exists, then it should retrive the inverse   ## from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if (!is.null(inv)) {
	   message("getting cached data")
	   return(inv)
	 }
	 mat <- x$get()
	 inv <- solve(mat, ...)
	 x$setInverse(inv)
	 inv
}
