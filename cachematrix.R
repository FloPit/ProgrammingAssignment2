## Functions allow it to compute the inverse of a matrix
## if the inverse for a matrix is already computed, inverse is taken from cache

## makeCacheMatrix offers (sub)functions to store and get a matrix,
## and to store and get the inverse. x "stores" matrix, inv "stores" the inverse

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


## cacheSolve returns matrix inverse of a "makeCacheMatrix"-matrix, if
## the inverse for that matrix has already been computed before, 
## returned from cache, otherwise, inverse is computed "from scratch",
## written to cache (.setinverse is called.) and inverse also returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
