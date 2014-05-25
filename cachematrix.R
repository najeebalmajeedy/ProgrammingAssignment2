#   usage:
#   x <- matrix(rnorm(25), nrow = 5) // Create normal matrix x
#   CachedX <- makeCacheMatrix(x) // Create cached matrix
#   CachedX$get() // Return the matrix
#   cacheSolve(CachedX) to get the inverse
#   cacheSolve(CachedX) again to get the cached inverse

#The makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # cached matrix inverse variable
  m <- NULL
  
  # matrix setter
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # matrix getter
  get <- function() x
  
  # inverse setter
  setinverse <- function(inverse) m <<- inverse
  # inverse getter
  getinverse <- function() m
  
  # Return the matrix 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Compute the matrix inverse. If the inverse is calculated returns the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  # return cached invers, If it is calculated already.
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # calculate the inverse if it's not exist.
  data <- x$get()
  m <- solve(data, ...)
  
  # Cache the inverse and return it
  x$setinverse(m)
  m
}