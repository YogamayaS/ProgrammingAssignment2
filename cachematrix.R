## The two makeCacheMatrix and cacheSolve will be used to create
## and cache the inverse of a matrix. The functions will use the 
## scoping rules of R and the 

## makCacheMatrix will create a special object "matrix" that can
## cache the inverse of the matrix. The function will get and set the matrix.
## It will also get and set the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
  }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## The function cacheSolve will first check if the inverse of the
## matrix exists in the cahce. If it exists then it will return the
## value from the cache otherwise it will calculate the inverse of the matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
