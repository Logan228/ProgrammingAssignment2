## Bellow two functions was to save computing time for your algorithm.
## Especailly, you just want to use the computing result, but don't want
## to computing again, it will very usefull.


## First, this function is to set & get the pair of data and inverse matrix,
## if your data was resetting, 'cacheSolve' function will recomputing. 
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


## Second, this function is to decide the data will recomputing.
## If data is same, print 'getting cached data' message,
## if not that computing the new data and return the result.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m   ## Return a matrix that is the inverse of 'x'
}
