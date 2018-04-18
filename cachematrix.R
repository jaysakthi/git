## Two functions are included to illustrate Caching in R
## makeCacheMatrix - intializes, stores and retrieves inverse of matrix 
## CacheSolve - Gets the inverse of a matrix from Cache or calculates it

## makeCacheMatrix function takes a matrix as an argument

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## CacheSolve function returns the inverse of a matrix. It takes a matrix as an argument and checks to see if the inverse
## exists in Cache before calculating it
## call makeCacheMatrix before calling cacheSolve

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
