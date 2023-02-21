## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL           ## This will create a NULL matrix and hold inverse values
  set <- function(y) {  ## This will define the set function, and assigns a new variable
    x <<- y             ## This will define a new object in parent environment
    inv <<- NULL        ## This will reset value to NULL
  }
  get <- function() x   ## Defines the get function
  setinverse <- function(inverse) inv <<- inverse   ## this will assign values of inv 
  getinverse <- function() inv                      ## gets inv values
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

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
