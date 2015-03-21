## @x: a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

## used `<<-` to assign a value to an object in an environment that different from the current environment. 
## @x: output of makeCacheMatrix(): returns inverse of the original matrix input to makeCacheMatrix()



makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## if the inverse has already been calculated =>if (!is.null(inv)){, get it from the cache and skips the computation, 
## if not calculates the inverse => data <- x$get() i <- solve(data, ...)
## sets the value of the inverse in the cache via the setinverse function. => x$setinverse(i)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
