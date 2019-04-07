

## The following function, makeCacheMatrix creates a special "matrix", which is really a list containing functions to
## set the value of the matrix, get the value of the matrix, set the value of the inverse and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) invm <<- solve
  getsolve <- function() invm
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}

## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        
  invm <- x$getsolve()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setsolve(invm)
  invm
  
}
