## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- solve(inv)
  getinv <- function() m
  list(set = set, get = get,setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){ 
        return(m)
  }
  y <- x$getmatrix()
  x$setmatrix(y)
  m <- solve(y, ...)
  x$setinverse(m)
  m
    
}

