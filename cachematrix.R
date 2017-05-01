# Caching Inverse of a Matrix
# Example usage
# m <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
# m2 <- makeCacheMatrix(m)
# cacheSolve(m2)
# [,1] [,2]
# [1,]  0.0    1
# [2,]  0.5    0
# cacheSolve(m2)
# inverse is cached
# [,1] [,2]
# [1,]  0.0    1
# [2,]  0.5    0

# Creates a matrix that can cache it's inverse
# Returns:
#   A matrix with functions to get/set value & get/set inverse

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

# Computes the inverse of a matrix. If the inverse has already been
# calculated before, the cached inverse is returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){ 
    message("inverse is cached")
        return(m)
  }
  y <- x$get()
  m <- solve(y, ...)
  x$setinv(m)
  return(m)
    
}

