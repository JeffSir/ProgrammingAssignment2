# The functions in this R script work together to calculate
# the inverse of a matrix, and use a cached value if it exists.
# The original matrix must first be initialized and passed
# as an argument to makeCacheMatrix().  Afterwards, the list
# of functions returned by makeCacheMatrix() should be passed
# as an argment to cacheSolve() to retrieve the inverse function.

# makeCacheMatrix
# 
# Args:
#   'x' matrix used to calculate inverse - must be square matrix
# Returns:
#   A list object containing function pointers to access/set
#   the matrix and its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


# cacheSolve
# 
# Args:
#   'x' list object returned by the makeCacheMatrix function
# Returns:
#   The cached inverse matrix (if it already exists).  Otherwise
#   returns the inverse matrix calculated by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}