#The function creates a “matrix” object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse   #setting inverse function
  getinverse <- function() i
  list(set = set,                     ##set the value of the matrix
       get = get,                     #get the value of the matrix
       setinverse = setinverse,       #set the value of the inverse
       getinverse = getinverse)       #get the value of the inverse
}


## The function computes the inverse of the “matrix” returned by makeCacheMatrix above:

cacheSolve <- function(x, ...) {   
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
