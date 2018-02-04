##makeCacheMatrix: This function creates a special matrix object that can cache its inverse. It does the following:
##set the values of the matrix
##get the value of the matrix
##set the value of the inverse of the matrix
##get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
  


## ##cacheSolve:  The following function calculates the inverse of the matrix created with the above function. 
##First, however, it checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the calculation 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

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

#OUTPUT
A <- matrix(c(1,2,3,4),2,2)
print(A)
A1 <- makeCacheMatrix(A)
cacheSolve(A1)

