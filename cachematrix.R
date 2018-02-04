## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(1:4, 2)) {{
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) 
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
funs <- makeCacheMatrix()
funs$get()
funs$setInverse()
funs$getInverse()

}


## Write a short comment describing this function

cacheSolve <- function(funs, ...) {
  inv <- funs$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- funs$get()
  inv <- solve(mat)
  funs$setInverse(inv)
  inv
}
cacheSolve(funs)
        

