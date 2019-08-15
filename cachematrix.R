## The function holding the other sub functions
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y){
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(newi) i <<- newi
      getinverse <- function() i
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Checking if there's something cached up, otherwise solving the matrix
cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if (!is.null(i)){
        message("getting cached inversed matrix")
        return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}